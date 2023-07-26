library(data.table)
library(R.utils)
library(tidyr)
library(dplyr)
library(zoo)
library(tidyverse)
library(geobr)
library(sf)
library(readxl)


# 7/25/23
# 
# 
# Preliminary deaths averted analysis
# 
# Missing: vaccine waning - need to think of how to implement. We know number 
# of doses at each time period BUT don't know timing of when someone got 
# subsequent dose.
# 

# Import data: population, vaccination, deaths
Pop_by_age <- read.csv("Pop_by_Age.csv")
Pop_by_muni <- read.csv("code_cities.csv")

# Saved file was too large to upload to github easily, so run code here. Can also write file on local machine and run from
# there.
source("Brazil_Vaccine_Cleaning.R")
Vaccinations <- vaccination_data5 #read.csv("Vaccine_data_clean_72023.csv")
Deaths <- read.csv("Deaths_clean_72023.csv")

# Rename municipality variable for merging
Pop_by_muni <- rename(Pop_by_muni, muni_pac=code)

# merge all data together
# 

Full_Data <- left_join(Deaths, Pop_by_muni, by="muni_pac")
Full_Data <- left_join(Full_Data, Pop_by_age, by="agegroup")

# Calculate population by age

Full_Data$Pop_Age <- Full_Data$population * Full_Data$Proportion

# Keep only relevant variables to make dataset easier to work with
# 
Full_Data <- Full_Data[,c(2:6,9:10,13)]

# Keep only necessary vaccinations data

Vaccinations2 <- Vaccinations[,c(3:5,46:50, 72:91)]

# Merge in vaccination data
# 

Full_Data2 <- left_join(Full_Data, Vaccinations2, by=c("muni_pac","agegroup","month"))


# Add vaccine effectiveness data
# PRELIMINARY: same VE by vaccine product. Only difference is by produce
# and number of doses.
# From Kahn paper on VE COVID LAC

CV1 <- 0.31
CV2 <- 0.58

PF1 <- 0.71
PF2 <- 0.93

JS1 <- 0.49
JS2 <- 0.69

AZ1 <- 0.42
AZ2 <- 0.85

VE3 <- 0.93


# For preliminary analysis ONLY: combine all 3+ doses for all products together,
# use pfizer efficacy
# 
Full_Data2$Booster <- Full_Data2$PF_5dose + Full_Data2$JS_5dose + Full_Data2$AZ_5dose + Full_Data2$CV_5dose +
  Full_Data2$PF_4dose + Full_Data2$JS_4dose + Full_Data2$AZ_4dose + Full_Data2$CV_4dose +
  Full_Data2$PF_3dose + Full_Data2$JS_3dose + Full_Data2$AZ_3dose + Full_Data2$CV_3dose 
  

# Change all NA (no vaccination data) to 0
Full_Data2[is.na(Full_Data2)] <- 0


# Calculated protected pop: number of people vaccinated with each dose
# multiplied by vaccine effectivess.
# Currently simplified: same for all ages and all time periods,
# And boosters all the same. Plus no waning. 

Full_Data2 <- Full_Data2 %>%
  mutate(ProtectedPop = AZ_1dose*AZ1 + CV_1dose*CV1 + PF_1dose*PF1 + JS_1dose*JS1+
           AZ_2dose*AZ2 + CV_2dose*CV2 + PF_2dose*PF2 + JS_2dose*JS2+
           Booster * VE3) %>%
  mutate(ProtectedPopProp = ProtectedPop/Pop_Age) 

# Need to evaluate what this looks like but for now-
# more people protected than exist (counts off?)

Full_Data2$ProtectedPopProp <- ifelse(Full_Data2$ProtectedPopProp>1, 0.95,
                                      Full_Data2$ProtectedPopProp)
  
# Calculate counterfactual deaths: number of observed deaths divided by 1- protected pop
# 
Full_Data2 <- Full_Data2 %>%
  mutate(Deaths_Counterfactual = death/(1-ProtectedPopProp)) %>%
  
# Calculate deaths averted: counterfactual deaths - observed deaths
# 
  mutate(Deaths_Averted = Deaths_Counterfactual - death)



sum(Full_Data2$death)
sum(Full_Data2$Deaths_Counterfactual)  
sum(Full_Data2$Deaths_Averted)  
  
# Make a map to describe deaths averted. 
# 1. Calculate cumulative deaths averted
# 2. Collapse across age groups
# 3. Keep only last month
# 4. Visualize on map
# 

# Collapse data across age groups to get a single value
# 

Full_Data3 <- Full_Data2 %>% group_by(muni_pac, month) %>% 
  summarize(cumDeathsAverted = sum(Deaths_Averted), cumDeaths = sum(death),
            cumDeathsCounterfactual = sum(Deaths_Counterfactual)) 

# Collapse data across months
Full_Data4 <-  Full_Data3 %>%
  group_by(muni_pac) %>%
  arrange(month) %>%
  mutate(cumSumDeathsAverted = cumsum(cumDeathsAverted),
         cumSumDeathsCounterfactual = cumsum(cumDeathsCounterfactual),
         cumSumDeaths = cumsum(cumDeaths))

max(Full_Data4$month)

# Keep only cumulative data from April 2023

Full_Data_Final <- Full_Data4[which(Full_Data4$month=="2023-04-01"),]


# Map!!
# 
muni <- read_municipality(
  year=2020, 
  showProgress = FALSE
)

muni$muni_pac <- substr(as.character(muni$code_muni),1,
                        nchar(muni$code_muni) -1)

Full_Data_Final$muni_pac <- as.character(Full_Data_Final$muni_pac)
Pop_by_muni$muni_pac <- as.character(Pop_by_muni$muni_pac)

Full_Data_Final <- left_join(Full_Data_Final, Pop_by_muni, by="muni_pac")

Full_Data_Final$Perc_Deaths_Averted <- Full_Data_Final$cumSumDeathsAverted/Full_Data_Final$cumSumDeathsCounterfactual*100
Full_Data_Final$DeathRateObserved <- Full_Data_Final$cumSumDeaths / Full_Data_Final$population * 100000
Full_Data_Final$DeathRateCounterfactual <- Full_Data_Final$cumSumDeathsCounterfactual / Full_Data_Final$population * 100000
Full_Data_Final$DeathRateAverted <- Full_Data_Final$cumSumDeathsAverted / Full_Data_Final$population * 100000



#muni$muni_pac <- muni$code_muni

muniDeaths <- left_join(muni, Full_Data_Final, by="muni_pac")

pdf(file = "Averted_deaths.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8) # The height of the plot in inches
ggplot() +
  geom_sf(data=muniDeaths, aes(fill=DeathRateAverted), color= NA, size=.15) +
  labs(subtitle="Deaths averted via COVID-19 vaccination, Brazilian municipalities, 2021-2023", size=8) +
  scale_fill_distiller(palette = "Blues", name="Deaths averted", limits=c(0,2000)) +
  theme_void() 
dev.off()

pdf(file = "Counterfactual_deaths.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8) # The height of the plot in inches
ggplot() +
  geom_sf(data=muniDeaths, aes(fill=DeathRateCounterfactual), color= NA, size=.15) +
  labs(subtitle="Estimated deaths without vaccination, Brazilian municipalities, 2021-2023", size=8) +
  scale_fill_distiller(palette = "Blues", name="Deaths without vaccination, per 100,000", limits=c(0,2000)) +
  theme_void() 
dev.off()


pdf(file = "Observed_Deaths2.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8) # The height of the plot in inches
ggplot() +
  geom_sf(data=muniDeaths, aes(fill=DeathRateObserved), color= NA, size=.15) +
  labs(subtitle="COVID-19 post vaccine era deaths, Brazilian municipalities, 2021-2023", size=8) +
  scale_fill_distiller(palette = "Blues", name="Observed death rate, per 100,000", , limits=c(0,1000)) +
  theme_void() 

dev.off()

pdf(file = "AvertedDeathsPerc.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8) # The height of the plot in inches
ggplot() +
  geom_sf(data=muniDeaths, aes(fill=Perc_Deaths_Averted), color= NA, size=.15) +
  labs(subtitle="Percent COVID-19 deaths averted via COVID-19 vaccination, 2021-2023", size=8) +
  scale_fill_distiller(palette = "Greens", name="% Deaths averted", limits=c(0,100)) +
  theme_void() 

dev.off()

write.csv(Full_Data_Final, file="Deaths_Averted_Data_72623.csv")
