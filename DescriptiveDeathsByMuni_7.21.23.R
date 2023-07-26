library(data.table)
library(R.utils)
library(tidyr)
library(dplyr)
library(zoo)
library(tidyverse)
library(geobr)
library(sf)
library(readxl)

# 7/20/23
# Exploratory data analysis: 
# 1. Match population data to deaths data
# 2. Map data
# 

# Import cleaned data
Pop_by_age <- read.csv("Pop_by_Age.csv")
Pop_by_muni <- read.csv("code_cities.csv")
Deaths <- read.csv("Deaths_clean_72023.csv")

# Rename municipality variable for merging
Pop_by_muni <- rename(Pop_by_muni, muni_pac=code)

# Merge into single dataset
Death_Data1 <- left_join(Deaths, Pop_by_muni, by="muni_pac")
Death_Data <- left_join(Death_Data1, Pop_by_age, by="agegroup")
Death_Data$PopByAge <- Death_Data$population* Death_Data$Proportion

# Just for exploratory mapping: drop age variable so we can look at total deaths
# by region only
# 
Death_Data_noage <- Death_Data[,c(2:3,5:11)]

# Collapse data across age groups to get a single value
# 

Death_Data_noage2 <- Death_Data_noage %>% group_by(muni_pac, month) %>% 
  summarize(cumDeaths = sum(death), cumHosp = sum(hosp)) 
  
# Collapse data across months
Death_Data_noage3 <-  Death_Data_noage2 %>%
      group_by(muni_pac) %>%
      arrange(month) %>%
      mutate(cumSumDeaths = cumsum(cumDeaths),
             cumSumHosp = cumsum(cumHosp))

#max(Death_Data_noage3$month)

# Keep only cumulative data from April 2023

Death_Data_noage_Final <- Death_Data_noage3[which(Death_Data_noage3$month=="2023-04-01"),]

Death_Data_noage_Final <- Death_Data_noage_Final[,c(1,2,5,6)]

# Add municipality population estimates and calculate death rate and hosp rate
Death_Data_noage_Final <- left_join(Death_Data_noage_Final, Pop_by_muni, by="muni_pac")
Death_Data_noage_Final$DeathRate <- round((Death_Data_noage_Final$cumSumDeaths / Death_Data_noage_Final$population) * 100000,0)
Death_Data_noage_Final$HospRate <- round((Death_Data_noage_Final$cumSumHosp / Death_Data_noage_Final$population) * 100000,0)


######
###### Put on a map!
###### 
#

muni <- read_municipality(
  year=2020, 
  showProgress = FALSE
)

muni$muni_pac <- substr(as.character(muni$code_muni),1,
               nchar(muni$code_muni) -1)

Death_Data_noage_Final$muni_pac <- as.character(Death_Data_noage_Final$muni_pac)

#muni$muni_pac <- muni$code_muni

muniDeaths <- left_join(muni, Death_Data_noage_Final, by="muni_pac")

ggplot() +
  geom_sf(data=muniDeaths, aes(fill=DeathRate), color= NA, size=.15) +
  labs(subtitle="COVID-19 post vaccine era death rate, Brazilian States, 2021-2023", size=8) +
  scale_fill_distiller(palette = "Blues", name="Deaths per 100,000", limits = c(0,2000)) +
  theme_void() 

ggplot() +
  geom_sf(data=muniDeaths, aes(fill=HospRate), color= NA, size=.15) +
  labs(subtitle="COVID-19 post vaccine era hospitalization rate, Brazilian States, 2021-2023", size=8) +
  scale_fill_distiller(palette = "Blues", name="Hospitalizations per 100,000", limits = c(0,2000)) +
  theme_void() 






