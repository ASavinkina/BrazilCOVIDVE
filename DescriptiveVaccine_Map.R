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
# Exploratory data analysis: 
# 1. Match population data to vaccine data
# 2. Map data
# 

# Import cleaned data
Pop_by_age <- read.csv("Pop_by_Age.csv")
Pop_by_muni <- read.csv("code_cities.csv")
Vaccinations <- read.csv("Vaccine_data_clean_72023.csv")

# Rename municipality variable for merging
Pop_by_muni <- rename(Pop_by_muni, muni_pac=code)


# Merge into single dataset
Vaccine_Data1 <- left_join(Vaccinations, Pop_by_muni, by="muni_pac")
Vaccine_Data <- left_join(Vaccine_Data1, Pop_by_age, by="agegroup")
Vaccine_Data$PropDose1 <- Vaccine_Data$cum_dose1/Vaccine_Data$population
Vaccine_Data$PropDose2 <- Vaccine_Data$cum_dose2/Vaccine_Data$population

# Just for exploratory mapping: drop age variable so we can look at total deaths
# by region only
# 
Vaccine_Data_noage <- Vaccine_Data[,c(2:3,5,97:98)]

# Collapse data across age groups to get a single value
# 

Vaccine_Data_noage2 <- Vaccine_Data_noage %>% group_by(muni_pac, month) %>% 
  summarize(cumVaccineDose1 = sum(PropDose1), cumVaccineDose2 = sum(PropDose2))


# Keep only cumulative data from April 2023

Vaccine_Data_noage_Final <- Vaccine_Data_noage2[which(Vaccine_Data_noage2$month=="2023-04-01"),]

# Put as rate per 100,000
# 
Vaccine_Data_noage_Final$cumSumVax1_rate <- Vaccine_Data_noage_Final$cumVaccineDose1*100000
Vaccine_Data_noage_Final$cumSumVax2_rate <- Vaccine_Data_noage_Final$cumVaccineDose2*100000


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

Vaccine_Data_noage_Final$muni_pac <- as.character(Vaccine_Data_noage_Final$muni_pac)

#muni$muni_pac <- muni$code_muni

muniVaccine <- left_join(muni, Vaccine_Data_noage_Final, by="muni_pac")

ggplot() +
  geom_sf(data=muniVaccine, aes(fill=cumVaccineDose1), color= NA, size=.15) +
  labs(subtitle="COVID-19 post vaccine era one dose vaccination proportion, Brazilian States, 2021-2023", size=8) +
  scale_fill_distiller(palette = "Blues", name="One dose vaccination proportion", limits = c(0,1)) +
  theme_void() 

ggplot() +
  geom_sf(data=muniVaccine, aes(fill=cumVaccineDose2), color= NA, size=.15) +
  labs(subtitle="COVID-19 post vaccine era two dose vaccination proportion, Brazilian States, 2021-2023", size=8) +
  scale_fill_distiller(palette = "Blues", name="Two dose vaccination proportion", limits = c(0,1)) +
  theme_void() 
