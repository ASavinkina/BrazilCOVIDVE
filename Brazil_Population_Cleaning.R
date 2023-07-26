library(data.table)
library(R.utils)
library(tidyr)
library(dplyr)
library(zoo)
library(tidyverse)
library(geobr)
library(sf)
library(readxl)

# Code to organize population data
# We need this data to calculate rates of death and vaccination by municipality
# We have overall population by municipality but not by age group
# Will use age structure of Brazil from the WPP, together with municipality pop data
# This assumes same age structure for all municipalities: limitation
# 
# 
# Age data from Brazil:

# Import population data 
# 

Population_by_age <- read_excel('WPP2022_POP_Brazil.xlsx', skip=16)

#   Keep only data for Brazil

Population_by_age <- Population_by_age[which(Population_by_age$`Region, subregion, country or area *`=="Brazil" & Population_by_age$Year==2021),]

#   Format age counts into numeric

Population_by_age <- Population_by_age %>% mutate_at(c(12:112), as.numeric,na.rm=TRUE)

#   Check total population
#rowSums(Population_by_age[,c(12:112)])
#
#
#   Make age categories according to our vaccination and deaths data
#   
Population_by_age$Pop_u18 <- rowSums(Population_by_age[,c(12:29)]) # 0 to 17
Population_by_age$Pop_18to29 <- rowSums(Population_by_age[,c(30:41)]) # 18-29
Population_by_age$Pop_30to39 <- rowSums(Population_by_age[,c(42:51)]) # 30-39
Population_by_age$Pop_40to49 <- rowSums(Population_by_age[,c(52:61)]) # 40-49
Population_by_age$Pop_50to59 <- rowSums(Population_by_age[,c(62:71)]) # 50-59
Population_by_age$Pop_60to69 <- rowSums(Population_by_age[,c(72:81)]) # 60-69
Population_by_age$Pop_70to79 <- rowSums(Population_by_age[,c(82:91)]) # 70-79
Population_by_age$Pop_80to89 <- rowSums(Population_by_age[,c(92:101)]) # 80-89
Population_by_age$Pop_90u <- rowSums(Population_by_age[,c(102:112)]) # 90+

#   Keep only necessary data
Population_by_age2 <- Population_by_age[,c(113:121)]

#   Check that population is correct
rowSums(Population_by_age2)

#   Make proportion of population by age group
#   

Population_by_age2$Prop_u18 <- Population_by_age2$Pop_u18 / rowSums(Population_by_age2)
Population_by_age2$Prop_18to29 <- Population_by_age2$Pop_18to29 / rowSums(Population_by_age2)
Population_by_age2$Prop_30to39 <- Population_by_age2$Pop_30to39 / rowSums(Population_by_age2)
Population_by_age2$Prop_40to49 <- Population_by_age2$Pop_40to49 / rowSums(Population_by_age2)
Population_by_age2$Prop_50to59 <- Population_by_age2$Pop_50to59 / rowSums(Population_by_age2)
Population_by_age2$Prop_60to69 <- Population_by_age2$Pop_60to69 / rowSums(Population_by_age2)
Population_by_age2$Prop_70to79 <- Population_by_age2$Pop_70to79 / rowSums(Population_by_age2)
Population_by_age2$Prop_80to89 <- Population_by_age2$Pop_80to89 / rowSums(Population_by_age2)
Population_by_age2$Prop_90u <- Population_by_age2$Pop_90u / rowSums(Population_by_age2)

# Transition to long dataframe with matching age group variable to vaccine and deaths data 

Population_by_age2_long <- Population_by_age2 %>% 
  pivot_longer(
    cols = `Prop_u18`:`Prop_90u`, 
    names_to = "age",
    values_to = "Proportion")

# Match to naming convenetion of age group

Population_by_age2_long$agegroup <- ifelse(Population_by_age2_long$age=="Prop_u18", 1,
                                           ifelse(Population_by_age2_long$age=="Prop_18to29",  2,
                                                  ifelse(Population_by_age2_long$age == "Prop_30to39", 3,
                                                         ifelse(Population_by_age2_long$age == "Prop_40to49", 4,
                                                                ifelse(Population_by_age2_long$age == "Prop_50to59", 5,
                                                                       ifelse(Population_by_age2_long$age =="Prop_60to69", 6,
                                                                              ifelse(Population_by_age2_long$age == "Prop_70to79", 7,
                                                                                     ifelse(Population_by_age2_long$age=="Prop_80to89", 8,
                                                                                            ifelse(Population_by_age2_long$age=="Prop_90u",9, NA))
                                                                              )))))) )

#   Keep only necessary data
Population_by_age_proportions <- Population_by_age2_long[,c(11:12)]

#   Export data 
#   

write.csv(Population_by_age_proportions, file="Pop_by_Age.csv")

# Import municipality population data 
# Looks fine
 
muni_pop <- read.csv("code_cities.csv")
