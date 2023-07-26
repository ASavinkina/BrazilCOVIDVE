library(data.table)
library(R.utils)
library(tidyr)
library(dplyr)
library(zoo)
library(tidyverse)
library(geobr)
library(sf)
library(readxl)

# Brazil COVID-19 vaccine impact by municipality, Vaccination Data Cleaning
# 7/14/23
# 
# 7/14/23: Merged together vaccine doses 1-5 for all vaccine types, merged in municipality
#          names, and calculated cumulative number of doses for each vaccine over time.
#          To do: Calculate number of people protected by each dose #, considering type of vaccine
#          and vaccine waning over time
#          Challenge: people likely mixed vaccines, and this is difficult to calculate (ie difficult to know
#          what percent of people with ONLY dose 1 have pfizer vs AZ etc)
# 7/17/23: Calculated number of people vaccinated by vaccine dose over time assuming
#          same proportion by vaccine product as baseline.
#          Next: add vaccine waning. We need vaccine effectiveness to wane after 12 months.
#          Challenge: we need to figure out how to do this with some people moving to subsequent vaccine dose.
#          Also: add vaccine effectiveness by product and dose.


# Import data on vaccination 

dose1 = fread("residence_municipality_D1.csv.gz")
dose2 = fread("residence_municipality_D2.csv.gz")
dose3 = fread("residence_municipality_D3.csv.gz")
dose4 = fread("residence_municipality_D4.csv.gz")
dose5 = fread("residence_municipality_D5.csv.gz")

dose1_wide <- spread(dose1, key = vacina, value = n)
colnames(dose1_wide)[c(4:7)] <- paste('dose1', colnames(dose1_wide)[c(4:7)], sep = '_')

dose2_wide <- spread(dose2, key = vacina, value = n)
colnames(dose2_wide)[c(4:7)] <- paste('dose2', colnames(dose2_wide)[c(4:7)], sep = '_')

dose3_wide <- spread(dose3, key = vacina, value = n)
colnames(dose3_wide)[c(4:7)] <- paste('dose3', colnames(dose3_wide)[c(4:7)], sep = '_')

dose4_wide <- spread(dose4, key = vacina, value = n)
colnames(dose4_wide)[c(4:7)] <- paste('dose4', colnames(dose4_wide)[c(4:7)], sep = '_')

dose5_wide <- spread(dose5, key = vacina, value = n)
colnames(dose5_wide)[c(4:7)] <- paste('dose5', colnames(dose5_wide)[c(4:7)], sep = '_')


#put all data frames into list
vaccination_list <- list(dose1_wide, dose2_wide, dose3_wide
                         , dose4_wide, dose5_wide)

#merge all data frames in list
#
vaccination_data <- vaccination_list %>% 
  reduce(full_join, by=c('muni_pac','agegroup','month'))

# Include names of municipalities, exclude any codes not corresponding to listed municpilaity
# Assuming data entry error
# 

municipalities <- read.csv("municipios_codigos.csv")

municipalities$muni_pac <- substr(as.character(municipalities$Código.Município.Completo),1,
                                  nchar(municipalities$Código.Município.Completo) -1)

municipalities$muni_pac <- as.numeric(municipalities$muni_pac)

municipalities2 <- municipalities[,c(13,14)]

vaccination_data2 <- left_join(municipalities2, vaccination_data, by="muni_pac")


# For data cleaning only: limit to single municipality (Sao Paolo)

#vaccination_data3 <- vaccination_data2[which(vaccination_data2$muni_pac==355030 & vaccination_data2$agegroup==5),]
vaccination_data3 <- vaccination_data2

# Calculate cumulative number of doses, by age and municipality
# 
vaccination_data4<-  vaccination_data3 %>% 
  group_by(muni_pac, agegroup) %>%
  arrange(month) %>%
  mutate(cum_1dose_AZ = cumsum(dose1_AZ)) %>%
  mutate(cum_1dose_CV = cumsum(dose1_CV)) %>%
  mutate(cum_1dose_JS = cumsum(dose1_JS)) %>%
  mutate(cum_1dose_PF = cumsum(dose1_PF)) %>%
  
  mutate(cum_2dose_AZ = cumsum(dose2_AZ)) %>%
  mutate(cum_2dose_CV = cumsum(dose2_CV)) %>%
  mutate(cum_2dose_JS = cumsum(dose2_JS)) %>%
  mutate(cum_2dose_PF = cumsum(dose2_PF)) %>%
  
  mutate(cum_3dose_AZ = cumsum(dose3_AZ)) %>%
  mutate(cum_3dose_CV = cumsum(dose3_CV)) %>%
  mutate(cum_3dose_JS = cumsum(dose3_JS)) %>%
  mutate(cum_3dose_PF = cumsum(dose3_PF)) %>%
  
  mutate(cum_4dose_AZ = cumsum(dose4_AZ)) %>%
  mutate(cum_4dose_CV = cumsum(dose4_CV)) %>%
  mutate(cum_4dose_JS = cumsum(dose4_JS)) %>%
  mutate(cum_4dose_PF = cumsum(dose4_PF)) %>%
  
  mutate(cum_5dose_AZ = cumsum(dose5_AZ)) %>%
  mutate(cum_5dose_CV = cumsum(dose5_CV)) %>%
  mutate(cum_5dose_JS = cumsum(dose5_JS)) %>%
  mutate(cum_5dose_PF = cumsum(dose5_PF)) %>%
  ungroup


# Realization: while we have dose data by date for each vaccine, we don't
# know how people may have mixed vaccines (ie first vaccine AZ, second PF, etc).
# Therefore we cannot assume people stuck to a single vaccine type.
# Instead, will calculate % of each vaccine dose that was each product, and assume
# that people independently moved on to additional doses (ie whether someone got PF
# or AZ first didn't affect their likelihood of subsequent vaccination).
# This is important because vaccine effectiveness varies by product- we want to know
# about what % of people who only had 1 dose or only 2 doses etc were in each vaccine
# category.

vaccination_data4 <- vaccination_data4 %>% 
  mutate(cum_dose1 = vaccination_data4$cum_1dose_AZ + 
                     vaccination_data4$cum_1dose_CV +
                     vaccination_data4$cum_1dose_JS +
                     vaccination_data4$cum_1dose_PF,
         cum_dose2 = vaccination_data4$cum_2dose_AZ + 
                     vaccination_data4$cum_2dose_CV +
                     vaccination_data4$cum_2dose_JS +
                     vaccination_data4$cum_2dose_PF,
         cum_dose3 = vaccination_data4$cum_3dose_AZ + 
                     vaccination_data4$cum_3dose_CV +
                     vaccination_data4$cum_3dose_JS +
                     vaccination_data4$cum_3dose_PF,
         cum_dose4 = vaccination_data4$cum_4dose_AZ + 
                     vaccination_data4$cum_4dose_CV +
                     vaccination_data4$cum_4dose_JS +
                     vaccination_data4$cum_4dose_PF,
         cum_dose5 = vaccination_data4$cum_5dose_AZ + 
                     vaccination_data4$cum_5dose_CV +
                     vaccination_data4$cum_5dose_JS +
                     vaccination_data4$cum_5dose_PF,) %>%
  mutate(p_dose1_AZ = vaccination_data4$cum_1dose_AZ / cum_dose1,
         p_dose1_CV = vaccination_data4$cum_1dose_CV / cum_dose1,
         p_dose1_JS = vaccination_data4$cum_1dose_JS / cum_dose1,
         p_dose1_PF = vaccination_data4$cum_1dose_PF / cum_dose1,
         p_dose2_AZ = vaccination_data4$cum_2dose_AZ / cum_dose2,
         p_dose2_CV = vaccination_data4$cum_2dose_CV / cum_dose2,
         p_dose2_JS = vaccination_data4$cum_2dose_JS / cum_dose2,
         p_dose2_PF = vaccination_data4$cum_2dose_PF / cum_dose2,
         p_dose3_AZ = vaccination_data4$cum_3dose_AZ / cum_dose3,
         p_dose3_CV = vaccination_data4$cum_3dose_CV / cum_dose3,
         p_dose3_JS = vaccination_data4$cum_3dose_JS / cum_dose3,
         p_dose3_PF = vaccination_data4$cum_3dose_PF / cum_dose3,
         p_dose4_AZ = vaccination_data4$cum_4dose_AZ / cum_dose4,
         p_dose4_CV = vaccination_data4$cum_4dose_CV / cum_dose4,
         p_dose4_JS = vaccination_data4$cum_4dose_JS / cum_dose4,
         p_dose4_PF = vaccination_data4$cum_4dose_PF / cum_dose4)


#   Now: We want to know what number of people JUST have one dose, vs two vs three etc.
#   Assume %'s of those who don't get additional dose by vaccine product 
#   is the same as % of those who had that vaccine product. 
#   

vaccination_data5<-  vaccination_data4 %>%
  mutate(onedose = cum_dose1-cum_dose2,
         twodose = cum_dose2-cum_dose3,
         threedose = cum_dose3-cum_dose4,
         fourdose = cum_dose4- cum_dose5,
         fivedose = cum_dose5) %>%

  mutate(AZ_1dose = p_dose1_AZ * onedose,
         CV_1dose = p_dose1_CV * onedose,
         JS_1dose = p_dose1_JS * onedose,
         PF_1dose = p_dose1_PF * onedose,
         AZ_2dose = p_dose2_AZ * twodose,
         CV_2dose = p_dose2_CV * twodose,
         JS_2dose = p_dose2_JS * twodose,
         PF_2dose = p_dose2_PF * twodose,
         AZ_3dose = p_dose3_AZ * threedose,
         CV_3dose = p_dose3_CV * threedose,
         JS_3dose = p_dose3_JS * threedose,
         PF_3dose = p_dose3_PF * threedose,
         AZ_4dose = p_dose4_AZ * fourdose,
         CV_4dose = p_dose4_CV * fourdose,
         JS_4dose = p_dose4_JS * fourdose,
         PF_4dose = p_dose4_PF * fourdose,
         AZ_5dose = cum_5dose_AZ,
         CV_5dose = cum_5dose_CV,
         JS_5dose = cum_5dose_JS,
         PF_5dose = cum_5dose_PF) 

# Export data for use
# 
# 
# 
#write.csv(vaccination_data5, file=("Vaccine_data_clean_72023.csv"))
#write.csv(vaccination_data5, file="Vaccine_data_clean_72023.csv")

