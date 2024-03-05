library(data.table)
library(tidyverse)
library(readxl)

# these files have been prepossessed on Google Earth Engine using municipality borders
# check the downloaded files 

# and convert the units 
# in mm/ha
# https://developers.google.com/earth-engine/datasets/catalog/NASA_GPM_L3_IMERG_V06
pre20189 <- read_csv('data_raw_secundary/abiotic/precipitation_muns_2018_2019.csv') %>% select(1:10) %>% mutate(precipitation_18_19 = precipitation ) %>% select(CD_MUN,precipitation_18_19 )
write.csv(pre20189, 'data_raw_secundary/abiotic/precipitation_mm_he_munis_2018.csv')

# https://developers.google.com/earth-engine/datasets/catalog/JAXA_GCOM-C_L3_LAND_LST_V3
# temperature -change from K to Celcius
temp20189 <- read_csv('data_raw_secundary/abiotic/temp_muns_2018_2019.csv')  %>% select(1:7) %>% mutate(mean_temp_18_19 = LST_AVE-273.15) %>% select(CD_MUN,mean_temp_18_19 )
write.csv(temp20189, 'data_raw_secundary/abiotic/temp_munis_2018_2019.csv')
