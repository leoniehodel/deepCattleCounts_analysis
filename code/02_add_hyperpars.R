#' ---
#' Title: "Regression analysis with stocking rate as regressand"
#' Author: "Leonie"
#' Date: 2023_02_24
#' ---

library(sf)
library(tidyverse)
library(arm)
library(units)
library(estimatr)
library(texreg)
library(vtable)
################################
#'## HYPERPARAMETERS
################################
min_area_car = 5
min_pasture = 2
max_area_car  = 800
min_ncattle = 4 
sigma_cut = 0.90
#CV_cut = 0.8
min_prop_mun = 10
sr_max = 10

################################
#'## Read in data
################################

output<- 'regression_pars.csv'

#new regession pars added like population number
ds_r<-st_read('data/regression_pars_final_raw.geojson')
ds_r$agricultural_credits<-ds_r$abc_inv_all_2013_2017.y/ds_r$credits_val_2013_2017.y
ds_r_clean<- ds_r  %>% dplyr::select(SIGLA_UF,
                                     n_cattle,
                                     n_cattle_sd,
                                     pasture_201819_tot,
                                     defo_abs_13_17,
                                     defo_buffer_abs_13_17,
                                     mod_deg_sum_201819,
                                     sev_deg_sum_201819,
                                     crop_201819_tot,
                                     agricultural_credits,
                                     mean_mun_msg4_1317,
                                     area,
                                     buffer_forest_tot_2013,
                                     precipitation_2018_19,
                                     mean_temp_18_19,
                                     population,
                                     nearest_fedSh,
                                     IBGE_CODE,
                                     outline_id)

ds_r_clean$area<- st_area(ds_r_clean) %>% drop_units()
ds_r_clean<- ds_r_clean%>% as.data.frame()  %>% distinct()
ds_r_clean<- ds_r_clean[ds_r_clean$n_cattle_sd < quantile(ds_r_clean$n_cattle_sd, sigma_cut),]

ds<-ds_r_clean
ds$nearest_fedSh <- ds$nearest_fedSh/1000
ds$population <- ds$population/1000
ds$buffer_forest_tot_2013 <- ds$buffer_forest_tot_2013*0.01

################################
#'## Preprocess
################################
dsfilter<-ds%>%
  mutate(area = round(area*0.0001,3), stocking_rate = n_cattle/pasture_201819_tot) %>%
  filter(area > min_area_car, area< max_area_car, n_cattle > min_ncattle, pasture_201819_tot>min_pasture) %>%
  group_by(IBGE_CODE) %>% filter(n() >min_prop_mun) %>% ungroup

# filter out sr higher than 10, these areas have been checked and are artefacts  
# confinement systems are therefore most likely not included

dsfilter<- dsfilter  %>% replace(is.na(.), 0) %>% filter(stocking_rate<sr_max)
ds<-dsfilter %>%  dplyr::select(SIGLA_UF,n_cattle,n_cattle_sd,pasture_201819_tot,stocking_rate, 
                                defo_abs_13_17,defo_buffer_abs_13_17,mod_deg_sum_201819,sev_deg_sum_201819,
                                crop_201819_tot,agricultural_credits,mean_mun_msg4_1317,
                                area,buffer_forest_tot_2013, precipitation_2018_19,mean_temp_18_19,
                                population,nearest_fedSh,
                                everything())


# summary stats of all variables including pasture, 
st(ds, file='summarystats_full_ds.csv')

# the inflation factor is not added here because it is only a constant and
# does not make any difference for the regression analysis

################################
#'##  Drop variables not used in the regression 
###############################
drop <- c("COD_IMOVEL","date",'n_cattle',
          'n_cattle_sd','pasture_201819_tot',
          'geometry','cv' )
dsdrop = ds[,!(names(ds) %in% drop)] 

# save dsdrop
write.csv(dsdrop, paste0("data/",output), row.names = FALSE)
# 
