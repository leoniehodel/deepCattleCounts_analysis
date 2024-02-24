library(sf)
library(terra)
library(exactextractr)
library(parallel)
library(data.table)
library(tictoc)
library(tidyverse)
################################
#'## Read data
################################
if(packageVersion("exactextractr")<= "0.7.2") {
  warning("Needs higher/dev version due to bug https://github.com/isciences/exactextractr/issues/68")
  devtools::install_github("isciences/exactextractr")
}

# ask for Matthieu Stigler for permission to get access to this code
source("../ZDCinBrazil/Data/rcode_EPL_clean_data.R")

MPB_meta <-read_csv('data/06_spatial/04_mapbiomas_transitions/mapbiomas-brazil-collection-70-amazonia-area.csv')

################################
#'## HYPERPARAMETERS
################################
input_data <- st_read('analysis_2018-19/working_ds/car_withcattle1_adam_lr5_ens5.geojson')
buffer_size <- 10000
################################
#'## Clean meta
################################

## prep MPB meta
MPB_meta_clean <- MPB_meta %>% 
  dplyr::select(band, class, from_class, to_class) %>% 
  mutate(year = str_extract(band, "[0-9]+") %>% as.integer())

MPB_meta

### different by year?
MPB_meta_clean %>% 
  distinct(class, from_class, to_class, year) %>% 
  add_count(class, from_class, to_class) %>% 
  arrange(n)

## final version:
MPB_meta_clean2 <- MPB_meta_clean %>% 
  distinct(class, from_class, to_class) %>% 
  dplyr::rename(LUC_class = class)

MPB_meta_clean2

################################
#'## property 
################################

yearfile<- c('2013_2014','2014_2015','2015_2016','2016_2017','2017_2018')

for(i in 1:length(yearfile)){
  raster_path <-tibble(full_path = list.files("data/06_spatial/04_mapbiomas_transitions/mapbiomas_v7_amazoina/",full.names = TRUE, pattern = yearfile[i]))
  print(raster_path)
  luc <- map(raster_path[[1]][], terra::rast)
  rsrc<- terra::sprc(luc)
  mo <-mosaic(rsrc)
  crs(mo)  <- "epsg:4674"
  LUC <- epl_ras_extract_table(mo,input_data, cols_keep = c('seq',"cod_imovel",'IBGE_CODE', 'SIGLA_UF'))
  LUC <- unique( LUC[ , c('seq','cod_imovel','value','IBGE_CODE', 'SIGLA_UF', 'sum_area') ] )
  
  out_LUC <-  LUC %>% 
    rename(LUC_class = value) %>% 
    left_join(MPB_meta_clean2, by = "LUC_class") %>% 
    mutate(across(c(IBGE_CODE, LUC_class), as.integer)) %>% 
    group_by(cod_imovel) %>% 
    mutate(area_perc = round(100 * sum_area/sum(sum_area),3)) %>% 
    mutate(area_tot = sum(sum_area))%>% 
    ungroup() %>% 
    rename(LUC_class_from = from_class,
           LUC_class_to = to_class,
           LUCclass_id = LUC_class) %>% 
    mutate(year =  yearfile[i]) 

  out_LUC
  out_LUC %>% filter(is.na(LUC_class_from)) #%>% summarise(sum(sum_area))
  st_write(out_LUC, paste0("data/13_CAR_spatial_infrence_merged/LUC_selected_car_",yearfile[i],".geojson"))
  gc()
  #LUC_all_years <-rbind(LUC_all_years,LUC)
  #writeRaster(mo, filename = paste0("data/06_spatial/04_mapbiomas_transitions/mapbiomas_v7_consolidated/",yearfile[i],".tif"), overwrite=TRUE)
}

################################
#'##   BUFFFER
################################

CAR_buffer <- input_data %>% st_buffer(buffer_size)
yearfile<- c('2013_2014','2014_2015','2015_2016','2016_2017','2017_2018')

for(i in 1:length(yearfile)){
  raster_path <-tibble(full_path = list.files("data/06_spatial/04_mapbiomas_transitions/mapbiomas_v7_amazoina/",full.names = TRUE, pattern = yearfile[i]))
  print(raster_path)
  luc <- map(raster_path[[1]][], terra::rast)
  
  rsrc<- terra::sprc(luc)
  
  mo <-mosaic(rsrc)
  crs(mo)  <- "epsg:4674"
  LUC <- epl_ras_extract_table(mo,CAR_buffer, cols_keep = c('seq',"cod_imovel",'IBGE_CODE', 'SIGLA_UF'))
  LUC <- unique( LUC[ , c('seq','cod_imovel','value','IBGE_CODE', 'SIGLA_UF', 'sum_area') ] )
  
  out_LUC <-  LUC %>% 
    dplyr::rename(LUC_class = value) %>% 
    left_join(MPB_meta_clean2, by = "LUC_class") %>% 
    mutate(across(c(IBGE_CODE, LUC_class), as.integer)) %>% 
    group_by(cod_imovel) %>% 
    mutate(area_perc = round(100 * sum_area/sum(sum_area),3)) %>% 
    mutate(area_tot = sum(sum_area))%>% 
    ungroup() %>% 
    dplyr::rename(LUC_class_from = from_class,
           LUC_class_to = to_class,
           LUCclass_id = LUC_class) %>% 
    mutate(year =  yearfile[i]) 
  
  out_LUC
  length(unique(out_LUC$cod_imovel))
  out_LUC %>% filter(is.na(LUC_class_from)) #%>% summarise(sum(sum_area))
  st_write(out_LUC, paste0("data/13_CAR_spatial_infrence_merged/LUC_selected_car_buffer_",yearfile[i],".geojson"))
  gc()

}


