library(sf)
library(terra)
library(exactextractr)
library(parallel)
library(data.table)
library(tictoc)
library(tidyverse)
library(patchwork)
################################
#'## Read data
################################
if(packageVersion("exactextractr")<= "0.7.2") {
  warning("Needs higher/dev version due to bug https://github.com/isciences/exactextractr/issues/68")
  devtools::install_github("isciences/exactextractr")
}

# ask for Matthieu Stigler for permission to get access to this code
source("../ZDCinBrazil/Data/rcode_EPL_clean_data.R")

MPB_meta <-read_csv('data/06_spatial/05_mapbiomas_pasture_quality/amazon/mapbiomas-brazil-collection-70-pasture-quality-amazonia-area.csv')
input_data <- st_read('analysis_2018-19/working_ds/car_withcattle1_adam_lr5_ens5.geojson')
degradation_2019 <- terra::rast('data/06_spatial/05_mapbiomas_pasture_quality/amazon/mapbiomas-brazil-collection-70-pasture-quality-amazonia-2019.tif')
degradation_2018 <- terra::rast('data/06_spatial/05_mapbiomas_pasture_quality/amazon/mapbiomas-brazil-collection-70-pasture-quality-amazonia-2018.tif')

################################
#'## Clean meta
################################

## prep MPB meta
MPB_meta_clean <- MPB_meta %>% 
  dplyr::select(band, class , class_name) %>% 
  mutate(year = str_extract(band, "[0-9]+") %>% as.integer())

MPB_meta_clean

### different by year?
MPB_meta_clean %>% 
  distinct(class, class_name, year) %>% 
  add_count(class, class_name, year) %>% 
  arrange(n)

## final version:
MPB_meta_clean2 <- MPB_meta_clean %>% 
  distinct(class, class_name, year) %>% 
  dplyr::rename(deg_class = class)%>% group_by(year) %>%
  reframe(add_row(cur_data(), deg_class = 0, class_name = 'No Data'))

MPB_meta_clean2

MPB_meta_clean2 <- MPB_meta_clean2[1:4,2:3]
MPB_meta_clean2

################################
#'## 
################################

epl_ras_extract_table(degradation_2019,input_data[1:3,], cols_keep = c('seq',"cod_imovel",'IBGE_CODE', 'SIGLA_UF'))
degradation_car_2019 <- epl_ras_extract_table(degradation_2019,input_data, cols_keep = c("cod_imovel",'IBGE_CODE', 'SIGLA_UF'))
degradation_car_2018 <- epl_ras_extract_table(degradation_2018,input_data, cols_keep = c("cod_imovel",'IBGE_CODE', 'SIGLA_UF'))


#somehow the entire ds is duplicated..
ddegradation_car_2019 <- unique( degradation_car_2019[ , c('cod_imovel','value','IBGE_CODE', 'SIGLA_UF', 'sum_area') ] )
ddegradation_car_2018 <- unique( degradation_car_2019[ , c('cod_imovel','value','IBGE_CODE', 'SIGLA_UF', 'sum_area') ] )

ddegradation_car_2019$year <- 2019
ddegradation_car_2018$year <- 2018

deg<- rbind(ddegradation_car_2018,ddegradation_car_2019)

out_LUC <-  deg %>% 
  dplyr::rename(deg_class = value) %>% 
  left_join(MPB_meta_clean2, by = "deg_class") %>% 
  mutate(across(c(IBGE_CODE, deg_class), as.integer)) %>% 
  group_by(cod_imovel,deg_class,IBGE_CODE,SIGLA_UF,class_name) %>% 
  summarise(mean_area = mean(sum_area)) %>% 
  ungroup() %>%
  group_by(cod_imovel)%>%
  mutate(area_tot = sum(mean_area)) %>% 
  mutate(area_perc = round(100 * mean_area/area_tot,2)) %>% 
  
  ungroup() %>% 
  #rename(deg_class_id = value) %>% 
  mutate(year = '2018-2019') %>%
  relocate(year, .after = SIGLA_UF) 

st_write(out_LUC, "data/13_CAR_spatial_infrence_merged/pasture_deg_selected_car_201819.geojson")

# visual inspection
car_shape <- st_as_sf(input_data[133,])
car_shape
# crop raster
r2 <- crop(degradation_2019, car_shape)
# and plot it
r3 <- mask(r2, car_shape,touches=FALSE)

plot(r3)
r3_poly <-terra::as.polygons(r3)
# convert in sf object
r3_poly <-st_as_sf(r3_poly)
r3_poly$centroid <- r3_poly$geometry%>% st_centroid()

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

register_google(key='AIzaSyAdYd-PCQ6rGSAyseKrsUO-vdzVp4Ux3Bw')
g <- get_googlemap(center = c(r3_poly$centroid[[1]][1], r3_poly$centroid[[1]][2]), maptype = "satellite", zoom=16)

ggmap(g) + 
  theme_light() +
  geom_sf(data=r3_poly, aes(fill=pasture_quality_2019),alpha=0.4,inherit.aes = FALSE)+
  #scale_fill_manual(name='Land use',values=c(cbPalette[4], cbPalette[2],cbPalette[5],cbPalette[3]) )+
  inset_raster(as.raster(r3),-62.60494, -62.58931, -10.48011, -10.46501 )+
  geom_sf(data = car_shape, fill = NA, inherit.aes = FALSE,color = cbPalette[4],lwd=1)


MPB_meta_clean
