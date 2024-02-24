library(sf)
library(data.table)
library(tidyverse)
library(scales)
library(reshape)
library(units)
library("osmdata")
library(ggmap)
library(xlsx)
################################
#'## Read in SR data
################################
# The data is filtered for the years 2018-2019 here 
df <- st_read('data/car_withcattle1_adam_lr5_ens5.geojson') %>% 
  dplyr::select(COD_IMOVEL= cod_imovel, IBGE_CODE, SIGLA_UF, date, outline_id, n_cattle,n_cattle_sd ) %>% filter(date > '2018-01-01')
df$area <- st_area(df) #%>% drop_units()%>%
df <- df%>%mutate(area = round(area*0.0001,4))
 
# 
agg_df <- aggregate(df$COD_IMOVEL, by=list(df$SIGLA_UF), FUN=length)
agg_df
length(unique(df$COD_IMOVEL))

################################
#'## Mean land use in the years 2018-2019 using MapBiomas v7 
################################

#
all_car_LU_2019<- st_read('data/13_CAR_spatial_infrence_merged/land_use_selected_car_20182019.geojson') %>% 
  dplyr::rename('COD_IMOVEL' = 'cod_imovel')%>% as.data.frame() %>% dplyr::select(COD_IMOVEL,  year, class,class_name, mean_area, area_tot, area_perc)

all_car_LU_2019

selected_car_LU_2019 <- df %>% left_join(all_car_LU_2019,by='COD_IMOVEL')%>% as.data.frame() %>% dplyr::select(COD_IMOVEL, IBGE_CODE, SIGLA_UF, year, class,class_name, mean_area, area_tot, area_perc)

length(selected_car_LU_2019$COD_IMOVEL)
length(unique(selected_car_LU_2019$COD_IMOVEL))

#add levels
selected_car_LU_2019 <- within(selected_car_LU_2019, class <- factor(class, levels=names(sort(table(class), decreasing=TRUE))))
selected_car_LU_2019

# print a col plot with all LU
ggplot(selected_car_LU_2019,aes(class_name,area_tot)) + 
  geom_col(na.rm=TRUE)+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))


# funtion to merge land use data with the selected car data
process_land_use <- function(data, luclass, new_col_prefix, target_df) {
  result <- data %>% filter(class == luclass)%>%
    group_by(COD_IMOVEL) %>%
    summarise(!!paste0(new_col_prefix, "_tot") := round(sum(mean_area * 0.0001),4),
               !!paste0(new_col_prefix, "_perc") := sum(area_perc)) %>%
    dplyr::select(COD_IMOVEL, !!paste0(new_col_prefix, "_tot"), !!paste0(new_col_prefix, "_perc"))

  print(paste0('Number of CAR with this LU: ', length(result$COD_IMOVEL)))
  target_df <- merge(target_df, result, by = "COD_IMOVEL",all.x = TRUE)
  print(paste0('Total remaining CAR: ',length(target_df$COD_IMOVEL)))
  return(target_df)
}

df <- process_land_use(selected_car_LU_2019, "15", "pasture_201819", df)
df <- process_land_use(selected_car_LU_2019, "3", "forest_201819", df)
# the function allows to combine various LU classes
df <- process_land_use(selected_car_LU_2019, c("39", "41"), "crop_201819", df)
df <- process_land_use(selected_car_LU_2019, '12', "grass_201819", df)
df <- process_land_use(selected_car_LU_2019, '4', "savanna_201819", df)

# test
length(df$COD_IMOVEL)
length(unique(df$COD_IMOVEL))

################################
#'## Pasture degradation 2018-2019
################################
# pasture deg 
deg_all_car <- st_read('data/13_CAR_spatial_infrence_merged/pasture_deg_selected_car_201819.geojson')

length(deg_all_car$cod_imovel)
length(unique(deg_all_car$cod_imovel))
# change to mean_area
selected_car_deg_2019 <- deg_all_car %>% as.data.frame() %>%  dplyr::rename(COD_IMOVEL=cod_imovel)%>%
  dplyr::select(COD_IMOVEL, IBGE_CODE, SIGLA_UF, year, mean_area,class_name, area_tot, area_perc)%>% filter(COD_IMOVEL %in% df$COD_IMOVEL)

length(unique(selected_car_deg_2019$COD_IMOVEL))

# 
sev_deg <- selected_car_deg_2019 %>%
  filter(class_name == "Severe degradation") %>% group_by(COD_IMOVEL)%>%
  summarize(sev_deg_perc_201819 = area_perc, sev_deg_sum_201819 = round(mean_area* 0.0001,4))

mod_deg <- selected_car_deg_2019 %>%
  filter(class_name == "Moderate degradation") %>%group_by(COD_IMOVEL)%>%
  summarize(mod_deg_perc_201819 = area_perc, mod_deg_sum_201819 = round(mean_area* 0.0001,4))

length(mod_deg$COD_IMOVEL)
length(unique(mod_deg$COD_IMOVEL))


df <- df %>%
  left_join(sev_deg, by = 'COD_IMOVEL') %>%
  left_join(mod_deg, by = 'COD_IMOVEL') %>%
  replace(is.na(.), 0)


################################
#'## Deforestation and buffer 
################################

# Function to process deforestation and reforestation data
process_luc_selected_car <- function(year_range, class_from, class_to, target_df, LUC_type) {
  result <- st_read(paste0('data/13_CAR_spatial_infrence_merged/LUC_selected_car_', year_range, '.geojson')) %>% as.data.frame() %>% 
    dplyr::select(cod_imovel,LUC_class_from,LUC_class_to,sum_area,area_perc)%>%
    filter(LUC_class_from == class_from, LUC_class_to == class_to) %>%
    group_by(cod_imovel) %>%
    dplyr::summarise(!!paste0(LUC_type,'_perc_', year_range) := area_perc,!!paste0(LUC_type,'_tot_', year_range) := round(sum_area * 0.0001, 4)) %>%
    dplyr::select(COD_IMOVEL = cod_imovel, !!paste0(LUC_type,'_tot_', year_range),!!paste0(LUC_type,'_perc_', year_range))
  
  target_df <- merge(target_df, result, by = "COD_IMOVEL", all.x = TRUE)
  return(target_df)
}
df2<-df%>%dplyr::select(COD_IMOVEL)
# Process deforestation data for different years
defo_years <- c("2013_2014", "2014_2015", "2015_2016", "2016_2017", "2017_2018")
for (year in defo_years) {
  df2 <- process_luc_selected_car(year, "Forest Formation", "Pasture", df2, 'defo')
}

# Process reforestation data for different years
refo_years <- c("2013_2014", "2014_2015", "2015_2016", "2016_2017", "2017_2018")
for (year in refo_years) {
  df2 <- process_luc_selected_car(year, "Pasture", "Forest Formation", df2, 'refo')
}


df2 <- df2%>%replace(is.na(.), 0)
df2$defo_perc_wo_refo_13_17 <- (df2$defo_perc_2013+df2$defo_perc_2014+df2$defo_perc_2015+df2$defo_perc_2016+df2$defo_perc_2017) - (df2$refo_perc_2013+df2$refo_perc_2014+df2$refo_perc_2015+df2$refo_perc_2016+df2$refo_perc_2017)
df2$defo_abs_13_17 <- (df2$defo_tot_2013+df2$defo_tot_2014+df2$defo_tot_2015+df2$defo_tot_2016+df2$defo_tot_2017)  
df2<- df2%>%as.data.frame()%>% dplyr::select(COD_IMOVEL,defo_abs_13_17,defo_perc_wo_refo_13_17)

df <- merge(df,df2, by = "COD_IMOVEL", all.x = TRUE)

# Process buffer forest data for 2013
buffer_forest2013 <- st_read('data/13_CAR_spatial_infrence_merged/LUC_selected_car_buffer_2013_2014.geojson')%>%as.data.frame() %>%
  filter(LUC_class_from == "Forest Formation", LUC_class_to == "Forest Formation") %>%
  dplyr::select(cod_imovel, buffer_forest_perc_2013 = area_perc, buffer_forest_tot_2013 = sum_area) %>%
  mutate(buffer_forest_tot_2013 = buffer_forest_tot_2013 * 0.0001) %>%
  dplyr::rename(COD_IMOVEL = cod_imovel) %>%
  mutate(buffer_forest_perc_2013 = buffer_forest_perc_2013, buffer_forest_tot_2013 = round(buffer_forest_tot_2013, 4))

df <- merge(df, buffer_forest2013, by = "COD_IMOVEL", all.x = TRUE)


# Process deforestation buffer data

process_luc_selected_car_buffer <- function(year_range, class_from, class_to, target_df, LUC_type) {
  result <- st_read(paste0('data/13_CAR_spatial_infrence_merged/LUC_selected_car_buffer_', year_range, '.geojson')) %>% as.data.frame() %>% 
    dplyr::select(cod_imovel,LUC_class_from,LUC_class_to,sum_area,area_perc)%>%
    filter(LUC_class_from == class_from, LUC_class_to == class_to) %>%
    group_by(cod_imovel) %>%
    dplyr::summarise(!!paste0(LUC_type,'_perc_', year_range) := area_perc,!!paste0(LUC_type,'_tot_', year_range) := round(sum_area * 0.0001, 4)) %>%
    dplyr::select(COD_IMOVEL = cod_imovel, !!paste0(LUC_type,'_tot_', year_range),!!paste0(LUC_type,'_perc_', year_range))
  
  target_df <- merge(target_df, result, by = "COD_IMOVEL", all.x = TRUE)
  return(target_df)
}
df3<-df%>%dplyr::select(COD_IMOVEL)
defo_years <- c("2013_2014", "2014_2015", "2015_2016", "2016_2017", "2017_2018")
for (year in defo_years) {
  df3 <- process_luc_selected_car_buffer(year, "Forest Formation", "Pasture", df3, 'defo_buffer')
}
df3 <- df3%>%replace(is.na(.), 0)
df3$defo_buffer_perc_13_17 <- (df3$defo_buffer_perc_2013+df3$defo_buffer_perc_2014+df3$defo_buffer_perc_2015+df3$defo_buffer_perc_2016+df3$defo_buffer_perc_2017) #- (df3$refo_perc_2013+df3$refo_perc_2014+df3$refo_perc_2015+df3$refo_perc_2016+df3$refo_perc_2017)
df3$defo_buffer_abs_13_17 <- (df3$defo_buffer_tot_2013+df3$defo_buffer_tot_2014+df3$defo_buffer_tot_2015+df3$defo_buffer_tot_2016+df3$defo_buffer_tot_2017)  
df3<- df3%>%as.data.frame()%>% dplyr::select(COD_IMOVEL,defo_buffer_abs_13_17,defo_buffer_perc_13_17)

df <- merge(df,df3, by = "COD_IMOVEL", all.x = TRUE)


################################
#'## CHECK CHECK CHECK all land uses for various CAR
################################

# plot <- df[50,]
# plot$centroid <- plot$geometry%>% st_centroid()
# 
# register_google(key='AIzaSyAdYd-PCQ6rGSAyseKrsUO-vdzVp4Ux3Bw')
# g <- get_googlemap(center = c(plot$centroid[[1]][1], plot$centroid[[1]][2]), maptype = "satellite", zoom=16)
# 
# ggmap(g) + 
#   theme_light() +
#   #geom_sf(data=r3_poly, aes(fill=transition_2018_2019),alpha=0.40,inherit.aes = FALSE)+
#   #scale_fill_manual(name='Land use',values=c(cbPalette[4], cbPalette[2],cbPalette[5],cbPalette[3]) )+
#   #inset_raster(as.raster(r3),-62.60494, -62.58931, -10.48011, -10.46501 )+
#   geom_sf(data = plot$geometry, fill = NA, inherit.aes = FALSE,lwd=1)
# 

################################
#'## Precipitation
################################
all_car_ncattle_lu2019_lucin3ybuffer_deg<-df
# precipitation
perc <- read.csv2('data/16_abiotic/precipitation_muns_2018_2019.csv',sep= ',')%>% dplyr::rename(IBGE_CODE = CD_MUN) %>% mutate(IBGE_CODE=as.character(IBGE_CODE),precipitation_2018_19 = as.double(precipitation))%>%
  dplyr::select(IBGE_CODE, precipitation_2018_19)
    
all_car_ncattle_perc <- all_car_ncattle_lu2019_lucin3ybuffer_deg %>% left_join(perc, by='IBGE_CODE')

################################
#'## Temperature
################################
temp <- read.csv2('data/16_abiotic/temp_munis_2018_2019.csv',sep= ',')%>% dplyr::rename(IBGE_CODE = CD_MUN) %>% mutate(IBGE_CODE=as.character(IBGE_CODE),mean_temp_18_19 = as.double(mean_temp_18_19))%>%
  dplyr::select(IBGE_CODE, mean_temp_18_19)

all_car_ncattle_perc_tem <- all_car_ncattle_perc %>% left_join(temp, by='IBGE_CODE')

################################
#'## ABC
################################
# include credit on the muni level
a<-read.csv('data/15_ABC/cust_all_allactividades_fno_2013_2017.csv') %>% dplyr::select(IBGE_CODE,abc_inv_all_2013_2017,credits_val_2013_2017)

a$agricultural_credits<-a$abc_inv_all_2013_2017/a$credits_val_2013_2017
a$IBGE_CODE<-as.character(a$IBGE_CODE)
all_car_ncattle_perc_tem<-all_car_ncattle_perc_tem%>% left_join(a,by='IBGE_CODE')


################################
#'## Population number 
################################
# include population number
library(readxl)
pop_density<- read.xlsx('data/00_stockingrates_census/POP2019_20220905.xls',sheetIndex = 2,startRow = 2)
pop_density$population <-as.numeric(pop_density$POPULAÇÃO.ESTIMADA)

pop_density$IBGE_CODE<-paste0(pop_density$COD..UF,pop_density$COD..MUNIC)
pop_density<- pop_density%>% dplyr::select(IBGE_CODE,population)
all_car_ncattle_perc_tem<- all_car_ncattle_perc_tem%>%left_join(pop_density,by='IBGE_CODE')

all_car_ncattle_perc_tem = all_car_ncattle_perc_tem[,!(names(all_car_ncattle_perc_tem) %in% c('geometry.x.x','geometry.x','geometry.x.y'))]
################################
#'## Open street map - distance to highways
################################


all_car_ncattle_perc_tem$centroids <- all_car_ncattle_perc_tem$geometry%>% st_centroid()
# find a way to calc center points of the CAR

# key = 'highway' or 
states<- c('Acre, Brazil', 'Amazonas, Brazil', 'Para, Brazil', 'Rondonia, Brazil')
state<-'Acre, Brazil'
streets <-function(state, federal=TRUE){
  bb<- getbb(state)
  
  tr <- opq(bb, timeout=100 ) %>%add_osm_feature(key = 'highway') %>%osmdata_sf(quiet = T)
  trr <- tr$osm_lines
  if(federal == TRUE){
    trr <- st_transform(trr, crs = 4674) %>% filter(trr$`IBGE:CD_ADMINIS`  == 'federal') %>%
      dplyr::select(name,`IBGE:CD_ADMINIS`,highway,ref)
  }else{
    #st_crs(trr) <- 4674 
    trr <- st_transform(trr, crs = 4674) %>% #filter(trr$`IBGE:CD_ADMINIS`  %in% c('federal','primary','tertiary','secondary')) %>%
      dplyr::select(name,`IBGE:CD_ADMINIS`,highway,ref)
  }
  return(trr)
}


hways<- lapply(states, streets) %>% bind_rows()

all_car_ncattle_perc_tem$nearest_fed_road<-sapply(1:nrow(all_car_ncattle_perc_tem), 
             function(i){
               min(
                 st_distance(
                   all_car_ncattle_perc_tem$centroids[i],
                   hways)
                 
               )
             }
)

length(unique(all_car_ncattle_perc_tem$COD_IMOVEL))

################################
#'## Distance to Slaughterhouses
################################

sh_loc <- read.csv2('data/14_distances/2021-02-25-br_beef_logistics_map_v2.csv') %>% drop_na(LONG)%>% st_as_sf(coords=c('LONG','LAT'), crs=4674)
sifs <- sh_loc %>% filter(INSPECTION_LEVEL=='SIF', STATE %in% c('AC','AM','PA','RO'), STATUS == "ATIVO") 
sifs <- st_transform(sifs, 4674)
dist <- st_distance(all_car_ncattle_perc_tem$centroids, sifs)
all_car_ncattle_perc_tem$nearest_fedSh <- apply(dist,1,min)

# include also SH form other states
#all_sh <- sh_loc %>% filter(STATE %in% c('AC','AM','PA','RO'), STATUS == "ATIVO")
#dist <- st_distance(all_car_ncattle_perc_tem$centroids, all_sh)
#all_car_ncattle_perc_tem$nearest_Sh <- apply(dist,1,min)

st_write(all_car_ncattle_perc_tem,"data/regression_pars_final_raw.geojson")




