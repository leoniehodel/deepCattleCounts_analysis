library(sf)
library(tidyverse)

################################
#'## Read in data
################################
outlines <- st_read('data/02_satellite_images/all_outlines_sf_jan23.geojson')
#write.csv(outlines, "analysis_2018-19/working_ds/outlines_table.csv", row.names = FALSE)
car_AC <- st_read('data/04_CAR/02_clean/AC_CAR_cln_dec22.geojson')
car_PA <- st_read('data/04_CAR/02_clean/PA_CAR.geojson')
car_RO <- st_read('data/04_CAR/02_clean/RO_CAR.geojson')
car_AM <- st_read('data/04_CAR/02_clean/AM_CAR.geojson')

################################
#'## Calculate area and save in log file
################################
binary_AC <-st_covered_by(car_AC, outlines)
a = as.vector.data.frame(binary_AC)
car_AC_select <- car_AC %>%  mutate(covered = ifelse(a>0,1,0)) %>% filter(covered!=0) %>% select(seq,cod_imovel,IBGE_CODE) %>% mutate(SIGLA_UF = 'AC')

#write_rds(car_AC_select,'data/08_CAR-outlinesmerged/car_AC_select.rds')

binary_PA <-st_covered_by(car_PA, outlines)
a = as.vector.data.frame(binary_PA)
car_PA_select <- car_PA %>%  mutate(covered = ifelse(a>0,1,0)) %>% filter(covered!=0) %>% select(seq,'cod_imovel'=COD_IMOVEL,IBGE_CODE)%>% mutate(SIGLA_UF = 'PA')
#write_rds(car_PA_select,'data/08_CAR-outlinesmerged/car_PA_select.rds')

binary_RO <-st_covered_by(st_make_valid(car_RO), outlines)
a = as.vector.data.frame(binary_RO)
car_RO_select <- car_RO %>%  mutate(covered = ifelse(a>0,1,0)) %>% filter(covered!=0) %>% select(seq,'cod_imovel'=COD_IMOVEL,IBGE_CODE)%>% mutate(SIGLA_UF = 'RO')
#write_rds(car_RO_select,'data/08_CAR-outlinesmerged/car_RO_select.rds')

binary_AM <-st_covered_by(st_make_valid(car_AM), outlines)
a = as.vector.data.frame(binary_AM)
car_AM_select <- car_AM %>%  mutate(covered = ifelse(a>0,1,0)) %>% filter(covered!=0) %>% select(seq,'cod_imovel'=COD_IMOVEL,IBGE_CODE)%>% mutate(SIGLA_UF = 'AM')


# combine all car
all_selected_car <- rbind(car_AC_select,car_PA_select,car_RO_select,car_AM_select)
  
st_write(all_selected_car,'data/08_CAR-outlinesmerged/all_selected_car.geojson')

