library(sf)
library(tictoc)
library(tidyverse)
library(furrr)

# unzip RO data
all_car_mun_RO <- tibble(full_path = list.files("data/04_CAR/01_raw/RO_rawdownload/", full.names = TRUE, recursive = TRUE,pattern = "AREA_IMOVEL.zip$" )) %>% 
  mutate( stringi = stringr::str_split(full_path,'/')) %>% mutate(name_out = map_chr(stringi, 6)) %>%
  mutate(path_out = paste0("data/04_CAR/01_raw/RO/",name_out ,'/' ))%>% select(full_path,path_out)

unzip(all_car_mun_RO$full_path[1], exdir=all_car_mun_RO$path_out[1]) 
walk2(all_car_mun_RO$full_path, 
      all_car_mun_RO$path_out,
      ~unzip(.x, exdir=.y))

# RO data aggregation

CAR_all <- tibble(full_path = list.files("data/04_CAR/01_raw/RO/", full.names = TRUE,pattern = ".shp$", recursive = TRUE))
shp_1 <- st_read(CAR_all$full_path[1])

### Read ALL shp
CAR_data <- CAR_all %>% 
  mutate(data = future_map(full_path, st_read, quiet=TRUE, check_ring_dir = TRUE) ) %>% 
  select(-full_path)

df_sf = reduce(CAR_data, rbind)
df = reduce(df_sf, rbind)

df$IBGE_CODE <- substr(df$COD_IMOVEL,4,10)
df$seq<- rep(seq_len(nrow(df)))

st_write(df,'data/04_CAR/02_clean/RO_CAR.geojson')


# PA data cleaning

CAR_PA_data<- readRDS('data/04_CAR/01_raw/PA_/car_spatial.rds') %>% as.data.frame()
CAR_PA <- st_read('data/04_CAR/01_raw/PA_/CAR_ungrouped.geojson')%>%
  left_join(CAR_PA_data,by='COD_PROTOCOLO') %>% select(COD_IMOVEL,  NOM_COMPLETO_CADASTRANTE,
                                                       IBGE_CODE = IDT_MUNICIPIO)
CAR_PA$seq<- rep(seq_len(nrow(CAR_PA)))
CAR_PA$NUM_AREA <- st_area(CAR_PA)
st_write(CAR_PA,'data/04_CAR/02_clean/PA_CAR.geojson')

################################
#'## AC --> already clean
################################

CAR_AC_data<- st_read('data/04_CAR/02_clean/AC_CAR_cln.geojson')
CAR_AC_data$seq<- rep(seq_len(nrow(CAR_AC_data)))
CAR_AC_data <- CAR_AC_data %>% select(seq, cod_imovel, nom_imovel, nome_compl_cln, IBGE_CODE)
st_write(CAR_AC_data,'data/04_CAR/02_clean/AC_CAR_cln_dec22.geojson')

################################
#'## unzip AM data
################################
all_car_mun_AM <- tibble(full_path = list.files("data/04_CAR/01_raw/AM_rawdownload/", full.names = TRUE, recursive = TRUE,pattern = "AREA_IMOVEL.zip$" )) %>% 
  mutate( stringi = stringr::str_split(full_path,'/')) %>% mutate(name_out = map_chr(stringi, 6)) %>%
  mutate(path_out = paste0("data/04_CAR/01_raw/AM/",name_out ,'/' ))%>% select(full_path,path_out)

#unzip(all_car_mun_AM$full_path[1], exdir=all_car_mun_AM$path_out[1]) 
walk2(all_car_mun_AM$full_path, 
      all_car_mun_AM$path_out,
      ~unzip(.x, exdir=.y))

################################
#'## AM data aggregation
################################

CAR_all <- tibble(full_path = list.files("data/04_CAR/01_raw/AM/", full.names = TRUE,pattern = ".shp$", recursive = TRUE))
shp_1 <- st_read(CAR_all$full_path[1])

### Read ALL shp
CAR_data <- CAR_all %>% 
  mutate(data = future_map(full_path, st_read, quiet=TRUE, check_ring_dir = TRUE) ) %>% 
  select(-full_path)

df_sf = reduce(CAR_data, rbind)
df = reduce(df_sf, rbind)

df$IBGE_CODE <- substr(df$COD_IMOVEL,4,10)
df$seq<- rep(seq_len(nrow(df)))

#fix geometry & drop null geometries
CAR_r <- df %>% st_make_valid()
CAR_rr <- CAR_r[!st_is_empty(CAR_r),]

st_write(df,'data/04_CAR/02_clean/AM_CAR.geojson')

