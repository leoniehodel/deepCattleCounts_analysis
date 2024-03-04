library(sf)
library(tictoc)
library(tidyverse)
library(furrr)

################################
#'## download CAR into raw_data_secondary
################################

#data for RO from https://www.car.gov.br/publico/municipios/downloads?sigla=RO
#data for AC from https://www.car.gov.br/publico/municipios/downloads?sigla=AC
#data for PA from https://www.car.gov.br/publico/municipios/downloads?sigla=PA
#data for AM from https://www.car.gov.br/publico/municipios/downloads?sigla=AM

# --> put in data_raw_secondary/CAR/01_raw/

# output files
outputs<- 'data_raw_secondary/CAR/02_clean/'

################################
#'## clean data
################################
AC<- st_read('data_raw_secondary/CAR/01_raw/AC/AREA_IMOVEL_1.shp')
AC$IBGE_CODE <- substr(AC$COD_IMOVEL,4,10)
AC$seq<- rep(seq_len(nrow(AC)))
st_write(AC,'data/04_CAR/02_clean/AC_CAR.geojson')

PA<- st_read('data_raw_secondary/CAR/01_raw/PA/AREA_IMOVEL_1.shp')
PA$IBGE_CODE <- substr(PA$COD_IMOVEL,4,10)
PA$seq<- rep(seq_len(nrow(PA)))
st_write(PA,'data/04_CAR/02_clean/PA_CAR.geojson')

RO<- st_read('data_raw_secondary/CAR/01_raw/RO/AREA_IMOVEL_1.shp')
RO$IBGE_CODE <- substr(RO$COD_IMOVEL,4,10)
RO$seq<- rep(seq_len(nrow(RO)))
st_write(RO,'data/04_CAR/02_clean/RO_CAR.geojson')

AM<- st_read('data_raw_secondary/CAR/01_raw/AM/AREA_IMOVEL_1.shp')
AM$IBGE_CODE <- substr(AM$COD_IMOVEL,4,10)
AM$seq<- rep(seq_len(nrow(AM)))
st_write(AM,'data/04_CAR/02_clean/AM_CAR.geojson')

