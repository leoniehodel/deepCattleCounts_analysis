library(sf)
library(tidyverse)

################################
#'## Read data
################################

selected_car <- st_read('data/08_CAR-outlinesmerged/all_selected_car.geojson') %>%
  st_make_valid()
selected_car$area <- st_area(selected_car)
outlines <- st_read('data/all_outlines_sf_jan23.geojson')

# Check unique cod_imovel
length(selected_car$cod_imovel)
length(unique(selected_car$cod_imovel))

################################
#'## Process data: count cattle per CAR
################################

#list of all inferences 
inf_dir <- 'data/inference_adam_lr5_ens5/'
name_output <- 'car_withcattle1_adam_lr5_ens5.geojson'

inference_list <- list.files(inf_dir)
all_car_with_cattle <- data.frame()
cor_mean <-c()
cor_cattlenumber <-c()

for(inf in inference_list){
  
  #find id to compare with date_table
  id = strsplit(inf, "[.]")[[1]][1]
  date <- outlines$date[outlines$id == id]

  # the geopoints are stored in EPGS:4674 already, just have to be assigned
  geopoints <- st_read(file.path(inf_dir,inf)) %>% st_set_crs(4674) %>%
    mutate(n_cattle = floor(n_cattle))
  binary <-st_covered_by(selected_car, outlines[outlines$id==id,])
  a = as.vector.data.frame(binary)
  
  car_select <- selected_car %>%  mutate(covered = ifelse(a>0,1,0)) %>% 
    filter(covered!=0) 
  
  # If no car is selected, skip to the next inference file
  if (nrow(car_select) == 0) {
    cat('no car in this outline..')
    next
  }
  
  
  r <- relate(vect(car_select), vect(geopoints), "intersects")
  car_select$n_cattle  <- apply(r, 1, function(i) sum(geopoints$n_cattle[i]))
  
  # take the mean of the sd, so the sd is independent of the number of geopoints (independent of area)
  print(car_select$area)
  
  car_select$n_cattle_sd <- apply(r, 1, function(i) mean((geopoints$n_cattle_sd[i])))
  car_select$n_cattle_mean <- apply(r, 1, function(i) mean((geopoints$n_cattle[i])))

  #check the correlation of the standard deviation and cattle number and area of the property
  cor_cattlenumber <- c(cor_cattlenumber, cor(car_select$n_cattle, car_select$n_cattle_sd))
  cor_mean <- c(cor_mean, cor(car_select$n_cattle_mean, car_select$n_cattle_sd))
  
  car_select$date <- date
  car_select$outline_id <- id
  
  #add the car to all_car_with_cattle
  all_car_with_cattle <- rbind(all_car_with_cattle,car_select)
}

sum(all_car_with_cattle$n_cattle)
#[1] 151247.8


omitzeros<- all_car_with_cattle%>%filter(round(n_cattle)>=1)

# find ground sampling distance
st_distance(geopoints[1,], geopoints[2,])

st_write(omitzeros,paste0('data/',name_output), append=FALSE)
length(omitzeros$cod_imovel)
length(unique(omitzeros$cod_imovel))
