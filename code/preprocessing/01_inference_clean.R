library(sf)
library(tidyverse)

################################
#'## Read data
################################

inf_dir <- 'data/inference_adam_lr5_ens5/'
inf_dir_clean <- 'data/inference_adam_lr5_ens5_clean/'
name_output <- 'data/all_inference_adam_lr5_ens5.geojson'


# Create directory if it doesn't exist
if (!dir.exists(inf_dir_clean)) {
  dir.create(inf_dir_clean, recursive = TRUE)
}

# Read outline data
all_outlines <- st_read('data/02_satellite_images/image_outlines.geojson') %>% 
  as.data.frame()


################################
# Add date to files
################################
add_date <- function(id_bbox){
  print(id_bbox)
  filepath = list.files(inf_dir,pattern = id_bbox, all.files=TRUE, full.names=TRUE)
  print(filepath)
  date <- all_outlines$date[all_outlines$id==id_bbox]
  print(date)
  
  if(identical(filepath, character(0))){
    cat(paste0(id,' is not in the dir, maybe does not exist in inference database'))}
  else{
    temp <-st_read(filepath) %>% 
      mutate(date = date, id_bbox=id_bbox)
    st_write(temp,paste0(inf_dir_clean,id_bbox,'.geojson'))
  }
} 
 
lapply(all_outlines$id, add_date)

# Read all inference data
all_inf_paths <- tibble(full_path = list.files(inf_dir_clean,full.names = TRUE)) 
all_inference <- do.call(rbind, lapply(all_inf_paths$full_path, function(x) st_read(x)))

# Convert id_bbox to factor
all_inference$id_bbox <-as.factor(all_inference$id_bbox)

#check if all 170 img patches were processed
length(levels(as.factor(all_inference$id_bbox)))

# save a file with all inference data
# projection is already in 4674 form deep learning output
all_inference<- all_inference %>% st_set_crs(4674)
st_write(all_inference,name_output)



