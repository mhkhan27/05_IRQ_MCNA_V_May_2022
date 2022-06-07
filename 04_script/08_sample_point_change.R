rm(list = ls())

library(dplyr)
library(sf)
library(st)
library(stringr)
library(maptools)


water<- st_read("01_inputs/03_shapefile/irq_water_bodies_dis.shp")  %>% st_transform(4326)
non_liveable <- st_read("01_inputs/03_shapefile/non_liveable_final_dis.shp") 

pop_group <- c("out_idp","returnees")[2]

if(pop_group == "out_idp"){
# out camp IDP ---------------------------------------------------------------------

all_point <- st_read("03_outputs/01_sampling/03_sample_point/02_IDPs/02_out_camp/total/Archived_not_in_use/all_point.kml")
all_point_without_baaj <- all_point %>% filter(!grepl("Al-Baaj",Name))
new_sample_frame <- read.csv( "03_outputs\\01_sampling\\02_sampling\\02_IDPs\\01_out_camp/sampling_frame.csv") %>% filter(district == "Al-Baaj")

sample_fm_sf <- new_sample_frame %>% st_as_sf(coords =c("longitude","latitude"),crs = 4326) %>% st_transform(32638)

sample_bffer<- st_buffer(sample_fm_sf,500)
sample_bffer <- sample_bffer %>% st_transform(4326)


old_sample_frame <- read.csv( "03_outputs\\01_sampling\\02_sampling\\02_IDPs\\01_out_camp/Archive_not_in_used/sampling_frame.csv") %>% filter(location_name_in_english == "Al-Risalah")
old_sample_frame_sf <- old_sample_frame %>% st_as_sf(coords =c("longitude","latitude"),crs = 4326) %>% st_transform(32638)

risala_buff <- old_sample_frame_sf %>% st_buffer(300) %>% st_transform(4326)


sample_bffer_remove_non_liveable <- rmapshaper::ms_erase(sample_bffer,non_liveable)
sample_bffer_final <- rmapshaper::ms_erase(sample_bffer_remove_non_liveable,water)
sample_bffer_final <- rmapshaper::ms_erase(sample_bffer_final,risala_buff)

sample_bffer_final$location_name_in_english <-  sample_bffer_final$location_name_in_english   %>% trimws() %>% str_replace_all(" / ","_") %>% str_replace_all("/","_")
sample_bffer_final$location_name_in_english <-  sample_bffer_final$location_name_in_english   %>% str_replace_all("-","_")
sample_bffer_final$local_and_id <- paste0(sample_bffer_final$location_name_in_english,"_",sample_bffer_final$place_id)



sample_point_all<- list()

for (i in sample_bffer_final$place_id) {
  area <- sample_bffer_final %>% filter(place_id == i)
  
  gov <- area$governorate
  dis <- area$district
  location <- area$location_name_in_english
  
  sample_point_all[[i]] <-  area %>% st_sample(area$survey_buffer) %>% st_as_sf() %>% 
    mutate(place_id = i,
           governorate = gov,
           district= dis,
           location_name_in_english = location,
           location_pt_id = row_number())
}


all_point <- do.call("bind_rows",sample_point_all)

sample_points <- all_point %>% mutate(
  Name = paste0("PT_",location_pt_id,"_",district,"_",governorate),
  local_and_id = paste0(location_name_in_english,"_",place_id)
)
sample_points$district <- sample_points$district %>% trimws()
sample_points$local_and_id <- sample_points$local_and_id %>% trimws() %>% str_replace_all(" / ","_")  %>% str_replace_all("/","_")  %>% str_replace_all("-","_")

st_write(sample_points,"03_outputs\\01_sampling\\03_sample_point\\02_IDPs\\02_out_camp/Ninewa/Al-Baaj/all_baaj_updated.kml",append = F,delete_dsn = T,delete_layer = T)

path <- "03_outputs\\01_sampling\\03_sample_point\\02_IDPs\\02_out_camp/Ninewa/Al-Baaj/all_baaj_updated.kml"
dis_kml <- st_read(path)
unlink(path, recursive = TRUE)
kmlPoints(obj = as_Spatial(dis_kml),kmlfile = path)
as_text <- readLines(path)
tx2 <- gsub(pattern = "#style1", replace = "#placemark-purple", x = as_text)
writeLines(tx2, con=path)


################################ Write by location ####################################

for (i in unique(sample_points$local_and_id)) {
  
  location_points <- sample_points %>% filter(local_and_id == i)
  file_name <- paste0(i,"_svy_",nrow(location_points))
  st_write(location_points,paste0( "03_outputs\\01_sampling\\03_sample_point\\02_IDPs\\02_out_camp\\Ninewa\\Al-Baaj/",i,"/",file_name,".kml"),append = F,delete_dsn = T,delete_layer = T)
  
}

#############################

sample_points_format <- sample_points %>% select(Name)


total_point <- all_point_without_baaj %>% bind_rows(sample_points_format)
st_write(total_point,"03_outputs\\01_sampling\\03_sample_point\\02_IDPs\\02_out_camp/total/all_point.kml",append = F,delete_dsn = T,delete_layer = T)





}

################################################################## RETURNEES ######################################################
if(pop_group == "returnees"){
  # Returnees ---------------------------------------------------------------------
  
  all_point <- st_read("03_outputs/01_sampling/03_sample_point/03_Returnees/total/archived_not_in_used/all_points.kml")
  all_point_without_baaj <- all_point %>% filter(!grepl("Al-Baaj",Name))
  
  new_sample_frame <- read.csv( "03_outputs\\01_sampling\\02_sampling\\03_Returnees/sampling_frame.csv") %>% filter(district == "Al-Baaj")
  
  sample_fm_sf <- new_sample_frame %>% st_as_sf(coords =c("longitude","latitude"),crs = 4326) %>% st_transform(32638)
  
  sample_bffer<- st_buffer(sample_fm_sf,500)
  sample_bffer <- sample_bffer %>% st_transform(4326)
  
  
  old_sample_frame <- read.csv( "03_outputs\\01_sampling\\02_sampling\\03_Returnees/archived_not_in_use/sampling_frame.csv") %>% filter(location_name_in_english == "Al-Risalah")
  old_sample_frame_sf <- old_sample_frame %>% st_as_sf(coords =c("longitude","latitude"),crs = 4326) %>% st_transform(32638)
  
  risala_buff <- old_sample_frame_sf %>% st_buffer(300) %>% st_transform(4326)
  
  
  sample_bffer_remove_non_liveable <- rmapshaper::ms_erase(sample_bffer,non_liveable)
  sample_bffer_final <- rmapshaper::ms_erase(sample_bffer_remove_non_liveable,water)
  sample_bffer_final <- rmapshaper::ms_erase(sample_bffer_final,risala_buff)
  
  sample_bffer_final$location_name_in_english <-  sample_bffer_final$location_name_in_english   %>% trimws() %>% str_replace_all(" / ","_") %>% str_replace_all("/","_")
  sample_bffer_final$location_name_in_english <-  sample_bffer_final$location_name_in_english   %>% str_replace_all("-","_")
  sample_bffer_final$local_and_id <- paste0(sample_bffer_final$location_name_in_english,"_",sample_bffer_final$place_id)
  
  
  
  sample_point_all<- list()
  
  for (i in sample_bffer_final$place_id) {
    area <- sample_bffer_final %>% filter(place_id == i)
    
    gov <- area$governorate
    dis <- area$district
    location <- area$location_name_in_english
    
    sample_point_all[[i]] <-  area %>% st_sample(area$survey_buffer) %>% st_as_sf() %>% 
      mutate(place_id = i,
             governorate = gov,
             district= dis,
             location_name_in_english = location,
             location_pt_id = row_number())
  }
  
  
  all_point <- do.call("bind_rows",sample_point_all)
  
  sample_points <- all_point %>% mutate(
    Name = paste0("PT_",location_pt_id,"_",district,"_",governorate),
    local_and_id = paste0(location_name_in_english,"_",place_id)
  )
  sample_points$district <- sample_points$district %>% trimws()
  sample_points$local_and_id <- sample_points$local_and_id %>% trimws() %>% str_replace_all(" / ","_")  %>% str_replace_all("/","_")  %>% str_replace_all("-","_")
  
  st_write(sample_points,"03_outputs\\01_sampling\\03_sample_point\\03_Returnees/Ninewa/Al-Baaj/all_baaj_updated.kml",append = F,delete_dsn = T,delete_layer = T)
  
  path <- "03_outputs\\01_sampling\\03_sample_point\\03_Returnees/Ninewa/Al-Baaj/all_baaj_updated.kml"
  dis_kml <- st_read(path)
  unlink(path, recursive = TRUE)
  kmlPoints(obj = as_Spatial(dis_kml),kmlfile = path)
  as_text <- readLines(path)
  tx2 <- gsub(pattern = "#style1", replace = "#placemark-blue", x = as_text)
  writeLines(tx2, con=path)
  
  
  
  ################################ Write by location ####################################
  
  for (i in unique(sample_points$local_and_id)) {
    
    location_points <- sample_points %>% filter(local_and_id == i)
    file_name <- paste0(i,"_svy_",nrow(location_points))
    st_write(location_points,paste0( "03_outputs\\01_sampling\\03_sample_point\\03_Returnees\\Ninewa\\Al-Baaj/",i,"/",file_name,".kml"),append = F,delete_dsn = T,delete_layer = T)
    
  }
  
  #############################
  
  sample_points_format <- sample_points %>% select(Name)
  
  
  total_point <- all_point_without_baaj %>% bind_rows(sample_points_format)
  st_write(total_point,"03_outputs\\01_sampling\\03_sample_point\\03_Returnees/total/all_points.kml",append = F,delete_dsn = T,delete_layer = T)
  
  
  
  
  
}
