rm(list = ls())

library(dplyr)
library(st)
library(sf)
library(stringr)

# IDP  --------------------------------------------------------------------

file_list <- list.files("03_outputs/01_sampling/03_sample_point/02_IDPs/02_out_camp",pattern = ".kml",full.names = T,recursive = T)

file_list2 <- file_list[!grepl("total|OUT_CAMP|NEW|999_Archived|all_ba|IDP_OUT",file_list)]

all_point <- list()

for(i in file_list2){
  kml <- st_read(i)
  rm_comn_path <- str_replace_all(i,"03_outputs/01_sampling/03_sample_point/02_IDPs/02_out_camp/","")
  split <- str_split(rm_comn_path,"/")
  gov <- split[[1]][1] 
  dis <- split[[1]][2]
  location <- split[[1]][3]
  kml <- kml %>% mutate(
    Name = paste0(dis," [",location,"]"," IDP_OUT"),
    governorate = gov,
    district = dis
  )
  all_point[[location]] <- kml
}

all_point <- do.call("bind_rows",all_point)


for (i in unique(all_point$district)) {
  dis_df <- all_point %>% filter(district == i)
  gover <- dis_df$governorate %>% unique()
  
  st_write(dis_df,
           paste0("03_outputs/01_sampling/03_sample_point/02_IDPs/02_out_camp/",gover,"/",i,
                  "/[updated]",snakecase::to_snake_case(i),"_",nrow(dis_df),".kml"),append = F,delete_dsn = T,delete_layer = T)
}




# retunr  --------------------------------------------------------------------

returnees_all_point <- st_read("03_outputs/01_sampling/03_sample_point/03_Returnees/total/all_points.kml")

file_list <- list.files("03_outputs/01_sampling/03_sample_point/03_Returnees",pattern = ".kml",full.names = T,recursive = T)

file_list2 <- file_list[!grepl("total|OUT_CAMP|retun_|rchived|all_ba|999_Archived[NOT_IN_USE]",file_list)]

all_point <- list()

for(i in file_list2){
  kml <- st_read(i)
  rm_comn_path <- str_replace_all(i,"03_outputs/01_sampling/03_sample_point/03_Returnees/","")
  split <- str_split(rm_comn_path,"/")
  gov <- split[[1]][1] 
  dis <- split[[1]][2]
  location <- split[[1]][3]
  print(location)
  kml <- kml %>% mutate(
    Name = paste0(dis," [",location,"]"," RETURNEES"),
    governorate = gov,
    district = dis,
    location = location,
    place_id = sub(".*_","",location)
  )
  all_point[[location]] <- kml
}

all_point <- do.call("bind_rows",all_point)

# count <- all_point %>% group_by(place_id) %>% summarise(
#   count = n()
# ) %>% mutate_all(as.character)
# 
# 
# 
# sample_fram_re <- read.csv("03_outputs/01_sampling/02_sampling/03_Returnees/sampling_frame.csv")  %>% mutate(place_id = place_id %>% as.character())%>% left_join(count)
# 
# 
# sample_fram_re <- sample_fram_re %>% mutate(
#   tf = survey_buffer == count
# )
# sample_fram_re$tf %>% table()
# 



for (i in unique(all_point$district)) {
  dis_df <- all_point %>% filter(district == i)
  gover <- dis_df$governorate %>% unique()
  
  st_write(dis_df,
           paste0("03_outputs/01_sampling/03_sample_point/03_Returnees/",gover,"/",i,
                  "/[updated]",snakecase::to_snake_case(i),"_",nrow(dis_df),".kml"),append = F,delete_dsn = T,delete_layer = T)
}






