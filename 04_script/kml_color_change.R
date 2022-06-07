rm(list = ls())
library(st)
library(maptools)

################### IDP ################################################
IDPs_all_point <- st_read("03_outputs/01_sampling/03_sample_point/02_IDPs/02_out_camp/total/all_point.kml")
IDPs_all_point$Name <- IDPs_all_point$Name %>% str_replace_all("PT_","PT-")
IDPs_all_point <- IDPs_all_point %>% mutate(
  point_number = sub("_.*", "", Name) %>% str_replace_all("-","_"),
  governorate = sub(".*_", "", Name),
  district = str_extract(Name,"(?<=_).*(?=_)"),
)


for ( i in unique(IDPs_all_point$district)){
  district_df <- IDPs_all_point %>% filter(district == i)
  gov <- district_df$governorate  %>% unique()
  path <- paste0("03_outputs/01_sampling/03_sample_point/02_IDPs/02_out_camp/",gov,"/",i,"/OUT_CAMP_",snakecase::to_snake_case(i),".kml")
  print(path)
  kmlPoints(obj = as_Spatial(district_df),kmlfile = path)
  as_text <- readLines(path)
  tx2 <- gsub(pattern = "#style1", replace = "#placemark-purple", x = as_text)
  writeLines(tx2, con=path)
}


################# RETURNEES ##########################
rm(list = ls())

returnees_all_point <- st_read("03_outputs/01_sampling/03_sample_point/03_Returnees/total/all_points.kml")
returnees_all_point$Name <- returnees_all_point$Name %>% str_replace_all("PT_","PT-")
returnees_all_point <- returnees_all_point %>% mutate(
  point_number = sub("_.*", "", Name) %>% str_replace_all("-","_"),
  governorate = sub(".*_", "", Name),
  district = str_extract(Name,"(?<=_).*(?=_)"),
)

for ( i in unique(returnees_all_point$district)){
  district_df <- returnees_all_point %>% filter(district == i)
  gov <- district_df$governorate  %>% unique()
  path <- paste0("03_outputs/01_sampling/03_sample_point/03_Returnees/",gov,"/",i,"/retun_",snakecase::to_snake_case(i),".kml")
  print(path)
  kmlPoints(obj = as_Spatial(district_df),kmlfile = path)
  as_text <- readLines(path)
  tx2 <- gsub(pattern = "#style1", replace = "#placemark-blue", x = as_text)
  writeLines(tx2, con=path)
  
}


