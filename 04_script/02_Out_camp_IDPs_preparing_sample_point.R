rm(list = ls())


library(sf)
library(raster)
library(dplyr)
library(sp)
library(exactextractr)
library(lwgeom)
library(units)
library(leaflet)
library(rgeos)
library(rmapshaper)
library(stringr)
library(geosphere)
library(Rfast)
library(htmltools)
library(htmlwidgets)

create_directory <- c(T,F)[1]



# base_map ----------------------------------------------------------------

admin_path <- "G:\\My Drive\\01_Professional\\08_REACH_IRAQ_UPDATE\\mh1\\05_common_shapefiles\\admin_boundary/"
admin_zero <- st_read(paste0(admin_path,"/irq_admbnda_adm0_cso_itos_20190603.shp"))
admin1_boundary <- st_read(paste0(admin_path,"irq_admbnda_adm1_cso_20190603.shp"))
admin2_boundary<- st_read(paste0(admin_path,"irq_admbnda_adm2_cso_20190603.shp"))
admin3_boundary<- st_read(paste0(admin_path,"irq_admbnda_adm3_cso_20190603.shp"))

base_map <- leaflet::leaflet() %>% 
  leaflet::addProviderTiles(providers$Esri.WorldImagery,group = "ERSI World Imagery") %>% 
  leaflet::addProviderTiles(providers$OpenStreetMap.HOT,group = "OSM") %>% 
  leaflet::addPolygons(data = admin_zero,color = "#EE5859",fillColor = "transparent") %>%  
  
  leaflet::addPolygons(data = admin1_boundary,color = "#D2CBB8",
                       label = ~htmlEscape(ADM1_EN),
                       labelOptions = labelOptions(noHide = T,
                                                   direction = 'center',
                                                   textOnly = T,
                                                   style = list(
                                                     "font-family" = "serif",
                                                     "font-size" = "15px",
                                                     "font-weight" =  "bold"
                                                   )),
                       popup = paste("Governorate:", admin1_boundary$ADM1_EN),
                       weight = 3,fillColor = "transparent") %>% 
  
  leaflet::addPolygons(data = admin2_boundary,
                       color = "#58585A",
                       label = ~htmlEscape(ADM2_EN),
                       labelOptions = labelOptions(noHide = T,
                                                   direction = 'center',
                                                   textOnly = T,
                                                   style = list(
                                                     "font-family" = "serif",
                                                     "font-size" = "12px"
                                                   )),
                       
                       popup = paste("Governorate:", admin2_boundary$ADM1_EN, "<br>",
                                     "District:", admin2_boundary$ADM2_EN),
                       
                       weight = 1,fillColor = "transparent",group = "District") %>% 
  
  leaflet::addPolygons(data = admin3_boundary,
                       color = "#F69E61",
                       label = ~htmlEscape(ADM3_EN),
                       labelOptions = labelOptions(noHide = T,
                                                   direction = 'center',
                                                   textOnly = T,
                                                   style = list(
                                                     "font-family" = "Arial Narrow",
                                                     "font-size" = "10px",
                                                     "font-style" = "italic"
                                                   )),
                       popup = paste("Governorate:", admin3_boundary$ADM1_EN, "<br>",
                                     "District:", admin3_boundary$ADM2_EN, "<br>",
                                     "Sub-District:", admin3_boundary$ADM3_EN),
                       
                       weight = 1,dashArray = "9",
                       fillColor = "transparent",
                       group = "Sub-district") 

base_map <- addControlGPS(base_map, options = gpsOptions(position = "topleft", activate = TRUE, 
                                                         autoCenter = TRUE, maxZoom = 10, 
                                                         setView = F))
base_map <- activateGPS(base_map)


# read_dataset ------------------------------------------------------------

out_idp_sample_frame <- read.csv("03_outputs/01_sampling/02_sampling/02_IDPs/01_out_camp/sampling_frame.csv")
out_idp_sample_frame$district %>% unique()
sample_frame_sf <- out_idp_sample_frame %>% st_as_sf(coords = c("longitude","latitude"),crs = 4326)
sample_frame_sf <- sample_frame_sf %>% st_transform(crs = 32638)

# take buffer
sample_bffer <- sample_frame_sf %>% st_buffer(500)
sample_bffer <- sample_bffer %>% st_transform(4326)



water<- st_read("01_inputs/03_shapefile/irq_water_bodies_dis.shp")  %>% st_transform(4326)
non_liveable <- st_read("01_inputs/03_shapefile/non_liveable_final_dis.shp") 

sample_bffer_remove_non_liveable <- rmapshaper::ms_erase(sample_bffer,non_liveable)
sample_bffer_final <- rmapshaper::ms_erase(sample_bffer_remove_non_liveable,water)
sample_bffer_final$location_name_in_english <-  sample_bffer_final$location_name_in_english   %>% trimws() %>% str_replace_all(" / ","_") %>% str_replace_all("/","_")
sample_bffer_final$location_name_in_english <-  sample_bffer_final$location_name_in_english   %>% str_replace_all("-","_")
sample_bffer_final$local_and_id <- paste0(sample_bffer_final$location_name_in_english,"_",sample_bffer_final$place_id)

sf::sf_use_s2(FALSE)
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
  name = paste0("PT_",location_pt_id,"_",district,"_",governorate),
  local_and_id = paste0(location_name_in_english,"_",place_id)
)
sample_points$district <- sample_points$district %>% trimws()
sample_points$local_and_id <- sample_points$local_and_id %>% trimws() %>% str_replace_all(" / ","_")  %>% str_replace_all("/","_")  %>% str_replace_all("-","_")


# all points export -------------------------------------------------------


full_map <-  base_map %>% leaflet::addPolygons(data = sample_bffer_final,
                                               color = "#772B2C",
                                               label = ~htmlEscape(Survey),
                                               labelOptions = labelOptions(noHide = F,
                                                                           direction = 'center',
                                                                           textOnly = T,
                                                                           style = list(
                                                                             "font-family" = "Arial Narrow",
                                                                             "font-size" = "15px",
                                                                             "font-style" = "italic"
                                                                           )),
                                               popup = paste("Governorate:", sample_bffer_final$governorate, "<br>",
                                                             "District:", sample_bffer_final$district, "<br>",
                                                             "location:", sample_bffer_final$location_name_in_english, "<br>",
                                                             "Population:", sample_bffer_final$pop_numbers, "<br>",
                                                             "Survey_needed:", sample_bffer_final$Survey),
                                               
                                               weight = 2,dashArray = "9",
                                               fillColor = "transparent",
                                               group = "cluster") %>% 
  
  leaflet::addAwesomeMarkers(data =sample_points,
                             popup= paste(sample_points$name)) %>% 
  
  addLayersControl(
    overlayGroups = c("District", "Sub-district","cluster"),
    baseGroups = c("OSM", "ERSI World Imagery"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("Sub-district","District"))




st_write(sample_points,"03_outputs\\01_sampling\\03_sample_point\\02_IDPs\\02_out_camp/total/all_point.kml",append = F,delete_dsn = T,delete_layer = T)
saveWidget(full_map, "03_outputs\\01_sampling\\03_sample_point\\02_IDPs\\02_out_camp/total/all_points.html",title = "Out camp IDPs sample point")


# create directory --------------------------------------------------------

if(create_directory ==T){
  
  for (i in unique(sample_points$governorate)) {
    
    dir.create(path = paste0("03_outputs/01_sampling/03_sample_point/02_IDPs\\02_out_camp/",i))
  }
  
  
  
  for (i in unique(sample_points$district)) {
    data_district <- sample_points %>% filter(district == i)
    
    gov <- (data_district %>% as.data.frame())$governorate %>% unique() %>% dput
    
    dir.create(path = paste0("03_outputs/01_sampling/03_sample_point/02_IDPs\\02_out_camp/",gov,"/",i))
  }
  
  
  for (i in unique(sample_points$place_id)) {
    data_district <- sample_points %>% filter(place_id == i)
    
    gov <- (data_district %>% as.data.frame())$governorate %>% unique() %>% dput
    dis <- (data_district %>% as.data.frame())$district %>% unique() %>% dput
    location <- (data_district %>% as.data.frame())$local_and_id %>% unique() %>% dput
    
    dir.create(path = paste0("03_outputs/01_sampling/03_sample_point/02_IDPs\\02_out_camp/",gov,"/",dis,"/",location))
  }
}


# export map by location --------------------------------------------------




for(i in unique(sample_points$local_and_id)){
  sample_point_sub_dis <- sample_points %>% filter(local_and_id ==i)
  cluster_sub_dis <- sample_bffer_final %>% filter(local_and_id ==i)
  
  point_needed <- cluster_sub_dis$Survey  %>% sum()
  sub_district <- i 
  district <- cluster_sub_dis$district %>% unique()
  governorate <-  cluster_sub_dis$governorate %>% unique()
  
  st_write(sample_point_sub_dis,paste0("03_outputs/01_sampling/03_sample_point/02_IDPs/02_out_camp/",
                                       governorate,"/",district,"/",sub_district,"/",
                                       str_replace_all(str_replace_all(paste0(sub_district),"-","_")," ","_"),
                                       "_svy_",point_needed,".kml"),append = F,delete_dsn = T,delete_layer = T)
  
  
  
  tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
  }
")) 
  
  
  title_name <- paste0("Governorate:",cluster_sub_dis$governorate," District:",cluster_sub_dis$district," Location with code:",cluster_sub_dis$local_and_id) %>% unique()
  title <- tags$div(
    tag.map.title, HTML(title_name)
    
  )  
  
  
  long <-(st_centroid(cluster_sub_dis) %>% st_coordinates())[1]
  lat <- (st_centroid(cluster_sub_dis) %>% st_coordinates())[2]
  
  
  
  map <-  base_map %>% leaflet::addPolygons(data = cluster_sub_dis,
                                            color = "#772B2C",
                                            label = ~htmlEscape(Survey),
                                            labelOptions = labelOptions(noHide = F,
                                                                        direction = 'center',
                                                                        textOnly = T,
                                                                        style = list(
                                                                          "font-family" = "Arial Narrow",
                                                                          "font-size" = "15px",
                                                                          "font-style" = "italic"
                                                                        )),
                                            popup = paste("Governorate:", cluster_sub_dis$governorate, "<br>",
                                                          "District:", cluster_sub_dis$district, "<br>",
                                                          "Location with code:", cluster_sub_dis$local_and_id, "<br>",
                                                          "Population:", cluster_sub_dis$pop_numbers, "<br>",
                                                          "Survey_needed:", cluster_sub_dis$Survey),
                                            
                                            weight = 2,dashArray = "9",
                                            fillColor = "transparent",
                                            group = "cluster") %>% 
    
    leaflet::addAwesomeMarkers(data =sample_point_sub_dis,
                               popup= paste(sample_point_sub_dis$name)) %>% 
    
    addLayersControl(
      overlayGroups = c("District", "Sub-district","cluster"),
      baseGroups = c("OSM","ERSI World Imagery"),
      options = layersControlOptions(collapsed = FALSE)) %>% 
    hideGroup(c("Sub-district","District")) %>% 
    addControl(title, position = "topleft", className="map-title") %>% setView(lng = long,lat = lat,zoom = 14)
  
  
  map
  saveWidget(map,"map_out_camp.html",title = i)
  
  file.copy(from = "map_out_camp.html",to =  paste0("03_outputs/01_sampling/03_sample_point/02_IDPs/02_out_camp/",
                                                    governorate,"/",district,"/",sub_district,"/",
                                                    str_replace_all(str_replace_all(paste0(sub_district),"-","_")," ","_"),
                                                    "_svy_",point_needed,".html"))
  
  
  
  
}





