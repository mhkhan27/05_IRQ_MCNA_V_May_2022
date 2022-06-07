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

# BASE MAP ----------------------------------------------------------------
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



# host community ----------------------------------------------------------

cluster<- st_read("03_outputs/01_sampling/01_sample_frame/01_host_community/01_grid_without_removing_non_livable_area/Hc_sample_frame_grid.shp")
cluster_id_and_po <- cluster %>% as.data.frame() %>% select(row_id,pop_wp)


water<- st_read("01_inputs/03_shapefile/irq_water_bodies_dis.shp")  %>% st_transform(4326)
non_liveable <- st_read("01_inputs/03_shapefile/non_liveable_final_dis.shp") 



grid_remove_non_liveable <- rmapshaper::ms_erase(cluster,non_liveable)
cluster_or_sampling <- rmapshaper::ms_erase(grid_remove_non_liveable,water)
cluster_or_sampling$row_id %>% duplicated() %>% table()

# read_sample_frame -------------------------------------------------------

sample_frame <- read.csv("03_outputs/01_sampling/02_sampling/01_host_community/sampling_frame.csv")
sample_frame_needed_cols <- sample_frame %>% select(row_id,id_sampl,Survey,survey_buffer,strata_id,psu_id,
                                                    pop_numbers,SumDist,proba)

sample_cluster <- cluster_or_sampling %>% filter(row_id %in% sample_frame_needed_cols$row_id)
sample_cluster$row_id <- sample_cluster$row_id %>% as.integer()


sample_cluster_with_all_info <- sample_cluster %>% left_join(sample_frame_needed_cols)

# create sample point
sample_point <- sample_cluster_with_all_info %>% st_sample(sample_cluster_with_all_info$survey_buffer) %>% st_as_sf()
sample_point_inter <- st_intersection(sample_point,(sample_cluster_with_all_info %>% select(ends_with("_EN")))) 
sample_point_inter_point_id <- sample_point_inter %>% group_by(ADM1_EN,ADM2_EN,ADM3_EN) %>% mutate(
  point_id = row_number()
) %>% ungroup() %>% mutate(
  name = paste0("PT_",point_id,"_",ADM3_EN,"_",ADM2_EN,"_",ADM1_EN)
)


# Full map ----------------------------------------------------------------


 full_map <-  base_map %>% leaflet::addPolygons(data = sample_cluster_with_all_info,
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
                                  popup = paste("Governorate:", sample_cluster_with_all_info$ADM1_EN, "<br>",
                                                "District:", sample_cluster_with_all_info$ADM2_EN, "<br>",
                                                "Sub-District:", sample_cluster_with_all_info$ADM3_EN, "<br>",
                                                "Population:", sample_cluster_with_all_info$pop_wp, "<br>",
                                                "Survey_needed:", sample_cluster_with_all_info$Survey),
                                  
                                  weight = 2,dashArray = "9",
                                  fillColor = "transparent",
                                  group = "cluster") %>% 
  
  leaflet::addAwesomeMarkers(data =sample_point_inter_point_id,
                             popup= paste(sample_point_inter_point_id$name)) %>% 
  
  addLayersControl(
    overlayGroups = c("District", "Sub-district","cluster"),
    baseGroups = c("OSM", "ERSI World Imagery"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("Sub-district","District"))

st_write(sample_point_inter_point_id,"03_outputs/01_sampling/03_sample_point/01_host_community/01_map_and_kml/full_map/HC_sample_point_all.shp",append = F,delete_dsn = T,delete_layer = T)
st_write(sample_cluster_with_all_info,"03_outputs/01_sampling/03_sample_point/01_host_community/01_map_and_kml/full_map/HC_cluster_only_sample.shp",append = F,delete_dsn = T,delete_layer = T)
st_write(sample_point_inter_point_id,"03_outputs/01_sampling/03_sample_point/01_host_community/01_map_and_kml/full_map/HC_cluster_only_sample.kml")
saveWidget(full_map, "03_outputs/01_sampling/03_sample_point/01_host_community/01_map_and_kml/full_map/HC_map.html",title = "Host community sample point")


# create directory --------------------------------------------------------

if(create_directory ==T){
  
  for (i in unique(sample_cluster_with_all_info$ADM1_EN)) {
    
    dir.create(path = paste0("03_outputs/01_sampling/03_sample_point/01_host_community/01_map_and_kml/",i))
  }
  
  
  
  for (i in unique(sample_cluster_with_all_info$ADM2_EN)) {
    data_district <- sample_cluster_with_all_info %>% filter(ADM2_EN == i)
    
    gov <- (data_district %>% as.data.frame())$ADM1_EN %>% unique() %>% dput
    
    dir.create(path = paste0("03_outputs/01_sampling/03_sample_point/01_host_community/01_map_and_kml/",gov,"/",i))
  }
  
  
  
  for (i in unique(sample_cluster_with_all_info$ADM3_EN)) {
    
    data_sub_district <- sample_cluster_with_all_info %>% filter(ADM3_EN == i)
    
    gov <- (data_sub_district %>% as.data.frame())$ADM1_EN %>% unique() %>% dput
    dis <- (data_sub_district %>% as.data.frame())$ADM2_EN %>% unique() %>% dput
    
    dir.create(paste0("03_outputs/01_sampling/03_sample_point/01_host_community/01_map_and_kml/",gov,"/",dis,"/",i))
  }
  
}


# sub_district wise export ------------------------------------------------




for(i in unique(sample_point_inter_point_id$ADM3_EN)){
  sample_point_sub_dis <- sample_point_inter_point_id %>% filter(ADM3_EN ==i)
  cluster_sub_dis <- sample_cluster_with_all_info %>% filter(ADM3_EN ==i)
  
  point_needed <- cluster_sub_dis$Survey  %>% sum()
  sub_district <- i 
  district <- cluster_sub_dis$ADM2_EN %>% unique()
  governorate <-  cluster_sub_dis$ADM1_EN %>% unique()
  
  st_write(sample_point_sub_dis,paste0("03_outputs/01_sampling/03_sample_point/01_host_community/01_map_and_kml/",
                                       governorate,"/",district,"/",sub_district,"/",
                                       str_replace_all(paste0(governorate,"_",district,"_",sub_district),"-","_"),
                                       "_survey_needed",point_needed,".kml"),append = F,delete_dsn = T,delete_layer = T)
  
  
  
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
  
  
  title_name <- paste0("Governorate:",cluster_sub_dis$ADM1_EN," District:",cluster_sub_dis$ADM2_EN," Sub_district:",cluster_sub_dis$ADM3_EN) %>% unique()
  title <- tags$div(
    tag.map.title, HTML(title_name)
    
  )  
  
  
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
                                                 popup = paste("Governorate:", cluster_sub_dis$ADM1_EN, "<br>",
                                                               "District:", cluster_sub_dis$ADM2_EN, "<br>",
                                                               "Sub-District:", cluster_sub_dis$ADM3_EN, "<br>",
                                                               "Population:", cluster_sub_dis$pop_wp, "<br>",
                                                               "Survey_needed:", cluster_sub_dis$Survey),
                                                 
                                                 weight = 2,dashArray = "9",
                                                 fillColor = "transparent",
                                                 group = "cluster") %>% 
    
    leaflet::addAwesomeMarkers(data =sample_point_sub_dis,
                               popup= paste(sample_point_sub_dis$name)) %>% 
    
    addLayersControl(
      overlayGroups = c("District", "Sub-district","cluster"),
      baseGroups = c("ERSI World Imagery","OSM"),
      options = layersControlOptions(collapsed = FALSE)) %>% 
    hideGroup(c("Sub-district","District")) %>% 
    addControl(title, position = "topleft", className="map-title")
  
  
  
  saveWidget(map,"map.html",title = i)
  
 file.copy(from = "map.html",to =  paste0("03_outputs/01_sampling/03_sample_point/01_host_community/01_map_and_kml/",
                                          governorate,"/",district,"/",sub_district,"/",
                                          str_replace_all(paste0(governorate,"_",district,"_",sub_district),"-","_"),
                                          "_survey_needed",point_needed,".html"))
  
  
  
  
}














