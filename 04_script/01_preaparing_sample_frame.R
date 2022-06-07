rm(list = ls())

library(dplyr)
library(openxlsx)
library(readxl)
library(stringr)
library(snakecase)
library(sf)
library(raster)
library(sp)
library(exactextractr)
library(lwgeom)
library(units)
library(rgeos)
library(rmapshaper)
library(stringr)
library(maptools)

options(scipen = 9999)
# host_community ----------------------------------------------------------


################ For stratified random sampling #########################3
hc_pop <- read.csv("01_inputs/01_sample_frame/01_host_community/HH_pop_from_shinyapp.csv")

hc_pop_summarized <- hc_pop %>% group_by(ADM1_EN,ADM2_EN) %>% summarise(
  pop_wp = sum(pop_wp,na.rm = T),
  pop_fb = sum(pop_fb,na.rm = T),
  pop_jrc = sum(pop_jrc,na.rm = T)
) 

write.csv(hc_pop_summarized,"03_outputs/01_sampling/01_sample_frame/01_host_community/HC_sample_frame.csv")

############## For 2 stage cluster sampling ####################################

hh_pop_c <-  read.csv("01_inputs/01_sample_frame/01_host_community/HH_pop_by_hex.csv")


hc_pop_c_summarized <- hh_pop_c %>% group_by(ADM1_EN,ADM2_EN) %>% summarise(
  pop_wp = sum(pop_wp,na.rm = T),
  pop_fb = sum(pop_fb,na.rm = T),
  pop_jrc = sum(pop_jrc,na.rm = T)
) 
write.csv(hc_pop_c_summarized,"03_outputs/01_sampling/01_sample_frame/01_host_community/HC_sample_frame_from_cluster.csv")



# idp sampling ------------------------------------------------------------

idp_dtm_df <- read_excel("01_inputs/01_sample_frame/02_IDPs/IDP_2022_03_31.xlsx",sheet = "DTM Dataset",skip = 2)[1:12]
names(idp_dtm_df) <-names(idp_dtm_df) %>% snakecase::to_snake_case()


admin_path <- "G:\\My Drive\\01_Professional\\08_REACH_IRAQ_UPDATE\\mh1\\05_common_shapefiles\\admin_boundary/"
admin3_boundary<- st_read(paste0(admin_path,"irq_admbnda_adm3_cso_20190603.shp"))
admin3_boundary <- admin3_boundary %>% dplyr::select(ends_with("_EN"))

idp_dtm_df_sf <- idp_dtm_df %>% st_as_sf(coords = c("longitude","latitude"),crs = 4326) %>% dplyr::select(-governorate,-district)

idp_dtm_df_sf_spa_join<- idp_dtm_df_sf %>% st_intersection(admin3_boundary)

plot_id  <-idp_dtm_df_sf$place_id[!idp_dtm_df_sf$place_id %in% idp_dtm_df_sf_spa_join$place_id]


idp_dtm_df_sf_spa_join_chec <- idp_dtm_df_sf %>% filter(place_id %in% plot_id) %>% mutate(
  ADM3_EN = case_when(place_id  ==27294 ~ "Um Qasr",
                      place_id  ==23620 ~ "Markaz Zakho",
                      place_id  ==25688 ~ "Rabia"),
  
  ADM2_EN = case_when(place_id  ==27294 ~ "Al-Zubair",
                      place_id  ==23620 ~ "Zakho",
                      place_id  ==25688 ~ "Telafar"),
  
  ADM1_EN = case_when(place_id  ==27294 ~ "Al-Basrah",
                      place_id  ==23620 ~ "Duhok",
                      place_id  ==25688 ~ "Ninewa"),
  ADM0_EN = "Iraq"
)

idp_dtm_df <- idp_dtm_df_sf_spa_join %>% bind_rows(idp_dtm_df_sf_spa_join_chec)



idp_dtm_df <- idp_dtm_df  %>% rename(
  governorate = ADM1_EN,
  district = ADM2_EN,
  sub_district = ADM3_EN
) %>% dplyr::select(-ADM0_EN)




idp_dtm_df <- idp_dtm_df %>% dplyr::select(-c("individuals_displaced_for_a_1_st_time", 
                               "individuals_arriving_from_other_location_of_displacement",
                               "location_name_in_arabic"))


idp_dtm_df$longitude <- st_coordinates(idp_dtm_df$geometry)[,1]
idp_dtm_df$latitude <- st_coordinates(idp_dtm_df$geometry)[,2]
idp_dtm_df <- idp_dtm_df %>% as.data.frame() %>% dplyr::select(-geometry)

district_summary <- idp_dtm_df %>% dplyr::group_by(district) %>% summarise(
  number_hh = sum(households,na.rm = T)
) %>% dplyr::filter(number_hh < 200)

district_to_remove <- district_summary$district

final_sampling <- idp_dtm_df  %>% dplyr::filter(!district %in% district_to_remove) # first conditon: in Tor its in population of interest            


idp_final_sampling <- final_sampling %>% mutate(
  in_out_camp = case_when(individuals == camp ~ "in_camp",T~"out_camp")
) %>% filter((in_out_camp == "in_camp" & households > 99) | in_out_camp == "out_camp")  # second codition: in ToR, its in sampling (3.4.2.1)

in_camp_idp_final_sampling <- idp_final_sampling %>% filter(in_out_camp == "in_camp")
out_camp_idp_final_sampling <- idp_final_sampling %>% filter(in_out_camp == "out_camp") #%>% filter(district != "Al-Hamdaniya")



write.csv(idp_dtm_df,"03_outputs/01_sampling/01_sample_frame/02_IDPs/IDP_all_include.csv")
write.csv(idp_final_sampling,"03_outputs/01_sampling/01_sample_frame/02_IDPs/IDP_sample_frame.csv")
write.csv(in_camp_idp_final_sampling,"03_outputs/01_sampling/01_sample_frame/02_IDPs/IDP_in_camp_sample_frame.csv")
write.csv(out_camp_idp_final_sampling,"03_outputs/01_sampling/01_sample_frame/02_IDPs/IDP_out_camp_sample_frame.csv")




# returnees ---------------------------------------------------------------
dtm_df_r <- read_excel("01_inputs/01_sample_frame/03_Returnees/Returnee_2022_03_31.xlsx",sheet = "RETURNEE DATASET",skip = 2)[1:9]
names(dtm_df_r) <-names(dtm_df_r) %>% snakecase::to_snake_case()


admin_path <- "G:\\My Drive\\01_Professional\\08_REACH_IRAQ_UPDATE\\mh1\\05_common_shapefiles\\admin_boundary/"
admin3_boundary<- st_read(paste0(admin_path,"irq_admbnda_adm3_cso_20190603.shp"))
admin3_boundary <- admin3_boundary %>% dplyr::select(ends_with("_EN"))

dtm_df_r_sf <- dtm_df_r %>% st_as_sf(coords = c("longitude","latitude"),crs = 4326) %>% dplyr::select(-governorate,-district)

dtm_df_r_sf_spa_join<- dtm_df_r_sf %>% st_intersection(admin3_boundary)





plot_id  <-dtm_df_r$place_id[!dtm_df_r$place_id %in% dtm_df_r_sf_spa_join$place_id]


dtm_df_r_sf_spa_join_chec <- dtm_df_r_sf %>% filter(place_id %in% plot_id) %>% mutate(
  ADM3_EN = case_when(place_id  ==33466 ~ "Al-Rummaneh",
                      place_id  ==23620 ~ "Markaz Zakho",
                      place_id  ==25688 ~ "Rabia"),
  
  ADM2_EN = case_when(place_id  ==33466 ~ "Al-Kaim",
                      place_id  ==23620 ~ "Zakho",
                      place_id  ==25688 ~ "Telafar"),
  
  ADM1_EN = case_when(place_id  ==33466 ~ "Al-Anbar",
                      place_id  ==23620 ~ "Duhok",
                      place_id  ==25688 ~ "Ninewa"),
  ADM0_EN = "Iraq"
)


dtm_df_r <- dtm_df_r_sf_spa_join %>% bind_rows(dtm_df_r_sf_spa_join_chec)



dtm_df_r <- dtm_df_r  %>% rename(
  governorate = ADM1_EN,
  district = ADM2_EN,
  sub_district = ADM3_EN
) %>% dplyr::select(-ADM0_EN)


dtm_df_r <- dtm_df_r %>% dplyr::select(-location_name_in_arabic)



dtm_df_r$longitude <- st_coordinates(dtm_df_r$geometry)[,1]
dtm_df_r$latitude <- st_coordinates(dtm_df_r$geometry)[,2]
dtm_df_r <- dtm_df_r %>% as.data.frame() %>% dplyr::select(-geometry)



district_summary_r <- dtm_df_r %>% group_by(district) %>% summarise(
  number_hh = sum(households,na.rm = T)
) %>% filter(number_hh < 200)


district_to_remove_r <- district_summary_r$district

returnees_final_sampling <- dtm_df_r  %>% filter(!district %in% district_to_remove_r)             

write.csv(dtm_df_r,"03_outputs/01_sampling/01_sample_frame/03_Returnees/RETURNEES_all_include.csv")
write.csv(returnees_final_sampling,"03_outputs/01_sampling/01_sample_frame/03_Returnees/RETURNEES_sample_frame.csv")



# host comunity -----------------------------------------------------------

district_of_interest <- c("Erbil","Sumail","Sinjar","Al-Falluja","Baquba", 
                          "Al-Rutba","Al-Baaj","Al-Hawiga","Al-Hatra","Tooz Khurmato" )
  
adm_boundary <- st_read("01_inputs/03_shapefile/irq_admbnda_adm2_cso_20190603.shp")
adm_3 <- st_read("01_inputs/03_shapefile/irq_admbnda_adm3_cso_20190603.shp")
raster_wp <- raster("01_inputs/02_population/01_world_pop/irq_ppp_2020_UNadj_constrained.tif")
adm_3<- adm_3 %>% filter(ADM2_EN %in% district_of_interest)



adm_boundary_dis_wgs<- adm_boundary %>% filter(ADM2_EN %in% district_of_interest)
adm_boundary_dis<- adm_boundary_dis_wgs %>% st_transform(32638)
adm_boundary_dis_spatial <- adm_boundary_dis %>% as_Spatial()


# create hexagons
HexPts <-spsample(adm_boundary_dis_spatial, "hexagonal",n=135000, offset=c(0,0))
grid <- HexPoints2SpatialPolygons(HexPts) 
grid <- grid %>% st_as_sf() %>% st_transform(crs= 32638) 
grid <- grid %>% st_transform(4326)
grid <- grid %>% mutate(
  row_id = row.names(grid)
)

grid$row_id %>% duplicated() %>% table()

adm_boundary_dis_wgs_all <- adm_boundary_dis_wgs %>% mutate(a="dis") %>% group_by(a) %>% summarise()
grid <- grid %>% st_intersection(adm_boundary_dis_wgs_all) %>% select(row_id)

grid$row_id %>% duplicated() %>% table()

# extract population 
pop_frm_zone_wp<- exact_extract(raster_wp, grid, function(values,coverage_fractions)
  sum(values*coverage_fractions,na.rm = T))
Zone_with_data_wp <- data.frame(row_id = row.names(grid),
                                pop_wp= pop_frm_zone_wp %>% as.integer() )



# adding population to the grid
zone_with_population<- grid %>% left_join(Zone_with_data_wp)


# add admin infromation to the grid
hex_centriod <- st_centroid(zone_with_population)
hex_point_admin_info <- st_intersection(hex_centriod,adm_3)
hex_point_admin_need_info <- hex_point_admin_info %>% dplyr::select(row_id, ends_with("_EN")) %>% as.data.frame() %>% dplyr::select(-geometry)


zone_with_population <- zone_with_population %>% st_transform(crs = 4326)


final_zone <- zone_with_population %>% left_join(hex_point_admin_need_info)
final_zone <- final_zone %>% st_transform(crs=32638)
final_zone <- final_zone %>% filter(pop_wp !=0) %>% st_transform(crs=4326)

final_zone_geo_remove <- final_zone %>% as.data.frame() %>% select(-geometry)

st_write(final_zone,"03_outputs/01_sampling/01_sample_frame/01_host_community/01_grid_without_removing_non_livable_area/Hc_sample_frame_grid.shp")
write.csv(final_zone_geo_remove,"03_outputs/01_sampling/01_sample_frame/01_host_community/HC_sample_frame_grid.csv")


final_zone$pop_wp %>% sum()
