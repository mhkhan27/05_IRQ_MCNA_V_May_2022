rm (list=ls())

library(dplyr)
library(st)
library(sf)


# out_camp_idp ------------------------------------------------------------
admin_path <- "G:\\My Drive\\01_Professional\\08_REACH_IRAQ_UPDATE\\mh1\\05_common_shapefiles\\admin_boundary/"
admin3_boundary<- st_read(paste0(admin_path,"irq_admbnda_adm3_cso_20190603.shp"))
admin3_boundary <- admin3_boundary %>% select(ends_with("_EN"))

out_camp_sf <- out_camp_idp %>% st_as_sf(coords = c("longitude","latitude"),crs = 4326) %>% select(-governorate,-district)

out_camp_sf_spa_join<- out_camp_sf %>% st_intersection(admin3_boundary)




plot_id  <-out_camp_idp$place_id[!out_camp_idp$place_id %in% out_camp_sf_spa_join$place_id]


out_camp_sf_chec <- out_camp_sf %>% filter(place_id %in% plot_id) %>% mutate(
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


full_out_camp <- out_camp_sf_spa_join %>% bind_rows(out_camp_sf_chec)

full_out_camp$longitude <- st_coordinates(full_out_camp$geometry)[,1]
full_out_camp$latitude <- st_coordinates(full_out_camp$geometry)[,2]
full_out_camp_df <- full_out_camp %>% as.data.frame() %>% select(-geometry)

full_out_camp_df <- full_out_camp_df %>% rename(
  governorate = ADM1_EN,
  district = ADM2_EN,
  sub_district = ADM3_EN
) %>% select(-ADM0_EN)


write.csv(full_out_camp_df,"03_outputs/01_sampling/01_sample_frame/02_IDPs/IDP_out_camp_sample_frame.csv")
