rm(list = ls())

library(dplyr)
library(illuminate)
library(readxl)
library(stringr)
library(openxlsx)

idp_iom <- read_excel("01_inputs\\01_sample_frame\\02_IDPs/IDP_2022_03_31.xlsx",sheet = "DTM Dataset",skip = 2) 
names(idp_iom) <- names(idp_iom) %>% snakecase::to_snake_case()
idp_iom_se <- idp_iom %>% select(place_id,location_name_in_arabic) 
idp_sf <- read.csv("03_outputs/01_sampling/02_sampling/02_IDPs/01_out_camp/sampling_frame.csv") %>% select(place_id,location_name_in_english,district) %>% mutate(pop_grp = 1)

idp_total <- idp_sf %>% left_join(idp_iom_se)




ret_iom <- read_excel("01_inputs\\01_sample_frame\\03_Returnees/Returnee_2022_03_31.xlsx",sheet = "RETURNEE DATASET",skip = 2) 
names(ret_iom) <- names(ret_iom) %>% snakecase::to_snake_case()
ret_iom_se <- ret_iom %>% select(place_id,location_name_in_arabic) 
retun_sf <- read.csv("03_outputs/01_sampling/02_sampling/03_Returnees/sampling_frame.csv") %>% select(place_id,location_name_in_english,district) %>% mutate(pop_grp = 0)
return_total <- retun_sf %>% left_join(ret_iom_se)



tool_path <- "02_ToR_tools_Dap/01_tool/IRQ2206_Tool_MCNA X_transl_inc_CampProfiling_June 2nd.xlsx"
read_all_sheet_as_csv_format(tool_path)

district_in_kobo <- (choices %>% filter(list_name == "district_mcna"))$name


combine <- return_total %>% bind_rows(idp_total) %>% mutate(
  name = snakecase::to_snake_case(location_name_in_english),
  district = snakecase::to_snake_case(district),
  check_dist = district %in% district_in_kobo
) %>% select(-pop_grp) %>% distinct()

write_excel_as_reach_format(list(combine = "comnine"),"cluster.xlsx")
