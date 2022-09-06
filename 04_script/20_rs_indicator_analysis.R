rm(list = ls())

library(dplyr)
library(readxl)
library(openxlsx)
library(purrr)
library(srvyr)
library(illuminate)
library(sf)
library(jtools)
library(car)
source("04_script/statistical_test_function.R")
options(scipen = 999)

# data preparation --------------------------------------------------------

# indicator <- readRDS("06_remote_sensing_analysis/surveyGEER-main/data_share/irq_rs_indicators_wide.rds")
# look_up <- readRDS("06_remote_sensing_analysis/surveyGEER-main/vault/lookup.rds")
# indicator$new_uid <- as.numeric(indicator$new_uid)
# look_up$new_uid <- as.numeric(look_up$new_uid)
# 
# indicator_with_uuid <- indicator %>% left_join(look_up)
# 
# distance <- st_read("01_inputs/03_shapefile/distance_from_water_body/MCNA with distances.shp") %>% select(new_uid,Distance) %>% distinct()
# 
# 
# indicator_with_uuid <- indicator_with_uuid %>% left_join(distance)
# 
# 
# 
# clean_data <- read_xlsx("03_outputs/10_analysis/2022_08_29_mcna_analysis.xlsx",sheet = "hh_with_weight")
# 
# clean_data_with_rs_indicator <- clean_data %>% left_join(indicator_with_uuid)

# write.csv(clean_data_with_rs_indicator,"clean_data_with_rs_indicators.csv")



# read data ---------------------------------------------------------------

clean_data_with_rs_indicator <- read.csv("clean_data_with_rs_indicators.csv")

clean_data_with_rs_indicator <- clean_data_with_rs_indicator %>% filter(strata_and_pop_group != "Returnee HH_al_rutba")


clean_data_with_rs_indicator$Distance <- clean_data_with_rs_indicator$Distance %>% log()
clean_data_with_rs_indicator$rs_avg_dist_perm_water_pix_20172020 <- clean_data_with_rs_indicator$rs_avg_dist_perm_water_pix_20172020 %>% log()
clean_data_with_rs_indicator$rs_city_accessibility2015 <- clean_data_with_rs_indicator$rs_city_accessibility2015 %>% log()


#  pearson correlation test   --------------------------------------------------------

correlation_test <- list()
dependent_variable <- c("Distance", "rs_avg_dist_perm_water_pix_20172020","rs_NDVI_May2022")


for(i in dependent_variable){


correlation_test[[i]] <- weighted_pearson_test(data = clean_data_with_rs_indicator,
                      dep_var =i,
                      ind_var = c("fcs","how_much_debt","food_exp","medical_exp","rs_city_accessibility2015"),
                      strata = "strata_and_pop_group",
                      survey_weights = "survey_weight")


}



correlation_test_final <- do.call("bind_rows",correlation_test)





















write_list <- list(
  correlation_test_final ="correlation_test_final",
  t_test ="t_test"
)

write_excel_as_reach_format(write_list,"03_outputs/10_analysis/correlation_test_mcna.xlsx")



# t test ------------------------------------------------------------------

t_test_result <- list()

independent_variable <- c("Distance","rs_avg_dist_perm_water_pix_20172020","rs_NDVI_May2022","rs_healthcare_accessibility2019")

for(x in independent_variable){

  t_test_result[[x]] <- weighted_t_test(data = clean_data_with_rs_indicator,
                                        dependent_variables = c("need_priorities.drinking_water","need_priorities.food","need_priorities.healthcare") ,
                                      independent_variable = x,
                                      strata = "strata_and_pop_group",
                                      survey_weights = "survey_weight")


}



t_test <- do.call("bind_rows",t_test_result)


m2 <- lm(fcs~Distance+log(rs_healthcare_accessibility2019),clean_data_with_rs_indicator)


avPlots(m2)


vif(m2)
summary(m2)

