rm(list = ls())

library(dplyr)
library(st)
library(sf)
library(stringr)
library(illuminate)
library(readxl)
library(readr)
library(openxlsx)
library(tidyverse)



kobo_tool <- read_excel("02_ToR_tools_Dap/01_tool/IRQ2206_Tool_MCNA X_transl_inc_CampProfiling_June 3rd_DEPLOYED.xlsx", sheet = 1)
kobo_choices <-  read_excel("02_ToR_tools_Dap/01_tool/IRQ2206_Tool_MCNA X_transl_inc_CampProfiling_June 3rd_DEPLOYED.xlsx", sheet = 2)

# you have change the path. Please ask TED for the folder
mcna_one_drive_folder <- "C:\\Users\\rakib\\OneDrive - ACTED\\MCNA 2022\\4. Sampling and Data/2. Data Cleaning/Logs/"
partner_log_folder_path <- "C:\\Users\\rakib\\OneDrive - ACTED\\MCNA 2022\\4. Sampling and Data/2. Data Cleaning/Partner logs/"


cl_list <- list.files(mcna_one_drive_folder,".xlsx",full.names = T)


# combine reach cleaning log ----------------------------------------------

cleaning_log_reach <- list()

for (i in cl_list ){
  cleaning_log_reach[[i]] <- read_excel(i,sheet = "CONC") %>%
    mutate_all(as.character) %>% mutate(
      file_name = i
    )
}

cleaning_log_total <- do.call("bind_rows",cleaning_log_reach)

cleaning_log_total <- cleaning_log_total %>% select (X_uuid, repeat_uuid, ngo_label, loop, question, issue, old_value, change_type, new_value,file_name) 



# combine partners cleaning logs ------------------------------------------


partner_cl_list <- list.files(partner_log_folder_path,".xlsx",full.names = T)

partner_cleaning_log <- list()
for (i in partner_cl_list ){
  print(i)
  partner_cleaning_log[[i]] <- read_excel(i,sheet = "CONC") %>%
    mutate_all(as.character) %>% mutate(
      file_name = i
    )
}


partner_cleaning_log_total <- do.call("bind_rows",partner_cleaning_log)

partner_cleaning_log_total <- partner_cleaning_log_total %>% select (X_uuid, repeat_uuid, ngo_label, loop, question, issue, old_value, change_type, new_value,file_name) 



# compile cleaning logs ---------------------------------------------------

final_cleaning_log <- cleaning_log_total %>% bind_rows(partner_cleaning_log_total)


#################### double check no change_type ################################33
# 
# a <- final_cleaning_log %>% filter(is.na(change_type))
# 
# write.csv(a,"no_value_in_change_response.csv",na = "")
#################################################################################



final_cleaning_log <- final_cleaning_log %>% filter(!X_uuid %in% c("1b75932c-3402-4b89-9ba0-aabe016a6bd6",
                                                                   "183663e99d1b4496876ad25b6a840df9") ) # removed survey as consent no

final_cleaning_log <- final_cleaning_log %>% filter(!is.na(change_type))

cleaning_log <- final_cleaning_log %>% filter(!question %in% c("location_camp","arrival_displace_date_diff_idp","arrival_displace_date_diff_camp") | 
                                                (is.na(question) & change_type == "remove_survey"))
  
cleaning_log <- cleaning_log %>% select (X_uuid, repeat_uuid, question, change_type, old_value, new_value, issue)

output_hh <- list()

#######################read data###############################

hh_raw <- read.csv("01_inputs/04_raw_data/hh.csv", stringsAsFactors = F, na.strings = c("", " ", "NA", "N/A"))

hh_70 <- read.csv("01_inputs/04_raw_data/hh_training.csv", stringsAsFactors = F, na.strings = c("", " ", "NA", "N/A"))

indv_raw <- read.csv("01_inputs/04_raw_data/indv.csv", stringsAsFactors = F, na.strings = c("", " ", "NA", "N/A"))

indv_raw <- indv_raw %>% filter(X_submission__uuid %in% hh_raw$X_uuid)


indv_70 <- read.csv("01_inputs/04_raw_data/indv_training.csv", stringsAsFactors = F, na.strings = c("", " ", "NA", "N/A"))

##############################################################
#combine remaining 70 surveys with raw data

hh_raw <- hh_raw %>% bind_rows(hh_70)

hh_raw <-  hh_raw %>% filter(consent == "yes" & calc_eligible == 1)

indv_raw <- indv_raw %>% bind_rows(indv_70)

#############################################################


# hh_raw <-  hh_raw %>% filter(consent == "yes" & calc_eligible == 1) # takes only yes consent



# add population group
hh_raw <- hh_raw %>% mutate(
  pop_group = case_when(idp_camp == "yes" &  !is.na(camp_name) ~ "In-camp IDP HH",
                        calc_idp == 1 ~ "Out-of-camp IDP HH",
                        calc_returnee == 1 ~ "Returnee HH",
                        calc_host == 1 ~ "Host Community HH"),
  dis_rs = rowSums(hh_raw[c("calc_idp","calc_returnee","calc_host")],na.rm = T)
) %>% mutate(
  check_grp = case_when(dis_rs > 1 ~ "multiple_group",
                        dis_rs == 1  & idp_camp == "yes" ~ "multiple_group"),
  enumerator_name_id = case_when(ngo_name == "reach" ~ as.character(enumerator_num),
                                 T~as.character(enumerator_name))
)


group_by_pop_grp <- hh_raw %>% group_by(pop_group) %>% summarise(
  total_elegible = n()
)
if(TRUE %in% unique(is.na(hh_raw$pop_group))) {stop("you have NAs in group")}

if("multiple_group" %in% unique(hh_raw$check_grp) ) {stop("you have multiple group")}



# fix the district boundary -----------------------------------------------


admin_path <- "01_inputs/03_shapefile/"
admin3_boundary<- st_read(paste0(admin_path,"irq_admbnda_adm3_cso_20190603.shp")) %>% select(ends_with("_EN")) %>% rename(
  governorate_true = ADM1_EN,
  district_true = ADM2_EN,
  sub_district_true = ADM3_EN 
) %>% select(-ADM0_EN)

admin2_boundary <- st_read(paste0(admin_path,"irq_admbnda_adm2_cso_20190603.shp")) %>% select(ends_with("_EN")) %>% rename(
  governorate = ADM1_EN,
  district  = ADM2_EN
) %>% select(-ADM0_EN) %>% mutate(
  district = district %>% tolower() %>% str_replace_all("-","_") %>% str_replace_all(" ","_") %>% str_replace_all("'","")
)

df_for_true <- hh_raw %>% select(X_uuid,contains("gps"))


data_sf <- df_for_true %>% st_as_sf(coords = c("X_gpslocation_longitude","X_gpslocation_latitude"),crs = 4326)


df_with_adm <- data_sf %>% st_intersection(admin3_boundary) %>% as.data.frame() %>% select(-contains("gps"),-geometry)


hh_raw <- hh_raw %>% left_join(df_with_adm) %>% mutate(
  district = case_when(is.na(district_true)~ district_mcna, T ~ district_true),
  governorate = case_when(is.na(governorate_true)~ governorate_mcna, T ~ governorate_true),
)%>%  mutate(
  district = district %>% tolower() %>% str_replace_all("-","_") %>% str_replace_all(" ","_") %>% str_replace_all("'","")
)



##############################################################
#clean individual data from remove individual cleaning log entries

cleaning_log_rm_indv <- cleaning_log %>% filter(change_type == "remove_individual")

indv_raw <- indv_raw[!indv_raw$repeat_uuid %in% cleaning_log_rm_indv$repeat_uuid,]


##############################################################

cleaning_log$question <- cleaning_log$question %>% str_replace_all("/","\\.")

rm_svy_uuid <- (cleaning_log %>% filter(change_type == "remove_survey"))$X_uuid
cleaning_log <- cleaning_log %>% mutate(
  change_type = case_when(X_uuid %in% rm_svy_uuid~ "remove_survey",
                          T~ change_type)
)


cleaning_log <- cleaning_log %>% mutate(
  dataset_loop = case_when(question %in% names(hh_raw) ~ "hh",
                           question %in% names(indv_raw) ~ "indv",
                           T ~ NA_character_)
) %>% filter(change_type != "no_action") %>% mutate(
  dataset_loop =  case_when(change_type  == "remove_survey" ~ "hh",
                            T~ dataset_loop)
)


cleaning_log <- cleaning_log %>% mutate(
  new_value = case_when(change_type %in% c("remove_survey","blank_response") ~ "", T~new_value)
)


cleaning_log_with_loop <- cleaning_log %>% filter(!is.na(dataset_loop) | change_type == "remove_survey")
cleaning_log_without_loop <- cleaning_log %>% filter(is.na(dataset_loop) & change_type != "remove_survey" )

cleaning_log_without_loop <- cleaning_log_without_loop %>% mutate(
  parent_name = sub("\\..*", "", question)
) %>% mutate(
  dataset_loop = case_when(parent_name %in% names(hh_raw) ~ "hh",
                           parent_name %in% names(indv_raw) ~ "indv",
                           T ~ NA_character_)
)


final_cleaning_log <- cleaning_log_with_loop %>% bind_rows(cleaning_log_without_loop) %>% select(-parent_name)

final_cleaning_log$change_type[final_cleaning_log$question == "aid_not_satisfied_other" & 
                                 final_cleaning_log$new_value == "did_not_receive_aid_30_days"] <- "blank_response"  

additional_response <- final_cleaning_log %>% filter (question == "aid_not_satisfied_other" & 
                                                        new_value == "did_not_receive_aid_30_days") %>% mutate (
                                                        question = "aid_satisfaction",
                                                        change_type = "change_response",
                                                        new_value = "did_not_receive_aid_30_days") 





final_cleaning_log <- final_cleaning_log %>% bind_rows(additional_response)


# output_hh[["final_cleaning_log"]] <- final_cleaning_log


hh_cleaning_log <- final_cleaning_log %>% filter(dataset_loop == "hh")

#####################################################################

#fix change type column accordingly

hh_cleaning_log$change_type <- case_when(hh_cleaning_log$change_type =="no action" ~ "no_action",
                                         hh_cleaning_log$change_type == "confirmed" ~ "no_action",
                           T ~  hh_cleaning_log$change_type) 

############################################################################

#check multiple type questions

hh_cleaning_other <- hh_cleaning_log %>%  filter(str_detect(question, "other"))

hh_cleaning_non_other <- hh_cleaning_log %>%  filter(!str_detect(question, "other") | is.na(question))

hh_cleaning_other <- hh_cleaning_other %>% mutate(parent_name = str_replace_all(question,"_other","")) %>% mutate(
  parent_name  = case_when(question == "over_18_not_eligible_other" ~ "over_18_eligible_vote",
                           question == "housing_agrement_other" ~ "aoo_housing_agreement",
                           question == "resons_not_attend_other" ~ "reasons_not_attend",
                           question == "restriction_type_other" ~ "restriction_other",
                           question == "restriction_other" ~ "restriction_other",
                         T~ parent_name )
)

hh_cleaning_other  <- hh_cleaning_other %>% left_join((kobo_tool %>% select(type,name)),by = c("parent_name" = "name")) 


hh_cleaning_other <- hh_cleaning_other%>% mutate(
  q_type = case_when(grepl("select_multiple",type) ~ "select_multiple", 
                     grepl("select_one",type) ~ "select_one",
                     T~type)
) %>% filter(!is.na(q_type)) %>% mutate(
  choice_list = type %>% str_replace_all("select_one ","") %>% str_replace_all("select_multiple ","")
)




########################################## OTHER CLEANING LOG #######################################

other_cl <- list()


other_cl[["blank_response_select_one_df"]] <- (hh_cleaning_other %>% filter(q_type == "select_one") %>% filter(change_type == "blank_response"))[,1:8] %>% mutate_all(as.character)


other_select_1 <- hh_cleaning_other %>% filter(q_type == "select_one") %>% filter(change_type == "change_response") 

select_one <- list()
for(i in 1:nrow(other_select_1)){
  df <- other_select_1[i,]
  choice_variable <- df$choice_list
  choice_list_from_kobo <- (kobo_choices %>%  filter(list_name == choice_variable))$name
  select_one[[i]]<- df %>% mutate(
    type_of_change = case_when(new_value %in% choice_list_from_kobo ~ "change_response",T~"translation")
  )
}

select_one_df <- do.call("bind_rows",select_one)

translation_select_one_df <- select_one_df %>% filter(type_of_change == "translation")
other_cl[["translation_select_one_df"]] <- translation_select_one_df[,1:8] %>% mutate_all(as.character)



### change_type to blank response
change_select_one_df <- select_one_df %>% filter(type_of_change == "change_response")

 blank_select_one<- change_select_one_df %>% mutate(
  change_type = "blank_response",
  new_value = ""
) 
  
other_cl[["blank_select_one_df"]] <- blank_select_one[,1:8]%>% mutate_all(as.character)

#select_one change type

change_select_one_to_change <- change_select_one_df %>% mutate(
  question = parent_name,
  change_type = "change_response"
)
other_cl[["change_select_one_to_change"]] <- change_select_one_to_change[,1:8]%>% mutate_all(as.character)







### select multiole 

other_multi <- hh_cleaning_other %>% filter(q_type == "select_multiple") %>% mutate(
  type_of_change = case_when(new_value %in% names(hh_raw) ~ change_type, T ~ "translation")
) 

other_cl[["blank_res_other"]]<- (other_multi %>% filter(change_type == "blank_response"))[,1:8] %>% mutate_all(as.character)

other_multi <- other_multi%>% filter(change_type == "change_response")


multiple_other_df_trnas <- other_multi %>% filter(type_of_change ==  "translation")

other_cl[["select_multiple_translated"]] <- multiple_other_df_trnas[,1:8] %>% mutate_all(as.character)



multiple_other_df_change <- other_multi %>% filter(type_of_change ==  "change_response")



### .other 1 to 0 
other_cl[["multiple_other_1_to_0"]] <- multiple_other_df_change %>% mutate(
  question = paste0(parent_name, ".other"),
  new_value = 0,
  old_value = 1, 
  change_type = "change_response"
)%>% mutate_all(as.character)


### .other to blank response
other_cl[["multiple_other_blank_re"]] <- multiple_other_df_change %>% mutate(
 change_type = "blank_response",
 new_value = ""
)%>% mutate_all(as.character)



### .other new_value to 1
other_cl[["multiple_other_0_to_1"]] <- multiple_other_df_change %>% mutate(
  question = new_value,
  change_type = "change_response"
) %>% mutate(
  new_value = 1, 
  old_value =0
) %>% mutate_all(as.character)


all_other <- do.call("bind_rows", other_cl)




hh_cleaning_log <- hh_cleaning_non_other %>% bind_rows(all_other) 


hh_remove_survey <- hh_cleaning_log %>% filter(change_type == "remove_survey") 
hh_remove_survey_uuid <- unique(hh_remove_survey$X_uuid)

hh_remove_survey_clean <- data.frame(X_uuid =hh_remove_survey_uuid
) %>% mutate(
  change_type = "remove_survey"
) 

hh_pre_clean <- hh_raw %>% filter(!X_uuid %in% hh_remove_survey_clean$X_uuid)

hh_cleaning_log_wthout_rm <- hh_cleaning_log %>% filter(change_type != "remove_survey")

hh_cleaning_log_wthout_rm <- hh_cleaning_log_wthout_rm %>% filter(!X_uuid %in% hh_remove_survey_uuid )

#############################################################################################



hh_cleaning_log_wthout_rm <- hh_cleaning_log_wthout_rm %>% mutate(
  question = case_when(question == "restriction_other" & change_type == "blank_response" ~ "restriction_type_other", T~ question)
)


####################################################################

#checking for issues 

a <- check_cleaning_log(df = hh_pre_clean,df_uuid = "X_uuid",cl = hh_cleaning_log_wthout_rm,cl_change_type_col = "change_type",cl_uuid = "X_uuid", cl_change_col = "question",cl_new_val = "new_value")

a
# write.csv(a,"uuid_does_not_exist_2022.csv")




################################################################

hh_clean_dataset <- implement_cleaning_log(df = hh_pre_clean,df_uuid = "X_uuid",cl = hh_cleaning_log_wthout_rm,cl_change_type_col = "change_type",cl_uuid = "X_uuid", cl_change_col = "question",cl_new_val = "new_value")

hh_clean_dataset <- recalculate_concerted_col_for_select_multiple(df = hh_clean_dataset,uuid = "X_uuid")

################################################################

indv_cleaning_log <-final_cleaning_log %>% filter(dataset_loop == "indv") 
indv_cleaning_log$repeat_uuid %>% is.na() %>% table()
indv_cleaning_log <- indv_cleaning_log %>%  filter(change_type != "remove_individual")
indv_cleaning_log$change_type <- case_when(indv_cleaning_log$change_type =="no action" ~ "no_action",
                                           indv_cleaning_log$change_type == "confirmed" ~ "no_action",
                                         T ~  indv_cleaning_log$change_type) 


indv_cleaning_log <- indv_cleaning_log %>% filter(X_uuid %in% hh_clean_dataset$X_uuid)




############# INDIVIDUAL OTHER ##############################

idv_cl <- list()

idv_cl[["indv_cleaning_non_other"]] <- indv_cleaning_log %>%  filter(!str_detect(question, "other") | is.na(question))
indv_cleaning_other <- indv_cleaning_log %>%  filter(str_detect(question, "other"))



indv_cleaning_other <- indv_cleaning_other %>% mutate(parent_name = str_replace_all(question,"_other","")) 


indv_cleaning_other  <- indv_cleaning_other %>% left_join((kobo_tool %>% select(type,name)),by = c("parent_name" = "name")) 


indv_cleaning_other <- indv_cleaning_other%>% mutate(
  q_type = case_when(grepl("select_multiple",type) ~ "select_multiple", 
                     grepl("select_one",type) ~ "select_one",
                     T~type)
) %>% filter(!is.na(q_type)) %>% mutate(
  choice_list = type %>% str_replace_all("select_one ","") %>% str_replace_all("select_multiple ","")
)


idv_cl[["indv_blank"]] <- (indv_cleaning_other %>% filter(change_type == "blank_response"))[,1:8] %>% mutate_all(as.character)





indv_other_multi <- indv_cleaning_other  %>% mutate(
  type_of_change = case_when(new_value %in% names(indv_raw) ~ change_type, T ~ "translation")
) 



indv_multiple_other_df_trnas <- indv_other_multi %>% filter(type_of_change ==  "translation")

idv_cl[["select_multiple_translated"]] <- indv_multiple_other_df_trnas[,1:8] %>% mutate_all(as.character)


indv_other_multi <- indv_other_multi%>% filter(change_type == "change_response") ## taking only change_response and getting rid of blank respose

indv_multiple_other_df_change <- indv_other_multi %>% filter(type_of_change ==  "change_response")



### .other 1 to 0 
idv_cl[["multiple_other_1_to_0"]] <- indv_multiple_other_df_change %>% mutate(
  question = paste0(parent_name, ".other"),
  new_value = 0,
  old_value = 1, 
  change_type = "change_response"
)%>% mutate_all(as.character)


### _other to blank response
idv_cl[["multiple_other_blank_re"]] <- indv_multiple_other_df_change %>% mutate(
  change_type = "blank_response",
  new_value = ""
)%>% mutate_all(as.character)



### .other new_value to 1
idv_cl[["multiple_other_0_to_1"]] <- indv_multiple_other_df_change %>% mutate(
  question = new_value,
  change_type = "change_response"
) %>% mutate(
  new_value = 1, 
  old_value =0
) %>% mutate_all(as.character)




indv_cleaning_log <- do.call("bind_rows",idv_cl)

# OUTPUT MUSHT BE SAVED AS indv_cleaning_log 
##########################################################


indv_removal <- indv_raw %>% filter (!X_submission__uuid %in% hh_clean_dataset$X_uuid)

indv_pre_clean <-  indv_raw %>% filter(X_submission__uuid %in% hh_clean_dataset$X_uuid)









b <- check_cleaning_log(df = indv_pre_clean,df_uuid = "repeat_uuid",
                   cl = indv_cleaning_log,
                   cl_change_type_col = "change_type",
                   cl_uuid = "repeat_uuid",
                   cl_change_col = "question",cl_new_val = "new_value")

b
# write.csv(b,"cleaning_log_issues_indv_2022.csv")
# indv_cleaning_log <- indv_cleaning_log %>% filter(!X_uuid %in% b$X_uuid)

################################################################


indv_cleaning_log$change_type %>% table() # There should be no remove_survey



indv_clean_data <- implement_cleaning_log(df = indv_pre_clean,df_uuid = "repeat_uuid",
                                          cl = indv_cleaning_log,
                                          cl_change_type_col = "change_type",
                                          cl_uuid = "repeat_uuid",
                                          cl_change_col = "question",cl_new_val = "new_value")

indv_clean_data <- recalculate_concerted_col_for_select_multiple(df = indv_clean_data,uuid = "repeat_uuid")




# recalculate expenses ----------------------------------------------------

hh_clean_dataset <- hh_clean_dataset %>% mutate(
  arrival_date = case_when(is.na(arrival_date_idp) ~ arrival_date_idp_camp,T~ arrival_date_idp),
  medical_exp = case_when(is.na(medical_exp) ~ medical_exp_camp,T~ medical_exp),
  food_exp = case_when(is.na(food_exp) ~ food_exp_camp,T~ food_exp),
  tot_expenses = case_when(is.na(tot_expenses) ~ tot_expenses_camp,T~ tot_expenses),
  district_origin = case_when(is.na(district_origin) ~ district_origin_camp,T~ district_origin),
  governorate_origin = case_when(is.na(governorate_origin) ~ governorate_origin_camp,T~ governorate_origin),
  idp_first_place = case_when(is.na(idp_first_place) ~ idp_first_place_camp,T~ idp_first_place),
  
)


expense_cols <- c("rent_exp","medical_exp","food_exp")
hh_clean_dataset <- hh_clean_dataset %>% mutate_at(expense_cols,as.integer)

hh_clean_dataset <- hh_clean_dataset %>% mutate(
  calc_expenditure = rowSums(hh_clean_dataset[expense_cols],na.rm = T)
)



hh_clean_dataset$date_assessment <- hh_clean_dataset$date_assessment %>% as.Date()
hh_clean_dataset$end_date <- hh_clean_dataset$end  %>% lubridate::dmy_hm() %>% as.Date()
hh_clean_dataset$date_assessment <- case_when(hh_clean_dataset$date_assessment < as.Date("05/01/22",format = "%m/%d/%y") ~hh_clean_dataset$end_date, T~hh_clean_dataset$date_assessment)

####################################################################################################################

hh_cleaning_log <- hh_cleaning_log_wthout_rm %>% bind_rows(hh_remove_survey_clean)

final_cleaning_log <- hh_cleaning_log %>% bind_rows(indv_cleaning_log)



abu_al_uuid <- (hh_clean_dataset %>% filter(district_mcna == "abu_al_khaseeb"))$X_uuid # remove abu_al_khaseeb

hh_raw <- hh_raw %>% filter(!X_uuid %in% abu_al_uuid)
indv_raw <- indv_raw %>% filter(!X_submission__uuid %in% abu_al_uuid)
final_cleaning_log <- final_cleaning_log %>% filter(!X_uuid %in% abu_al_uuid)
hh_clean_dataset <- hh_clean_dataset %>% filter(!X_uuid %in% abu_al_uuid)
indv_clean_data <- indv_clean_data %>% filter(!X_submission__uuid %in% abu_al_uuid)



### Remove relationship error #######
indv_raw <- indv_raw %>% filter(relationship != "error" )
indv_clean_data <- indv_clean_data %>% filter(relationship != "error" )


##################################


#### fixing adult,child, issue ################333


indv_clean_data <- indv_clean_data %>% mutate(
male_2 = case_when(age > 2 & sex == "male" ~ 1,T~0),
female_2 = case_when(age > 2 & sex == "female" ~ 1,T~0),
male_3_5 = case_when(age %in% 3:5 & sex == "male" ~ 1,T~0),
female_3_5 = case_when(age %in% 3:5 & sex == "female" ~ 1,T~0),

male_3_5 = case_when(age %in% 3:5 & sex == "male" ~ 1,T~0),
female_3_5 = case_when(age %in% 3:5 & sex == "female" ~ 1,T~0),

male_6_17 = case_when(age %in% 6:17 & sex == "male" ~ 1,T~0),
female_6_17 = case_when(age %in% 6:17 & sex == "female" ~ 1,T~0),

male_18_59 = case_when(age %in% 18:59 & sex == "male" ~ 1,T~0),
female_18_59 = case_when(age %in% 18:59 & sex == "female" ~ 1,T~0),

female_1549 = case_when(age %in% 15:49 & sex == "female" ~ 1,T~0),


male_60 = case_when(age > 59 & sex == "male" ~ 1,T~0),
female_60 = case_when(age > 59 & sex == "female" ~ 1,T~0),


child_012 = case_when(age %in% 0:12 ~1,T~0),
child_1317 = case_when(age %in% 13:17 ~1,T~0),
child = case_when(age < 18 ~1,T~0),

female_15_49 == case_when(age %in% 15:49 & sex == "female" ~ 1,T~0)


) 



indv_clean_data_summary <- indv_clean_data %>% group_by(X_submission__uuid) %>% summarise(
  male_2_calc = sum(male_2,na.rm = T),
  female_2_calc = sum(female_2,na.rm = T),
  
  male_3_5_calc = sum(male_3_5,na.rm = T),
  female_3_5_calc = sum(female_3_5,na.rm = T),
  
  male_6_17_calc = sum(male_6_17,na.rm = T),
  female_6_17_calc = sum(female_6_17,na.rm = T),
  
  
  male_18_59_calc = sum(male_18_59,na.rm = T),
  female_18_59_calc = sum(female_18_59,na.rm = T),
  
  
  
  male_60_calc = sum(male_60,na.rm = T),
  female_60_calc = sum(female_60,na.rm = T),
  
  tot_child = sum(child,na.rm = T),
  tot_female_1549 = sum(female_1549,na.rm = T),
  num_family_member  = n()
  
  ) %>% ungroup() %>% mutate(
    tot_male = male_2_calc +male_3_5_calc+male_6_17_calc+male_18_59_calc+male_60_calc,
    tot_female =  female_2_calc +female_3_5_calc+female_6_17_calc+female_18_59_calc+female_60_calc ,
    tot_adult = male_18_59_calc + female_18_59_calc + male_60_calc +female_60_calc,
    tot_6_above = male_6_17_calc + female_6_17_calc + male_18_59_calc +female_18_59_calc + male_60_calc +female_60_calc
) %>% rename(X_uuid = X_submission__uuid)



indv_rem_nams <- names(indv_clean_data_summary)[!names(indv_clean_data_summary) %in% "X_uuid"]

hh_clean_dataset <- hh_clean_dataset %>% select(-indv_rem_nams)

hh_clean_dataset <- hh_clean_dataset %>% left_join(indv_clean_data_summary)

###############################################


########### CHeck adult child numbers #################33
# 
# 
# hh_clean_dataset$tot_adult %>% sum() + hh_clean_dataset$tot_child %>% sum()
# indv_clean_data %>% nrow()



# hh_clean_dataset$num_family_member <- hh_clean_dataset$num_family_member %>% as.numeric()
# hh_clean_dataset$num_hh_member <- hh_clean_dataset$num_hh_member %>% as.numeric()
# 
# hh_clean_dataset$num_family_member %>% sum(na.rm = T) -
# hh_clean_dataset$num_hh_member %>% sum(na.rm = T)


################################################3333




# fixing informal site issue for camp -------------------------------------


hh_clean_dataset <- hh_clean_dataset %>% mutate(
  informal_site = case_when(pop_group == "In-camp IDP HH" ~ NA_character_,T~informal_site)
)










output_hh[["hh_raw"]] <- hh_raw
output_hh[["indv_raw"]] <- indv_raw
output_hh[["final_cleaning_log"]] <- final_cleaning_log
# output_hh[["hh_clean_dataset"]] <- hh_clean_dataset
# output_hh[["indv_clean_data"]] <- indv_clean_data

write_excel_as_reach_format(output_hh,"03_outputs/09_clean_dataset/raw_data.xlsx")
write.csv(hh_clean_dataset,"03_outputs/09_clean_dataset/hh.csv")
write.csv(indv_clean_data,"03_outputs/09_clean_dataset/indv.csv")

####################################################################################################################


