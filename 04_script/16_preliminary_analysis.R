rm(list = ls())


# read library ------------------------------------------------------------
library(dplyr)
library(srvyr)
library(illuminate)
library(stringr)
library(openxlsx)
library(readxl)
library(purrr)
library(expss)
options(scipen = 999)


# read data and tool ------------------------------------------------------


kobo_path <- "02_ToR_tools_Dap/01_tool/IRQ2206_Tool_MCNA X_transl_inc_CampProfiling_June 3rd_DEPLOYED - Copy with corrected rCSI labels.xlsx"
read_all_sheet_as_csv_format(kobo_path)
read_all_sheet_as_csv_format("03_outputs/09_clean_dataset/raw_data.xlsx")

hh_dap <- read.csv("02_ToR_tools_Dap/02_dap/preliminary_household_dap.csv",na.strings = "")[1:5] %>% rename(main_variable = dependent_variable ) %>% select(-X) %>% filter(!is.na(main_variable))
indv_dap <- read.csv("02_ToR_tools_Dap/02_dap/preliminary_individual_dap.csv")[1:4]  %>% rename(main_variable = dependent_variable )

PII <- c("enumerator_name_id","enumerator_name","referral_contact",
         "enumerator_num","referral_name","contact_details_name","contact_details_telephone",
         "gpslocation","X_gpslocation_latitude","X_gpslocation_longitude","audit_URL","enumerator_name","X_gpslocation_altitude")

hh_raw <- hh_raw %>% select(-PII)


df <- read.csv("03_outputs/09_clean_dataset/hh.csv",na.strings = c(""," ","NA"))

df <-  df %>% filter(consent == "yes" & calc_eligible == 1)

df <- df %>% select(-PII)


df <-df %>% rename(
  consuming_less_per_meal1 = borrow_food,
 reduce_for_adults1 = reduce_meals, 
 reduce_meals1 = consuming_less_per_meal,
 skip_whole_days1 = reduce_for_adults
) %>% rename(
  reduce_for_adults =reduce_for_adults1,
  consuming_less_per_meal =consuming_less_per_meal1,
  reduce_meals =  reduce_meals1 ,
  skip_whole_days =skip_whole_days1 
)


loop <- read.csv("03_outputs/09_clean_dataset/indv.csv",na.strings = c(""," ","NA",NA)) %>% rename(X_uuid=X_submission__uuid)
loop <- loop %>% filter(X_uuid %in% df$X_uuid)

# fix data format ---------------------------------------------------------

df <- fix_data_type_frm_kobo(df = df,kobo_survey_sheet = survey,remove_all_NA_col = T)
loop <- fix_data_type_frm_kobo(df = loop,kobo_survey_sheet = survey,remove_all_NA_col = T)


# recoding ----------------------------------------------------------------


source("04_script/17_recoding.R") # creating additional variables


# weighting and analysis --------------------------------------------------


df <- df %>% mutate(
  Stratification = case_when(pop_group == "In-camp IDP HH" ~ camp_name, T ~ district_mcna)
)
loop <- loop %>% left_join((df %>% select(X_uuid,Stratification,pop_group))) 


weights <- list()

pop_group <- c("In-camp IDP HH","Out-of-camp IDP HH","Returnee HH","Host Community HH")

pop_group %in% df$pop_group

for(i in pop_group) {
  
  hh_pop <- df %>% filter(pop_group == i)
  hh_pop <- fix_data_type_frm_kobo(df = hh_pop,kobo_survey_sheet = survey,remove_all_NA_col = T)
  
  indv_pop <- loop %>% filter(pop_group == i)
  indv_pop <- fix_data_type_frm_kobo(df = indv_pop,kobo_survey_sheet = survey,remove_all_NA_col = T)
  

# read sample frame -------------------------------------------------------

  if(i == "Out-of-camp IDP HH") {
    sample_frame_summary <- read.csv("03_outputs/01_sampling/02_sampling/02_IDPs/01_out_camp/sampling_summary.csv"
    ) %>% mutate(pop_indv = Population*6 )
    sample_frame_summary$Stratification <- sample_frame_summary$Stratification %>% snakecase::to_snake_case()
    
    
    sample_frame_summary <- sample_frame_summary %>% filter(Stratification != "abu_al_khaseeb") # remove abu al khaseeb
     
    }
  
  if(i == "In-camp IDP HH"){
    sample_frame_summary <- read.csv("03_outputs/01_sampling/02_sampling/02_IDPs/02_in_camp/sampling_summary.csv") 
    
    sample_frame_summary$Stratification <- sample_frame_summary$Stratification %>% snakecase::to_snake_case()
    
    sample_frame_summary <- sample_frame_summary%>%
      mutate(pop_indv = Population*5,
             Stratification = case_when(Stratification == "qayyarah_jad_ah_5" ~ "qayyarah_jadah_five" ,
                                        Stratification == "khazer_m_1" ~ "khazer_m1" ,
                                        Stratification == "hasansham_u_2" ~ "hasansham_u2" ,
                                        Stratification == "hasansham_u_3" ~ "hasansham_u3" ,
                                        Stratification == "berseve_1" ~ "bersive_1" ,
                                        Stratification == "berseve_2" ~ "bersive_2" ,
                                        Stratification == "bajet_kandala" ~ "bajed_kandala" ,
                                        Stratification == "dawadia" ~ "dawoudia" ,
                                        T~ Stratification
                                        )) 
    
    }
  
  
  if(i == "Returnee HH")  {
    sample_frame_summary <- read.csv("03_outputs/01_sampling/02_sampling/03_Returnees/sampling_summary.csv") %>% 
      mutate(pop_indv = Population*6)
    sample_frame_summary$Stratification <- sample_frame_summary$Stratification %>% snakecase::to_snake_case()
    
    }
  
  if(i == "Host Community HH")  {
   
     sample_frame_summary <- read.csv("03_outputs/01_sampling/02_sampling/01_host_community/sampling_summary.csv") %>% 
       mutate(pop_indv = Population*5)
     sample_frame_summary$Stratification <- sample_frame_summary$Stratification %>% snakecase::to_snake_case()
     
     }
 

# checking names  ---------------------------------------------------------

  unique(hh_pop$Stratification) %in% sample_frame_summary$Stratification %>% table()
  
  unique(hh_pop$Stratification)[!unique(hh_pop$Stratification) %in% sample_frame_summary$Stratification] 


# calculate weights -------------------------------------------------------

   weights_hh <- hh_pop %>% group_by(Stratification) %>% summarise(
    survey_count = n()
  ) %>% left_join(sample_frame_summary) %>%
    mutate(sample_global=sum(survey_count),
           pop_global=sum(Population),
           survey_weight= (Population/pop_global)/(survey_count/sample_global)) %>% select(
             Stratification,Population,pop_global,survey_count,sample_global,survey_weight,X..surveys,Effective.sample)
  
  # weights_only_hh <- weights_hh %>% select(Stratification,survey_weight)
  
  
  # hh_with_weight <- hh_pop %>% left_join(weights_only_hh)
  
  # hh_with_weight$survey_weight %>% is.na() %>% table()
  
  
  ###################### indv Weights ######################
 
  indv_pop$Stratification[!indv_pop$Stratification %in% sample_frame_summary$Stratification]
  
   weights_indv <- indv_pop %>% group_by(Stratification) %>% summarise(
    survey_count = n()
  ) %>% left_join(sample_frame_summary %>% select(Stratification,pop_indv)) %>% 
    mutate(sample_global=sum(survey_count),
           pop_global=sum(pop_indv),
           survey_weight= (pop_indv/pop_global)/(survey_count/sample_global)) %>%
    select(Stratification,pop_indv,pop_global,survey_count,sample_global,survey_weight)
   
  # weights_only_indv <- weights_hh %>% select(Stratification,survey_weight)
  # indv_with_weight <- indv_pop %>% left_join(weights_only_indv)
  


  weights_hh_c <-weights_hh %>% rename(family_total= pop_global,
                                       family_surveyed = survey_count,
                                       family_surveyed_total = sample_global,
                                       hh_weights= survey_weight)
  weights_indv_c <- weights_indv %>% rename(indv_total= pop_global,
                                            indv_surveyed = survey_count,
                                            indv_surveyed_total = sample_global,
                                            indv_weights= survey_weight)

  weights[[i]]  <- weights_hh_c %>% left_join(weights_indv_c) %>% mutate(
    pop_group = i
  )
  
  
}

weighting_frame <- do.call("bind_rows",weights) %>% select(Stratification,pop_group,everything()) %>% rename(
  targeted_survey_with_buffer = X..surveys
)

weighting_frame_hh <- weighting_frame %>% select(pop_group,Stratification,hh_weights)
weighting_frame_indv <- weighting_frame %>% select(pop_group,Stratification,indv_weights)

hh_with_weight <- df %>% left_join(weighting_frame_hh,by = c("pop_group","Stratification")) %>% rename(survey_weight = hh_weights) %>% mutate(
  strata_and_pop_group = paste0(pop_group, "_",Stratification)
)
indv_with_weight <- loop %>% left_join(weighting_frame_indv,by = c("pop_group","Stratification")) %>% rename(survey_weight = indv_weights) %>% mutate(
  strata_and_pop_group = paste0(pop_group, "_",Stratification)
)


hh_with_weight <- hh_with_weight %>%
  select_if(function(x) !all(is.na(x)))

indv_with_weight <- indv_with_weight %>%
  select_if(function(x) !all(is.na(x)))


# 
# hh_logical_cols <- hh_with_weight %>%
#   select_if(function(x) all(unique(x) %in% c(0,1))) %>% names()
# 
# hh_with_weight <- hh_with_weight %>% mutate_at(hh_logical_cols,as.logical)
# 
# 
# indv_logical_cols <- indv_with_weight %>%
#   select_if(function(x) all(unique(x) %in% c(0,1))) %>% names()
# 
# indv_with_weight <- indv_with_weight %>% mutate_at(indv_logical_cols,as.logical)


#### CHECK IF ALL OF THE DAP IS COVERING OR NOT 

hh_dap$main_variable [!hh_dap$main_variable %in% names(hh_with_weight)]
indv_dap$main_variable [!indv_dap$main_variable %in% names(indv_with_weight)]


remove <- c("tot_child", "tot_male", "tot_female_1549", "tot_female", "tot_adult",
            "tot_6_above","dif_reason","district_true","sub_district_true","not_residing_num",
            "reasons_rs","governorate_true","num_family_member",
            "male_2",
            "female_2",
            "male_3_5",
            "female_3_5",
            "male_6_17",
            "female_6_17",
            "male_18_59",
            "female_18_59",
            "female_1549",
            "male_60",
            "female_60",
            "separated",
            "child_012",
            "child_1317",
            "child")

remove <- remove[remove %in% names(hh_with_weight)]


hh_with_weight <- hh_with_weight %>% select(-remove)




####################################[ANALYSIS]##################################


# hh level analysis -------------------------------------------------------

 #  additional_remove_hh <- c("ngo_label","idp_camp","calc_idp","calc_returnee","calc_host", "calc_eligible_camp","calc_eligible_noncamp",
 #                            "calc_eligible_returnee","cluster_location_id","governorate_mcna","camp_name","date_assessment","X","enumerator_num",
 #                            "enumerator_gender","survey_weight","audit_URL","instance_name","formatted_date_assessment","consent","verification_site",
 #                           "ngo_name","calc_eligible_host","calc_eligible", "will_to_response",
 #                           "district","district_mcna","governorate","ï._start","ï..Ã.._start","ï..start")
 # 
 # additional_remove_indv <- c("index","X","ï._index","repeat_uuid", "survey_weight","Stratification")

  
  # hh_cols_remove_cols<- find_qualitative_cols(df = hh_with_weight,kobo_survey_sheet = survey, additional_cols_to_remove = additional_remove_hh)
  # cols_to_ana <- hh_with_weight %>% select(-hh_cols_remove_cols) %>% names()

cols_to_ana <- hh_dap$main_variable %>% unique()


hh_svy <- as_survey(hh_with_weight,strata = "strata_and_pop_group", weight = "survey_weight")


  indv_svy <- as_survey(indv_with_weight,strata = "strata_and_pop_group", weight = "survey_weight")
  # indv_cols_remove_cols<- find_qualitative_cols(df = indv_with_weight,kobo_survey_sheet = survey, additional_cols_to_remove = additional_remove_indv)
  # indv_cols_to_ana <- indv_with_weight %>% select(-indv_cols_remove_cols) %>% names()
  
  indv_cols_to_ana <- indv_dap$main_variable  %>% unique()
  
  # National level analysis
  overall_analysis_hh <- survey_analysis(df = hh_svy,vars_to_analyze = cols_to_ana,
                                         kobo_path = kobo_path,
                                         question_lable = T,sm_sep = "/")  %>% arrange(main_variable) %>% left_join(hh_dap) %>%  select(names(hh_dap),everything()) %>% 
    select (- contains("Count"), -"n_unweighted")
  
  overall_analysis_indv <- survey_analysis(df = indv_svy,vars_to_analyze = indv_cols_to_ana,
                                           kobo_path = kobo_path,
                                           question_lable = T,sm_sep = "/") %>% left_join(indv_dap) %>%  select(names(indv_dap),everything()) %>% arrange(Sector,main_variable) %>% 
    select (- contains("Count"), -"n_unweighted")
  
  # Strata (population group + district/camp) level analysis
  ana_by_strata_hh <- survey_analysis(df = hh_svy,vars_to_analyze = cols_to_ana,
                                      kobo_path = kobo_path,
                                      question_lable = T,sm_sep = "/",disag = c("pop_group","Stratification"))  %>% left_join(hh_dap)  %>% 
                                      arrange(Sector,main_variable) %>%
                                     select(names(hh_dap),everything()) %>%   select (- contains("Count"), -"n_unweighted")
  
  # ana_by_strata_hh$subset_1_val <- ana_by_strata_hh$subset_1_val %>% snakecase::to_sentence_case()
  ana_by_strata_hh$subset_2_val <- ana_by_strata_hh$subset_2_val %>% snakecase::to_sentence_case() %>% 
    stringr::str_replace_all("Al ","Al-")
  
  ana_by_strata_indv <- survey_analysis(df = indv_svy,vars_to_analyze = indv_cols_to_ana,
                                        kobo_path = kobo_path,
                                        question_lable = T,sm_sep = "/",disag = c("pop_group","Stratification")) %>% 
                                        left_join(indv_dap)  %>% 
                                        arrange(Sector,main_variable) %>% 
                                        select(names(indv_dap),everything()) %>%  select (- contains("Count"), -"n_unweighted")
  
  
  # ana_by_strata_indv$subset_1_val <- ana_by_strata_indv$subset_1_val %>% snakecase::to_sentence_case()
  ana_by_strata_indv$subset_2_val <- ana_by_strata_indv$subset_2_val %>% snakecase::to_sentence_case() %>% 
    stringr::str_replace_all("Al ","Al-")
  
  # population group  level analysis
  ana_by_pop_grp_hh <- survey_analysis(df = hh_svy,vars_to_analyze = cols_to_ana,
                                      kobo_path = kobo_path,
                                      question_lable = T,sm_sep = "/",disag = "pop_group") %>% left_join(hh_dap) %>% arrange(Sector,main_variable) %>%   
    select(names(hh_dap),everything()) %>%  select (- contains("Count"), -"n_unweighted")
  
  # ana_by_pop_grp_hh$subset_1_val <- ana_by_pop_grp_hh$subset_1_val %>% snakecase::to_sentence_case()
  
  ana_by_pop_grp_indv <- survey_analysis(df = indv_svy,vars_to_analyze = indv_cols_to_ana,
                                        kobo_path = kobo_path,
                                        question_lable = T,sm_sep = "/",disag = "pop_group") %>% left_join(indv_dap)  %>% arrange(Sector,main_variable) %>% 
      select(names(indv_dap),everything()) %>%   select (- contains("Count"), -"n_unweighted")
  
  # ana_by_pop_grp_indv$subset_1_val <- ana_by_pop_grp_indv$subset_1_val %>% snakecase::to_sentence_case()
  
  
  
  
  # population group  level analysis
  ana_by_district_hh<- survey_analysis(df = hh_svy,vars_to_analyze = cols_to_ana,
                                       kobo_path = kobo_path,
                                       question_lable = T,sm_sep = "/",disag = "Stratification") %>% left_join(hh_dap) %>% arrange(Sector,main_variable) %>%   
    select(names(hh_dap),everything()) %>%  select (- contains("Count"), -"n_unweighted")
  
  ana_by_district_hh$subset_1_val <- ana_by_district_hh$subset_1_val %>% snakecase::to_sentence_case() %>% 
    stringr::str_replace_all("Al ","Al-")
  
  ana_by_district_indv <- survey_analysis(df = indv_svy,vars_to_analyze = indv_cols_to_ana,
                                         kobo_path = kobo_path,
                                         question_lable = T,sm_sep = "/",disag = "Stratification") %>% left_join(indv_dap)  %>% arrange(Sector,main_variable) %>% 
    select(names(indv_dap),everything()) %>%   select (- contains("Count"), -"n_unweighted")
  
  ana_by_district_indv$subset_1_val <- ana_by_district_indv$subset_1_val %>% snakecase::to_sentence_case() %>% 
    stringr::str_replace_all("Al ","Al-")
  
  write_list2 <- list(
    ana_by_district_hh = "ana_by_district_hh",
    ana_by_district_indv ="ana_by_district_indv"
  )
  write_excel_as_reach_format(write_list2,paste0("03_outputs/10_analysis/",str_replace_all(Sys.Date(),"-","_"),"_mcna_analysis_2.xlsx"))

  
  
  
  
  
  ########################################################### Write outputs ###############################################
  
  cleaning_log <- final_cleaning_log %>% filter(change_type != "remove_survey")
  deletion_log <- final_cleaning_log %>% filter(change_type == "remove_survey")
  
  
  
  # hh_raw <- hh_raw %>% select(-skip_whole_days)
  hh_with_weight <- hh_with_weight %>% select(-skip_whole_days)
  hh_with_weight$district_mcna <- hh_with_weight$district_mcna  %>% snakecase::to_title_case() %>% 
    stringr::str_replace_all("Al ","Al-")

  
  
  write_list <- list(
    hh_raw = "hh_raw",
    indv_raw="indv_raw",
    cleaning_log = "cleaning_log",
    deletion_log="deletion_log",
    hh_with_weight = "hh_with_weight",
    indv_with_weight = "indv_with_weight",
    weighting_frame="weighting_frame",
    overall_analysis_hh= "overall_analysis_hh",
    ana_by_strata_hh ="ana_by_strata_hh",
    ana_by_pop_grp_hh ="ana_by_pop_grp_hh",
    overall_analysis_indv= "overall_analysis_indv",
    ana_by_strata_indv ="ana_by_strata_indv",
    ana_by_pop_grp_indv ="ana_by_pop_grp_indv",
    ana_by_district_hh = "ana_by_district_hh",
    ana_by_district_indv ="ana_by_district_indv"
  )
  
  write_excel_as_reach_format(write_list,paste0("03_outputs/10_analysis/",str_replace_all(Sys.Date(),"-","_"),"_mcna_analysis.xlsx"))  
  # write_excel_as_reach_format(write_list,paste0("C:\\Users\\rakib\\OneDrive - ACTED\\MCNA 2022\\7. ECHO/",str_replace_all(Sys.Date(),"-","_"),"_mcna_analysis.xlsx"))  
  # write_excel_as_reach_format(write_list,paste0("/Users/mehedi/Library/CloudStorage/OneDrive-ACTED/MCNA 2022/7. ECHO/",str_replace_all(Sys.Date(),"-","_"),"_mcna_analysis.xlsx"))  
  
  


  


