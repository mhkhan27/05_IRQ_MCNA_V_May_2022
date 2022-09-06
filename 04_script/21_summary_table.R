rm(list = ls())

library(dplyr)
library(tidyr)
library(illuminate)
library(stringr)
library(openxlsx)
library(readxl)
library(srvyr)
library(purrr)



# read_data ---------------------------------------------------------------

read_all_sheet_as_csv_format("03_outputs/10_analysis/2022_09_01_mcna_analysis.xlsx")
hh_with_weight <- hh_with_weight %>% select(survey_weight,strata_and_pop_group,pop_group,Stratification,everything())
hh_with_weight <- hh_with_weight[,1:817]
hh_with_weight$governorate  %>% unique()
hh_with_weight$governorate <- hh_with_weight$governorate %>% str_replace_all("ninewa","Ninewa")


indv_with_weight <- indv_with_weight %>% left_join((hh_with_weight %>% select(X_uuid,governorate)))

kobo_path <- "02_ToR_tools_Dap/01_tool/IRQ2206_Tool_MCNA X_transl_inc_CampProfiling_June 3rd_DEPLOYED - Copy with corrected rCSI labels.xlsx"
read_all_sheet_as_csv_format(kobo_path)



hh_with_weight <- fix_data_type_frm_kobo(df = hh_with_weight,kobo_survey_sheet = survey)
indv_with_weight <- fix_data_type_frm_kobo(df = indv_with_weight,kobo_survey_sheet = survey)



hh_svy <- as_survey(hh_with_weight,strata = "strata_and_pop_group", weight = "survey_weight")
indv_svy <- as_survey(indv_with_weight,strata = "strata_and_pop_group", weight = "survey_weight")

calc_cols <- hh_with_weight %>% select(starts_with("calc_")) %>% names()
calc_cols_ends <- hh_with_weight %>% select(ends_with("_calc")) %>% names()

colst_remove <- c(calc_cols,calc_cols_ends,"X","relationship","age_member","age","calc_separated","camp_name","ngo_label","consent","date_assessment",
                  "governorate_mcna","arrival_date","Stratification", "pop_group","strata_and_pop_group",
                  "further_contact_willing", "deviceid","ngo_name","enumerator_num","enumerator_gender","age_respondent","subdistrict_origin_camp",
                  "cluster_location_id","displace_status","displace_status_returnee","displace_status_idp_out","district_origin_camp"
                  ,"governorate_origin_camp","survey_weight","repeat_uuid","index",
                  "displace_date_idp","arrival_date_idp","will_to_response","district_mcna","idp_camp")



cols_not_to_ana_hh <- find_qualitative_cols(df = hh_with_weight,kobo_survey_sheet = survey,
                                         additional_cols_to_remove = colst_remove)
cols_not_to_ana_indv <- find_qualitative_cols(df = indv_with_weight,kobo_survey_sheet = survey,
                                            additional_cols_to_remove = c(colst_remove,"governorate"))

cols_to_ana_hh <- names(hh_with_weight)[!names(hh_with_weight) %in% cols_not_to_ana_hh]
cols_to_ana_indv <- names(indv_with_weight)[!names(indv_with_weight) %in% cols_not_to_ana_indv]



# hh level ----------------------------------------------------------------


overall_summary_hh<- survey_analysis(df = hh_svy,vars_to_analyze = cols_to_ana_hh,kobo_path = kobo_path,question_lable = T)
pop_grp_summary_hh<- survey_analysis(df = hh_svy,vars_to_analyze = cols_to_ana_hh,kobo_path = kobo_path,question_lable = T,disag = "pop_group")

district_summary_hh<- survey_analysis(df = hh_svy,vars_to_analyze = cols_to_ana_hh,kobo_path = kobo_path,question_lable = T,disag = "Stratification")
governorate_summary_hh<- survey_analysis(df = hh_svy,vars_to_analyze = cols_to_ana_hh,kobo_path = kobo_path,question_lable = T,disag = "governorate")


district_pop_group_summary_hh<- survey_analysis(df = hh_svy,vars_to_analyze = cols_to_ana_hh,kobo_path = kobo_path,question_lable = T,disag = c("pop_group","Stratification"))
governorate_pop_group_summary_hh<- survey_analysis(df = hh_svy,vars_to_analyze = cols_to_ana_hh,kobo_path = kobo_path,question_lable = T,disag =c("pop_group", "governorate"))


# indv level --------------------------------------------------------------


overall_summary_indv<- survey_analysis(df = indv_svy,vars_to_analyze = cols_to_ana_indv,kobo_path = kobo_path,question_lable = T)
pop_grp_summary_indv<- survey_analysis(df = indv_svy,vars_to_analyze = cols_to_ana_indv,kobo_path = kobo_path,question_lable = T,disag = "pop_group")

district_summary_indv<- survey_analysis(df = indv_svy,vars_to_analyze = cols_to_ana_indv,kobo_path = kobo_path,question_lable = T,disag = "Stratification")
governorate_summary_indv<- survey_analysis(df = indv_svy,vars_to_analyze = cols_to_ana_indv,kobo_path = kobo_path,question_lable = T,disag = "governorate")


district_pop_group_summary_indv<- survey_analysis(df = indv_svy,vars_to_analyze = cols_to_ana_indv,kobo_path = kobo_path,question_lable = T,disag = c("pop_group","Stratification"))
governorate_pop_group_summary_indv<- survey_analysis(df = indv_svy,vars_to_analyze = cols_to_ana_indv,kobo_path = kobo_path,question_lable = T,disag =c("pop_group", "governorate"))






# combine -----------------------------------------------------------------

overall_summary <- overall_summary_hh %>% bind_rows(overall_summary_indv)
pop_grp_summary <- pop_grp_summary_hh %>% bind_rows(pop_grp_summary_indv)


district_summary <- district_summary_hh %>% bind_rows(district_summary_indv)

governorate_summary <- governorate_summary_hh %>% bind_rows(governorate_summary_indv)
district_pop_group_summary <- district_pop_group_summary_hh %>% bind_rows(district_pop_group_summary_indv)
governorate_pop_group_summary <- governorate_pop_group_summary_hh %>% bind_rows(governorate_pop_group_summary_indv)


# write
write_list <- list()

write_list[["overall_summary"]] <- overall_summary
write_list[["pop_grp_summary"]] <-pop_grp_summary


write_list[["district_summary"]] <- district_summary

write_list[["governorate_summary"]] <- governorate_summary
write_list[["district_pop_group_summary"]] <- district_pop_group_summary
write_list[["governorate_pop_group_summary"]] <-governorate_pop_group_summary


write_excel_as_reach_format(write_list,"03_outputs/10_analysis/summary.xlsx")
                        