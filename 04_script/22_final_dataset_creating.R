rm(list = ls())

library(dplyr)
library(tidyr)
library(illuminate)
library(stringr)
library(openxlsx)
library(readxl)
library(naniar)


# read_data ---------------------------------------------------------------

read_all_sheet_as_csv_format("03_outputs/10_analysis/2022_09_01_mcna_analysis.xlsx")
read_all_sheet_as_csv_format("03_outputs/10_analysis/2022_09_04_mcna_analysis_2.xlsx")

read_all_sheet_as_csv_format("02_ToR_tools_Dap/01_tool/IRQ2206_Tool_MCNA X_transl_inc_CampProfiling_June 3rd_DEPLOYED - Copy with corrected rCSI labels.xlsx")




## kobo tool 
kobo_type <- survey %>% select(type,name) %>% mutate(
  `Question type` = word(type,1)
) %>% rename(main_variable=name) %>% select(main_variable,`Question type` )

####






to_write <- list()
# to_write[["hh_raw"]] <- hh_raw
# to_write[["indv_raw"]] <- indv_raw
# to_write[["cleaning_log"]] <- cleaning_log
# to_write[["deletion_log"]] <- deletion_log
# to_write[["hh_with_weight"]] <- hh_with_weight
# to_write[["indv_with_weight"]] <- indv_with_weight


#################################################### Start:: Preliminary - Nationwide #######################################################

############### Analysis_by_pop_group and overall- hh level #######################

overall_analysis_hh_re <- overall_analysis_hh %>%  select(-c("mean.pct_low", "mean.pct_upp", "choice_label::English", 
                                                             "choice_label::Arabic","label::English", "label::Arabic")) %>% rename(
                                                               "All groups" = "mean.pct"
                                                             )

########## hh by pop group ###############################

ana_by_pop_grp_hh_re <- ana_by_pop_grp_hh %>% select(-c("mean.pct_low", "mean.pct_upp", "choice_label::English", 
                                                        "choice_label::Arabic", "subset_1_name","label::English", "label::Arabic"))

ana_by_pop_grp_hh_pivot <- ana_by_pop_grp_hh_re %>% pivot_wider(values_from = "mean.pct",names_from = "subset_1_val") 



# comning_overall and ana by pop for hh -----------------------------------


hh_national_wide_hh <-overall_analysis_hh_re %>% left_join(ana_by_pop_grp_hh_pivot)

############### Analysis_by_pop_group and overall- hh level #######################

overall_analysis_indv_re <- overall_analysis_indv %>%  select(-c("mean.pct_low", "mean.pct_upp", "choice_label::English", 
                                                             "choice_label::Arabic","label::English", "label::Arabic")) %>% rename(
                                                               "All groups" = "mean.pct"
                                                             )



ana_by_pop_grp_indv_re <- ana_by_pop_grp_indv %>% select(-c("mean.pct_low", "mean.pct_upp", "choice_label::English", 
                                                        "choice_label::Arabic", "subset_1_name","label::English", "label::Arabic"))

ana_by_pop_grp_indv_pivot <- ana_by_pop_grp_indv_re %>% pivot_wider(values_from = "mean.pct",names_from = "subset_1_val") 

indv_national_wide_indv <-overall_analysis_indv_re %>% left_join(ana_by_pop_grp_indv_pivot)


preliminary_national_wide <- hh_national_wide_hh %>% bind_rows(indv_national_wide_indv)

preliminary_national_wide$choice <-preliminary_national_wide$choice %>% snakecase::to_sentence_case() %>% 
  stringr::str_replace_all("Al ","Al-")


preliminary_national_wide <- preliminary_national_wide %>% mutate(
  per_inc = case_when(grepl(pattern = "%|Most commonly|likelihood of at least one member|Barrier to access|Displacement status",research_question) ~ "yes",T~ "no")
)



preliminary_national_wide <- preliminary_national_wide %>% dplyr::mutate(
  `All groups` = case_when(per_inc == "yes" ~ paste0(round(`All groups`*100),"%"), T~as.character(round(`All groups`,2))),
  `Host Community HH` = case_when(per_inc == "yes"~ paste0(round(`Host Community HH`*100),"%"),T~as.character(round(`Host Community HH`))),
  `In-camp IDP HH` = case_when(per_inc == "yes"~ paste0(round(`In-camp IDP HH`*100),"%"),T~as.character(round(`In-camp IDP HH`))),
  `Returnee HH` = case_when(per_inc == "yes"~ paste0(round(`Returnee HH`*100),"%"),T~as.character(round(`Returnee HH`))),
  `Out-of-camp IDP HH` = case_when(per_inc == "yes"~ paste0(round(`Out-of-camp IDP HH`*100),"%"),T~as.character(round(`Out-of-camp IDP HH`)))
)

preliminary_national_wide <- preliminary_national_wide %>% naniar::replace_with_na_all(condition = ~.x == "NA%")


preliminary_national_wide <- preliminary_national_wide  %>% select(-main_variable,-per_inc)


to_write[["preliminary_national_wide"]] <- preliminary_national_wide



#################################################### END:: Preliminary - Nationwide #######################################################

#################################################### Start:: Preliminary - District #######################################################

ana_by_district_hh_re <- ana_by_district_hh %>%  select(-c("mean.pct_low", "mean.pct_upp", "choice_label::English", "subset_1_name", 
                                                            "choice_label::Arabic","label::English", "label::Arabic")) %>% rename(
                                                              "All groups" = "mean.pct",
                                                              "District" = "subset_1_val"
                                                            ) 



ana_by_strata_hh_re <- ana_by_strata_hh %>%  select(-c("mean.pct_low", "mean.pct_upp", "choice_label::English", "subset_1_name",  "subset_2_name", 
                                                         "choice_label::Arabic","label::English", "label::Arabic"))  %>% 
  rename(District = subset_2_val)
# 
#  
ana_by_strata_hh_re %>% duplicated() %>% table()


ana_by_strata_hh_pivot <- ana_by_strata_hh_re %>% pivot_wider(names_from = "subset_1_val",values_from = "mean.pct")


Preliminary_District_hh <- ana_by_district_hh_re %>% left_join(ana_by_strata_hh_pivot)





ana_by_district_indv_re<- ana_by_district_indv %>%  select(-c("mean.pct_low", "mean.pct_upp", "choice_label::English", "subset_1_name", 
                                                           "choice_label::Arabic","label::English", "label::Arabic")) %>% rename(
                                                             "All groups" = "mean.pct",
                                                             "District" = "subset_1_val"
                                                           ) 



ana_by_strata_indv_re <- ana_by_strata_indv %>%  select(-c("mean.pct_low", "mean.pct_upp", "choice_label::English", "subset_1_name",  "subset_2_name", 
                                                       "choice_label::Arabic","label::English", "label::Arabic"))  %>% 
  rename(District = subset_2_val)

ana_by_strata_indv_pivot <- ana_by_strata_indv_re %>% pivot_wider(names_from = "subset_1_val",values_from = "mean.pct")


nrow(ana_by_district_indv_re)
nrow(ana_by_strata_indv_pivot)

Preliminary_District_indv <- ana_by_district_indv_re %>% left_join(ana_by_strata_indv_pivot)

nrow(Preliminary_District_indv)


Preliminary_District <- Preliminary_District_hh %>% bind_rows(Preliminary_District_indv)
nrow(Preliminary_District)


Preliminary_District$choice <-Preliminary_District$choice %>% snakecase::to_title_case() %>% 
  stringr::str_replace_all("Al ","Al-")






Preliminary_District <- Preliminary_District %>% mutate(
  per_inc = case_when(grepl(pattern = "%|Most commonly|likelihood of at least one member|Barrier to access|Displacement status",research_question) ~ "yes",T~ "no")
)



Preliminary_District <- Preliminary_District %>% dplyr::mutate(
  `All groups` = case_when(per_inc == "yes" ~ paste0(round(`All groups`*100),"%"), T~as.character(round(`All groups`,2))),
  `Host Community HH` = case_when(per_inc == "yes"~ paste0(round(`Host Community HH`*100),"%"),T~as.character(round(`Host Community HH`))),
  `In-camp IDP HH` = case_when(per_inc == "yes"~ paste0(round(`In-camp IDP HH`*100),"%"),T~as.character(round(`In-camp IDP HH`))),
  `Returnee HH` = case_when(per_inc == "yes"~ paste0(round(`Returnee HH`*100),"%"),T~as.character(round(`Returnee HH`))),
  `Out-of-camp IDP HH` = case_when(per_inc == "yes"~ paste0(round(`Out-of-camp IDP HH`*100),"%"),T~as.character(round(`Out-of-camp IDP HH`)))
)

Preliminary_District <- Preliminary_District %>% naniar::replace_with_na_all(condition = ~.x == "NA%")

Preliminary_District$District <- Preliminary_District$District %>% snakecase::to_title_case() %>% stringr::str_replace_all("Al ","Al-")


Preliminary_District <- Preliminary_District  %>% select(-main_variable,-per_inc)

to_write[["Preliminary_District"]] <- Preliminary_District 


######################################################### Sumarry table ##############################################

read_all_sheet_as_csv_format("03_outputs/10_analysis/summary.xlsx")

## Nationa wide summary
overall_summary_re <- overall_summary %>% select(-c("mean.pct_low","mean.pct_upp", "n_unweighted","response_count")) %>% rename(
 "All groups" = "mean.pct"
)

pop_grp_summary_re <- pop_grp_summary %>% select(-c("mean.pct_low","mean.pct_upp","response_count","n_unweighted",
                                                    "subset_1_name","count_by_subset"))
# 
# a <- pop_grp_summary_re %>% duplicated() 
# pop_grp_summary_re$a <- a


pop_grp_summary_pivot <- pop_grp_summary_re %>% pivot_wider(names_from =  "subset_1_val",values_from = "mean.pct")

nrow(pop_grp_summary_pivot)
nrow(overall_summary_re)


nationwide_summary <- overall_summary_re %>% left_join(pop_grp_summary_pivot)

nrow(nationwide_summary)


nationwide_summary2 <- nationwide_summary %>% select(-choice) %>% left_join(kobo_type) %>% rename(
  `Question ID` = main_variable, 
   `Question::English`=`label::English`,
  `Question::Arabic` = `label::Arabic`,
   `Choice::English` = `choice_label::English`,
  `Choice::Arabic`= `choice_label::Arabic`
  
) %>% select(`Question ID`, `Question::English`,`Question::Arabic`, `Choice::English`,`Choice::Arabic`,`Question type`,everything()) %>% 
  filter(!is.na(`Question type`))


nationwide_summary3 <- nationwide_summary2 %>% dplyr::mutate(
  `All groups` = case_when(`Question type` %in% c("select_one","select_multiple")~ paste0(round(`All groups`*100),"%"), T~as.character(round(`All groups`,2))),
  `Host Community HH` = case_when(`Question type` %in% c("select_one","select_multiple")~ paste0(round(`Host Community HH`*100),"%"),T~as.character(round(`Host Community HH`))),
  `In-camp IDP HH` = case_when(`Question type` %in% c("select_one","select_multiple")~ paste0(round(`In-camp IDP HH`*100),"%"),T~as.character(round(`In-camp IDP HH`))),
  `Returnee HH` = case_when(`Question type` %in% c("select_one","select_multiple")~ paste0(round(`Returnee HH`*100),"%"),T~as.character(round(`Returnee HH`))),
  `Out-of-camp IDP HH` = case_when(`Question type` %in% c("select_one","select_multiple")~ paste0(round(`Out-of-camp IDP HH`*100),"%"),T~as.character(round(`Out-of-camp IDP HH`)))
)


nationwide_summary <- nationwide_summary3 %>% naniar::replace_with_na_all(condition = ~.x == "NA%")


to_write[["nationwide_summary"]] <- nationwide_summary







#################### governorate wide summary ############# 

governorate_summary
governorate_pop_group_summary

governorate_summary_re <- governorate_summary %>% select(-c("mean.pct_low", "mean.pct_upp", "n_unweighted", "subset_1_name", 
                                                            "count_by_subset", "response_count")) %>% rename(
                                                              "Governorate" = "subset_1_val",
                                                              "All groups" = "mean.pct"
                                                            )

governorate_pop_group_summary_re <- governorate_pop_group_summary %>% 
  select(-c( "mean.pct_low", "mean.pct_upp", "n_unweighted", 
             "subset_1_name", "subset_2_name", "count_by_subset", "response_count")) %>% rename(
               Governorate = subset_2_val
             )

governorate_pop_group_summary_pivot <- governorate_pop_group_summary_re %>% pivot_wider(names_from = "subset_1_val",
                                                                                        values_from = "mean.pct")

nrow(governorate_pop_group_summary_pivot)
nrow(governorate_summary_re)


governorate_wide_summary <- governorate_summary_re %>% left_join(governorate_pop_group_summary_pivot)
nrow(governorate_wide_summary)






governorate_wide_summary2 <- governorate_wide_summary %>% select(-choice) %>% left_join(kobo_type) %>% rename(
  `Question ID` = main_variable, 
  `Question::English`=`label::English`,
  `Question::Arabic` = `label::Arabic`,
  `Choice::English` = `choice_label::English`,
  `Choice::Arabic`= `choice_label::Arabic`
  
) %>% select(Governorate,`Question ID`, `Question::English`,`Question::Arabic`, `Choice::English`,`Choice::Arabic`,`Question type`,everything()) %>% 
  filter(!is.na(`Question type`))



governorate_wide_summary3 <- governorate_wide_summary2 %>% dplyr::mutate(
  `All groups` = case_when(`Question type` %in% c("select_one","select_multiple")~ paste0(round(`All groups`*100),"%"), T~as.character(round(`All groups`,2))),
  `Host Community HH` = case_when(`Question type` %in% c("select_one","select_multiple")~ paste0(round(`Host Community HH`*100),"%"),T~as.character(round(`Host Community HH`))),
  `In-camp IDP HH` = case_when(`Question type` %in% c("select_one","select_multiple")~ paste0(round(`In-camp IDP HH`*100),"%"),T~as.character(round(`In-camp IDP HH`))),
  `Returnee HH` = case_when(`Question type` %in% c("select_one","select_multiple")~ paste0(round(`Returnee HH`*100),"%"),T~as.character(round(`Returnee HH`))),
  `Out-of-camp IDP HH` = case_when(`Question type` %in% c("select_one","select_multiple")~ paste0(round(`Out-of-camp IDP HH`*100),"%"),T~as.character(round(`Out-of-camp IDP HH`)))
)


governorate_wide_summary <- governorate_wide_summary3 %>% naniar::replace_with_na_all(condition = ~.x == "NA%")


to_write[["governorate_wide_summary"]] <- governorate_wide_summary


#################### district wide summary ############# 

district_summary
district_pop_group_summary


district_summary_re <- district_summary %>% select(-c( "mean.pct_low", "mean.pct_upp", "n_unweighted", "subset_1_name", 
                                                       "count_by_subset", "response_count")) %>% rename(
                                                         District = subset_1_val,
                                                        `All group` = mean.pct
                                                       ) #%>% filter(main_variable != "governorate")

district_pop_group_summary_re <- district_pop_group_summary %>% 
  select(-c("mean.pct_low", "mean.pct_upp", "n_unweighted", 
            "subset_1_name", "subset_2_name", "count_by_subset", "response_count"
  )) %>% rename(
    District= subset_2_val
  ) #%>% filter(main_variable != "governorate")


district_pop_group_summary_pivot <- district_pop_group_summary_re %>% pivot_wider(names_from = "subset_1_val",
                                                                                  values_from = "mean.pct")

nrow(district_pop_group_summary_pivot)
nrow(district_summary_re)


district_wide_summary <- district_summary_re %>% left_join(district_pop_group_summary_pivot)
nrow(district_wide_summary)








district_wide_summary2 <- district_wide_summary %>% select(-choice) %>% left_join(kobo_type) %>% rename(
  `Question ID` = main_variable, 
  `Question::English`=`label::English`,
  `Question::Arabic` = `label::Arabic`,
  `Choice::English` = `choice_label::English`,
  `Choice::Arabic`= `choice_label::Arabic`,
  `All groups` = `All group`
  
) %>% select(District,`Question ID`, `Question::English`,`Question::Arabic`, `Choice::English`,`Choice::Arabic`,`Question type`,everything()) %>% 
  filter(!is.na(`Question type`))



district_wide_summary3 <- district_wide_summary2 %>% dplyr::mutate(
  `All groups` = case_when(`Question type` %in% c("select_one","select_multiple")~ paste0(round(`All groups`*100),"%"), T~as.character(round(`All groups`,2))),
  `Host Community HH` = case_when(`Question type` %in% c("select_one","select_multiple")~ paste0(round(`Host Community HH`*100),"%"),T~as.character(round(`Host Community HH`))),
  `In-camp IDP HH` = case_when(`Question type` %in% c("select_one","select_multiple")~ paste0(round(`In-camp IDP HH`*100),"%"),T~as.character(round(`In-camp IDP HH`))),
  `Returnee HH` = case_when(`Question type` %in% c("select_one","select_multiple")~ paste0(round(`Returnee HH`*100),"%"),T~as.character(round(`Returnee HH`))),
  `Out-of-camp IDP HH` = case_when(`Question type` %in% c("select_one","select_multiple")~ paste0(round(`Out-of-camp IDP HH`*100),"%"),T~as.character(round(`Out-of-camp IDP HH`)))
)


district_wide_summary <- district_wide_summary3 %>% naniar::replace_with_na_all(condition = ~.x == "NA%")

district_wide_summary$District <- district_wide_summary$District %>% snakecase::to_title_case() %>%
  stringr::str_replace_all("Al ","Al-")




to_write[["district_wide_summary"]] <- district_wide_summary



write_excel_as_reach_format(to_write,"03_outputs/10_analysis/formatted_summary.xlsx")



