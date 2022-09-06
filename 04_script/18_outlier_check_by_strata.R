rm (list=ls())

library(dplyr)
library(srvyr)
library(illuminate)
library(stringr)
library(openxlsx)
library(readxl)
library(purrr)
library(expss)
library(tidyr)
options(scipen = 999)

read_all_sheet_as_csv_format("03_outputs/09_clean_dataset/raw_data.xlsx")
kobo_tool <- "02_ToR_tools_Dap/01_tool/IRQ2206_Tool_MCNA X_transl_inc_CampProfiling_June 3rd_DEPLOYED.xlsx"
read_all_sheet_as_csv_format(kobo_tool)

hh_raw <- hh_raw %>% filter(consent == "yes" & calc_eligible == 1)

hh_raw<- hh_raw %>% mutate(
  strata = case_when(is.na(camp_name) ~ district_mcna, T~ camp_name)
) %>% mutate(
  strata = paste0(pop_group, "_", strata)
)



hh_raw <- fix_data_type_frm_kobo(df = hh_raw,kobo_survey_sheet = survey)

outliers <- list()


for( i in unique(hh_raw$strata)){
  
  df <- hh_raw %>% filter(strata == i)
  outliers[[i]] <- outlier_check(df = df,include_multiple_choices = F,
                                 kobo_tool_location = kobo_tool,cols_to_report = c("X_uuid","strata"))
}

all_outliers <- do.call("bind_rows",outliers) %>% rename(
  question = questions
)





##################### cleaning_logs #############

# you have change the path. Please ask TED for the folder
mcna_one_drive_folder <- "C:\\Users\\rakib\\OneDrive - ACTED\\MCNA 2022\\4. Sampling and Data/2. Data Cleaning/Logs/"
partner_log_folder_path <- "C:\\Users\\rakib\\OneDrive - ACTED\\MCNA 2022\\4. Sampling and Data/2. Data Cleaning/Partner logs/"

# 
# mcna_one_drive_folder <- "/Users/mehedi/Library/CloudStorage/OneDrive-ACTED/MCNA 2022/4. Sampling and Data/2. Data Cleaning/Logs"
# partner_log_folder_path <- "/Users/mehedi/Library/CloudStorage/OneDrive-ACTED/MCNA 2022/4. Sampling and Data/2. Data Cleaning/Partner logs/"

cl_list <- list.files(mcna_one_drive_folder,".xlsx",full.names = T)


# combine reach cleaning log ----------------------------------------------

cleaning_log_reach <- list()

for (i in cl_list ){
  cleaning_log_reach[[i]] <- read_excel(i,sheet = "CONC") %>%
    mutate_all(as.character)
}

cleaning_log_total <- do.call("bind_rows",cleaning_log_reach)

cleaning_log_total <- cleaning_log_total %>% select (X_uuid, repeat_uuid, ngo_label, loop, question, issue, old_value, change_type, new_value) 



# combine partners cleaning logs ------------------------------------------


partner_cl_list <- list.files(partner_log_folder_path,".xlsx",full.names = T)

partner_cleaning_log <- list()
for (i in partner_cl_list ){
  partner_cleaning_log[[i]] <- read_excel(i,sheet = "CONC") %>%
    mutate_all(as.character) 
}


partner_cleaning_log_total <- do.call("bind_rows",partner_cleaning_log)

partner_cleaning_log_total <- partner_cleaning_log_total %>% select (X_uuid, repeat_uuid, ngo_label, loop, question, issue, old_value, change_type, new_value) 



# compile cleaning logs ---------------------------------------------------

final_cleaning_log <- cleaning_log_total %>% bind_rows(partner_cleaning_log_total)

################################################


cleaning_log <-final_cleaning_log %>% select(question,X_uuid) %>% mutate(exist = "exists")



outliers_joined <- all_outliers %>% left_join(cleaning_log) %>% filter(is.na(exist))



can_based_on_locality <- c("cereals", "vegetables", "fruits", "oil_fats", 
                           "sweets", "spices_condiments","inc_employment_pension", "rent_exp", "medical_exp", 
                           "food_exp", "medical_exp_camp", "food_exp_camp", 
                            "how_much_debt")



final_missing  <- outliers_joined %>% filter(question %in% can_based_on_locality)


write.csv(outliers_joined %>% select(-exist),"Additinal_outliers.csv")
