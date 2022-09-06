rm (list=ls())

library(dplyr)
library(srvyr)
library(illuminate)
library(stringr)
library(openxlsx)
library(readxl)
library(purrr)
library(expss)

read_all_sheet_as_csv_format("03_outputs/10_analysis/2022_08_02_mcna_analysis.xlsx")
read_all_sheet_as_csv_format("LIVE_MCNA_LIVE_-_all_versions_-_False_-_2022-08-03-07-26-46.xlsx")




# indv_with_weight <- indv_raw %>% rename (X_uuid = X_submission__uuid)

member <- member %>% rename(X_uuid = X_submission__uuid)
member <- member %>% filter(!relationship %in%  c("error","guest","friend" ))

member$relationship %>% unique()
member_s <- member %>% group_by(X_uuid) %>% summarise(
  summary =n()
)

hh_raw_com  <- hh_raw %>% select(X_uuid, num_family_member) %>% left_join(member_s) %>% mutate(
  diff = summary - num_family_member
) %>% filter( diff != 0)



write.csv(hh_raw_com,"number_mismatched.csv")
check <- hh_raw_com %>% left_join( hh_raw %>% select(X_uuid,cal_idp)
  ) 
