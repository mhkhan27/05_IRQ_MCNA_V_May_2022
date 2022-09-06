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


kobo_path <- "02_ToR_tools_Dap/01_tool/IRQ2206_Tool_MCNA X_transl_inc_CampProfiling_June 3rd_DEPLOYED - Copy with corrected rCSI labels.xlsx"
read_all_sheet_as_csv_format(kobo_path)
read_all_sheet_as_csv_format("03_outputs/09_clean_dataset/raw_data.xlsx")


## raw dataset

## cleaning


