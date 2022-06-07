# download the data 
# change the start date 



rm(list = ls())

library(dplyr)
library(illuminate)
library(readxl)
library(stringr)
library(tidyr)
library(st)
library(sf)
library(kableExtra)
library(leaflet)
library(cluster)
library(openxlsx)
library(readxl)
library(randomcoloR)

### variable_to_change

write_file <- c(T,F)[1]
old_date_log <- read.csv("03_outputs/02_date_log/2022-06_06_date_log.csv")


rmarkdown::render("daily_monitoring_report.Rmd")

if(write_file ==T){
file.copy(from = "daily_monitoring_report.html",
          to = paste0("03_outputs/07_daily_progress_tracker/",str_replace_all(Sys.Date(),"-","_"),"_daily_monitoring_report.html"),overwrite = T)
}
