rm(list = ls(all = T))
library(dplyr)
library(lubridate)
library(readxl)
library(kableExtra)
library(knitr)
library(readr)
library(openxlsx)
library(sf)
library(raster)
library(cluster)
library(stringr)
library(stringi)

source("functions/audit_function_full.R")
source("functions/function_handler.R")

WGS84 <- crs("+init=epsg:4326")

assessment_start_date <- as.Date("2021-05-30")

# set min time and max # interviews per day
time_limit <- 12
flag_time_limit <- 25
max_interv <- 10

list_of_excludes <- c(
  "note_returnee",
  "note_host",
  "note_idp",
  "note_inelegible",
  "ind_level",
  "male_2_note",
  "female_2_note",
  "male_3_5_note",
  "female_3_5_note",
  "male_6_17_note",
  "female_6_17_note",
  "male_18_59_note",
  "female_18_59_note",
  "male_60_note",
  "female_60_note",
  "tot_male_note",
  "tot_female_note",
  "ref_child",
  "not_residing_reason_note",
  "food_security_note",
  "livelihood_strategies",
  "sufficient_water",
  "exp_compare",
  "ref_evic",
  "note_missing_documents_above_18",
  "note_missing_documents_under_18",
  "movement_restrictions",
  "thanks"
)

###########################################################################################################

name <-
  "IRQ2108_MCNA_IX_Face-to-Face_May2021.xlsx"

# read data from excel file
df <-
  read_excel(sprintf("input/raw_data/%s", name), sheet = "IRQ2108_MCNA IX_Face-to-Face...") %>%
  mutate(calc_eligible = as.numeric(calc_eligible))

df$X_uuid <- df$`_uuid`


# import loop data
indiv_df <-
  read_excel(sprintf("input/raw_data/%s", name), sheet = "member") 

indiv_df$X_submission__uuid <- indiv_df$`_submission__uuid`

df <- distinct(df, X_uuid, .keep_all = T)
indiv_df <- unique(indiv_df)

kobo_tool <- read_excel("input/questionnaire/REACH_IRQ2108_Kobo_MCNA IX_DEPLOYED.xlsx", sheet = 1)
kobo_choices <- read_excel("input/questionnaire/REACH_IRQ2108_Kobo_MCNA IX_DEPLOYED.xlsx", sheet = 2)

###########################################################################################################

df <-
  dplyr::select(df,!c(list_of_excludes))

# add population group
df <- df %>% 
  mutate(population_group = case_when(calc_eligible == 0 ~ "not_eligible",
                                      calc_idp == 1 ~ "idp_out_camp",
                                      calc_returnee == 1 ~ "returnee",
                                      displace_status == "yes" &
                                        displace_status_returnee == "no" ~ "host",
                                      TRUE ~ "not_eligible"))

deletion_log <- df %>% filter(population_group == "not_eligible")

# remove ineligible surveys
df <- df %>% filter(population_group != "not_eligible")

###########################################################################################################
# SPATIAL district check  for in person data collection #
############# Data frames for tracking ##############


in_person_points <- df %>%
  filter(!is.na(`_gpslocation_latitude`),!is.na(`_gpslocation_longitude`)) %>%
  dplyr::select(
    `_uuid`,
    governorate_mcna,
    cluster_location_id,
    enumerator_num,
    population_group,
    lat = `_gpslocation_latitude`,
    long = `_gpslocation_longitude`
  ) %>%
  as_tibble()

points_sf <- in_person_points %>%
  dplyr::select(
    `_uuid`,
    governorate_mcna,
    cluster_location_id,
    enumerator_num,
    population_group,
    lat,
    long
  ) %>%
  st_as_sf(
    coords = c("long", "lat"),
    agr = "constant",
    crs = WGS84,
    stringsAsFactors = FALSE,
    remove = TRUE
  )


######### IRQ Dist boundary ###################

irq_dist <- st_read("input/spatial_data/irq_dist.geojson") %>%
  mutate(district_gps = irq_dist_lookup_dist_kobo,
         District = irq_dist_lookup_dist_kobo)

irq_dist <-
  sf::st_as_sf(irq_dist, coords = c("long", "lat"), crs = 4326)
irq_dist <- sf::st_cast(irq_dist, "POLYGON")

# spatial join points to get true districts of GPS locations
district_join <- st_join(points_sf, irq_dist) %>%
  dplyr::select(`_uuid`:geometry, population_group, district_gps)

##
# convert back to df
district_join_df <- as.data.frame(district_join) %>%
  dplyr::select(`_uuid`, district_gps) %>%
  mutate(uuid = `_uuid`,
         district_gps = as.character(district_gps))

# merge back with main df to add actual district
df <- merge(df, district_join_df, all = T)


# set district_mcna column to true district for in person surveys
df <- df %>%
  mutate(district_mcna = case_when(!is.na(district_gps) ~ district_gps,
                                   TRUE ~ district_mcna))

# add full strata
df <- df %>%
  mutate(strata = paste0(district_mcna, "_", population_group))

###########################################################################################################
#Similarity Checks

df$ngo_name <-
  case_when(is.na(df$ngo_name) ~ as.character(df$ngo_name_001),
            TRUE ~ df$ngo_name)
df$enumerator_num <-
  case_when(
    is.na(df$enumerator_num) ~ as.character(df$enumerator_name),
    TRUE ~ as.character(df$enumerator_num)
  )
names(df) <- gsub("/", ".", names(df))
names(indiv_df) <- gsub("/", ".", names(indiv_df))

all_similarity <- calculateDifferences(df, kobo_tool)
# enum_similarity <- calculateEnumeratorSimilarity(df, kobo_tool, col_enum = "enumerator_num", col_admin = "district_mcna")
# 
write_excel_csv(all_similarity, sprintf("output/enumerator_behavior_checks/overall_similarity_%s.csv",today()))
# write_excel_csv(enum_similarity,sprintf("output/enumerator_behavior_checks/enumerator_similarity_percentage_%s.csv",today()))


# time check from audit files

df$start_time <- format(as.POSIXct(df$start, format="%Y-%m-%dT%H:%M:%S"), format="%H:%M")
# today
df$today <- as.Date(df$start, "%Y-%m-%d")

df <-
  time_check_audit(audit_dir_path = "audit/", df,  "X_uuid", time_limit = time_limit, list_of_excludes=list_of_excludes)


########################################################################################
###########################################################################################################

# number of NAs check
df$NAs <- apply(
  df,
  1,
  FUN = function(x) {
    length(which(is.na(x)))
  }
)

#########################################################################################################

##### Write to csv for data checking ###################
write_excel_csv(df,
                sprintf("output/data_checking/mcna_all_data_%s.csv", today()))

###########################################################################################################
###########################################################################################################
###########################################################################################################
# DO CLEANING
# read cleanimg conditions csv list

conditionDf <-
  read.csv("input/conditions/conditions_log.csv", as.is = TRUE)

conditionLoop <-   read.csv("input/conditions/conditions_loop_log.csv", as.is = TRUE)
indiv_df <- indiv_df %>%
  mutate(ngo_name = df$ngo_name[match(indiv_df$X_submission__uuid, df$X_uuid)],
         `_uuid` = paste0(indiv_df$index, "_", `_submission__uuid`),
         date_assessment = df$date_assessment[match(indiv_df$X_submission__uuid, df$`_uuid`)],
         enumerator_num = df$enumerator_num[match(indiv_df$X_submission__uuid, df$`_uuid`)],
         cluster_location_id = df$cluster_location_id[match(indiv_df$X_submission__uuid, df$`_uuid`)],
         governorate_mcna = df$governorate_mcna[match(indiv_df$X_submission__uuid, df$`_uuid`)],
         district_mcna = df$district_mcna[match(indiv_df$X_submission__uuid, df$`_uuid`)],
         age_respondent = df$age_respondent[match(indiv_df$X_submission__uuid, df$`_uuid`)],
         gender_respondent = df$gender_respondent[match(indiv_df$X_submission__uuid, df$`_uuid`)])

# return logs
logsDF <- read_conditions_from_excel_limited_row(df, conditionDf)
logsLoop <- read_conditions_from_excel_limited_row(indiv_df, conditionLoop)

# create new columns "log_number"
logsDF$log_number = seq.int(nrow(logsDF))
logsLoop$log_number = seq.int(nrow(logsLoop))
# order data fram by log_number
ordered_df <- logsDF[order(logsDF$log_number),]
ordered_loop <- logsLoop[order(logsLoop$log_number),]
readr::write_excel_csv(ordered_df,
                       sprintf("output/cleaning_log/cleaning_log_household_%s.csv", today()))
readr::write_excel_csv(ordered_loop,
                       sprintf("output/cleaning_log/cleaning_log_individual_%s.csv", today()))

# 
# # export data with check columns
# logs_as_columns <-
#   read_conditions_from_excel_column(df, conditionDf)
# 
# write.csv(
#   logs_as_columns,
#   sprintf(
#     "output/cleaning_log/data_w_checks/data_checks_%s.csv",
#     today()
#   ),
#   row.names = FALSE
# )
#################################################################################################################
#checking head and spouse gender
indiv <- indiv_df
indiv$spouse_sex <- case_when(indiv$relationship == "spouse" ~ indiv$sex, TRUE ~ NA_character_)
indiv$head_sex <- case_when(indiv$relationship == "head" ~ indiv$sex, TRUE ~ NA_character_)

indiv$spouse_age <- case_when(indiv$relationship == "spouse" ~ as.numeric(indiv$age), TRUE ~ NA_real_)
indiv$head_age <- case_when(indiv$relationship == "head" ~ as.numeric(indiv$age), TRUE ~ NA_real_)

df$head_sex <- indiv$head_sex[match(df$X_uuid, indiv$X_submission__uuid)]
df$head_age <- indiv$head_age[match(df$X_uuid, indiv$X_submission__uuid)]
indiv <- indiv %>% filter(!is.na(spouse_sex))
df$spouse_sex <- indiv$spouse_sex[match(df$X_uuid, indiv$X_submission__uuid)]
df$spouse_age <- indiv$spouse_age[match(df$X_uuid, indiv$X_submission__uuid)]

indiv_df$spouse_sex <- df$spouse_sex[match(indiv_df$X_submission__uuid, df$X_uuid)]
indiv_df$head_sex <- df$head_sex[match(indiv_df$X_submission__uuid, df$X_uuid)]
indiv_df$spouse_age <- df$spouse_age[match(indiv_df$X_submission__uuid, df$X_uuid)]
indiv_df$head_age <- df$head_age[match(indiv_df$X_submission__uuid, df$X_uuid)]

#filtering illogical age differences between members
list_uuids <-
  unique(indiv_df[(indiv_df$relationship %in% c("child", "childinlaw") &
                     indiv_df$head_age - as.numeric(indiv_df$age) < 15
  ) | 
    (indiv_df$relationship %in% c("grandchild") &
       indiv_df$head_age - as.numeric(indiv_df$age) < 30
    ) |
    (indiv_df$relationship %in% c("parent", "parentinlaw") &
       as.numeric(indiv_df$age) - indiv_df$head_age < 15 ) |
    (indiv_df$relationship %in% c("parent", "parentinlaw") &
       as.numeric(indiv_df$age) - indiv_df$head_age < 30)
  , "X_submission__uuid"])

indiv <- indiv_df %>% filter(X_submission__uuid %in% list_uuids$X_submission__uuid)
indiv$district <- df$district_mcna[match(indiv$X_submission__uuid, df$X_uuid)]
indiv$cluster_location_id <- df$cluster_location_id[match(indiv$X_submission__uuid, df$X_uuid)]
indiv$date_assessment <- df$date_assessment[match(indiv$X_submission__uuid, df$X_uuid)]
indiv$enumerator_name <- df$enumerator_num[match(indiv$X_submission__uuid, df$X_uuid)]
indiv$age_respondent <- df$age_respondent[match(indiv$X_submission__uuid, df$X_uuid)]
indiv$gender_respondent <- df$gender_respondent[match(indiv$X_submission__uuid, df$X_uuid)]
indiv$governorate <- df$governorate_mcna[match(indiv$X_submission__uuid, df$X_uuid)]
indiv$population_group <- df$population_group[match(indiv$X_submission__uuid, df$X_uuid)]

write_excel_csv(indiv, "loop_age.csv")


#################################################################################################################
# read log csv file after AO has indicated decision on flagged data
log_df <-
  read.csv("input/cleaned_log/cleaned_df.csv", na.strings = c("", NA, "NA")) %>% rename(X_uuid = ?..X_uuid)

log_indiv <- readr::read_csv("input/cleaned_log/cleaned_loop.csv", na = c("", NA, "NA")) %>% filter(district %in%  c("al.falluja", "duhok", "al.nasiriya", "kirkuk"))
log_indiv <- data.frame(log_indiv)

replaced_df <- df %>% rename(difficulty_accessing_services_001_other = difficulty_accessing_services_other)
replaced_indiv <- indiv_df

log_df <- log_df %>% filter(X_uuid %in% replaced_df$X_uuid)

#cleaning the dataset and integrating the multiple answe choices
for (i in 1:length(log_df$X_uuid)) {
  log_df[i, "old.value"] <- case_when(is.na(log_df[i, "old.value"]) ~ replaced_df[replaced_df$X_uuid == log_df[i, "X_uuid"], log_df[i, "question.name"]], TRUE ~ log_df[i, "old.value"])
  replaced_df[replaced_df$X_uuid == log_df[i, "X_uuid"], log_df[i, "question.name"]] <- log_df[i, "new.value"]
  if(log_df[i, "select_type"] == "select_one" & log_df[i, "new.value"] %in% kobo_choices[kobo_choices$list_name == log_df[i, "choice_list"], "name"]){
    replaced_df[replaced_df$X_uuid == log_df[i, "X_uuid"], gsub("_other", "", log_df[i, "question.name"])] <- log_df[i, "new.value"]
  }else if (log_df[i, "select_type"] == "select_multiple"){
    choices <- strsplit(log_df[i, "new.value"], " ")
    j <- 1
    while (j <= length(choices)) {
      if(choices[j] %in% as.list(kobo_choices[kobo_choices$list_name == log_df[i, "choice_list"], "name"])$name)
        replaced_df[replaced_df$X_uuid == log_df[i, "X_uuid"], paste0(gsub("_other", "", log_df[i, "question.name"]), ".", choices[j])] <- 1
      if(log_df[i, "old.value"] %in% as.list(kobo_choices[kobo_choices$list_name == log_df[i, "choice_list"], "name"])$name)
        replaced_df[replaced_df$X_uuid == log_df[i, "X_uuid"], paste0(gsub("_other", "", log_df[i, "question.name"]), ".", log_df[i, "old.value"])] <- 0
      
      j <- j + 1
    }
  }
}



replaced_indiv$cluster_location_id <- df$cluster_location_id[match(replaced_indiv$X_submission__uuid, df$X_uuid)]
replaced_indiv$enumerator_name     <- df$enumerator_num[match(replaced_indiv$X_submission__uuid, df$X_uuid)]
replaced_indiv$age_respondent      <- df$age_respondent[match(replaced_indiv$X_submission__uuid, df$X_uuid)]
replaced_indiv$gender_respondent   <- df$gender_respondent[match(replaced_indiv$X_submission__uuid, df$X_uuid)]

for (i in 1:nrow(log_indiv)) {
  log_indiv[i, "index"] <-
    ifelse(!is.na(log_indiv[i, "X_uuid"]), log_indiv[i, "index"],
           na.omit(unique(replaced_indiv$index[replaced_indiv[, log_indiv[i, "question"]] == log_indiv[i, "old.value"] &
                                                 as.numeric(replaced_indiv$age_respondent) == log_indiv[i, "age_respondent"] &
                                                 replaced_indiv$gender_respondent == log_indiv[i, "gender_respondent"] &
                                                 replaced_indiv$cluster_location_id == log_indiv[i, "cluster_location_id"] &
                                                 replaced_indiv$enumerator_name == as.character(log_indiv[i, "enumerator_name"])])))
  log_indiv[i, "X_uuid"] <-
    ifelse(!is.na(log_indiv[i, "X_uuid"]), log_indiv[i, "X_uuid"],
           na.omit(unique(replaced_indiv$X_submission__uuid[replaced_indiv[, log_indiv[i, "question"]] == log_indiv[i, "old.value"] &
                                                              as.numeric(replaced_indiv$age_respondent) == log_indiv[i, "age_respondent"] &
                                                              replaced_indiv$gender_respondent == log_indiv[i, "gender_respondent"] &
                                                              replaced_indiv$cluster_location_id == log_indiv[i, "cluster_location_id"] &
                                                              replaced_indiv$enumerator_name == as.character(log_indiv[i, "enumerator_name"])])))
  
  replaced_indiv[replaced_indiv$X_submission__uuid == log_indiv[i, "X_uuid"] &
                   replaced_indiv$index == log_indiv[i, "index"] &
                   replaced_indiv[, log_indiv[i, "question"]] == log_indiv[i, "old.value"], log_indiv[i, "question"]] <- log_indiv[i,"new.value"]
}

sub_indiv <- replaced_indiv %>% filter(head_sex == spouse_sex)

for (i in 1:nrow(sub_indiv)) {
  if(sub_indiv$relationship[i] == "spouse" & sub_indiv$spouse_sex[i] == "male"){
    replaced_indiv$sex[replaced_indiv$X_submission__uuid == sub_indiv$X_submission__uuid[i] & replaced_indiv$index == sub_indiv$index[i]] <- "female"
  } else if (sub_indiv$relationship[i] == "spouse" &
             sub_indiv$spouse_sex[i] == "female" &
             (sub_indiv$difficulty_seeing[i] %in% c("some_difficulty", "a_lot_of_difficulty", "cannot_do_at_all") |
              sub_indiv$difficulty_communicating[i] %in% c("some_difficulty", "a_lot_of_difficulty", "cannot_do_at_all") |
              sub_indiv$difficulty_hearing[i] %in% c("some_difficulty", "a_lot_of_difficulty", "cannot_do_at_all") |
              sub_indiv$difficulty_remembering[i] %in% c("some_difficulty", "a_lot_of_difficulty", "cannot_do_at_all") |
              sub_indiv$difficulty_selfcare[i] %in% c("some_difficulty", "a_lot_of_difficulty", "cannot_do_at_all") |
              sub_indiv$difficulty_walking[i] %in% c("some_difficulty", "a_lot_of_difficulty", "cannot_do_at_all"))
             & sub_indiv$work[i] == "no"){ 
    replaced_indiv$sex[replaced_indiv$X_submission__uuid == sub_indiv$X_submission__uuid[i] & replaced_indiv$index == sub_indiv$index[i]] <- "male"
  } else if (sub_indiv$relationship[i] == "head" &
             sub_indiv$head_sex[i] == "female" &
             sub_indiv$head_age[i] - sub_indiv$spouse_age[i] >= 6){
    replaced_indiv$sex[replaced_indiv$X_submission__uuid == sub_indiv$X_submission__uuid[i] & replaced_indiv$index == sub_indiv$index[i]] <- "male"
  } else if(sub_indiv$relationship[i] == "head" &
            sub_indiv$head_sex[i] == "female" &
            sub_indiv$work[i] == "yes"){
    replaced_indiv$sex[replaced_indiv$X_submission__uuid == sub_indiv$X_submission__uuid[i] & replaced_indiv$index == sub_indiv$index[i]] <- "male"
  } else if (sub_indiv$relationship[i] == "head" &
             sub_indiv$head_sex[i] == "female"&
             sub_indiv$marital_status[i] == "married"){
    replaced_indiv$sex[replaced_indiv$X_submission__uuid == sub_indiv$X_submission__uuid[i] & replaced_indiv$index == sub_indiv$index[i]] <- "male"
  }
}

# take uuids of deleted loop surveys and remove from cleaned dataset
deleted_loop <- replaced_indiv %>%
  filter(X_submission__uuid %in% deletion_log$X_uuid, relationship == "error")

indiv_cleaned <- replaced_indiv %>%  filter(!X_submission__uuid %in% deletion_log$X_uuid, relationship != "error")




# export clean data
write.csv(
  replaced_df,
  sprintf(
    "output/cleaned_data/mcna_data_clean_parent_%s.csv",
    today()
  ),
  row.names = FALSE
)
write.csv(
  indiv_cleaned,
  sprintf("output/cleaned_data/mcna_data_clean_loop_%s.csv", today()),
  row.names = FALSE
)

# export to one spreadsheet
mcna_datasets <-
  list("MCNA_VIII_2020" = replaced_df, "member" = indiv_cleaned)
write.xlsx(mcna_datasets, (
  file = sprintf("output/cleaned_data/mcna_data_clean_%s.xlsx", today())
))

# export deletion log
mcna_deleted <-
  list("MCNA_VIII_2020" = deletion_log, "member" = deleted_loop)
write.xlsx(mcna_deleted, (
  file = sprintf("output/deletion_log/mcna_data_deleted_%s.xlsx", today())
))



###########################################################################################################



###########################################################################################################