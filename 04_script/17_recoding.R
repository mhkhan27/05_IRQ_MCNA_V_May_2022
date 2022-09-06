





loop$age <- as.numeric(loop$age)
names(loop) <- gsub("/", ".",names(loop))
names(df) <- gsub("/", ".",names(df))



loop_school_age <- loop %>% group_by(X_uuid) %>% summarise(
  num_school_going_children  = sum(age %in% 6:17 &  (school_formal_enrollment == "yes"|school_regular_attendance_formal == "yes"),na.rm = T)
)

df <- df %>% left_join(loop_school_age)

# a <- df %>% select(num_school_going_children,drop_out,child_dropout_school)

df <- df %>% mutate(
  child_dropout_school = case_when(
    num_school_going_children == 0 ~ "not_applicable",
    num_school_going_children > 1 & (drop_out == 0| is.na(drop_out)) ~ "no_need" ,
    T~child_dropout_school
  )
)

coping_column <- c("selling_assets",
                  "borrow_debt",
                   "selling_transportation_means",
                   "child_dropout_school",
                   "reduce_spending",
                   "change_place",
                   "adult_risky",
                   "child_work_cs",
                   "family_migrating",
                   "child_forced_marriage"
)


for (i in coping_column) {
  df[[i]] <- case_when(df[[i]] == "not_applicable" ~ NA_character_,T~df[[i]])
}







reasons <- c("not_residing_married",
             "not_residing_seek_employment",
             "not_residing_study",
             "not_residing_armed_actors",
             "not_residing_kidnapped",
             "not_residing_missing",
             "not_residing_detained"
)

df <- df %>% mutate(
  reasons_rs = rowSums(df[reasons],na.rm = T)
) %>% mutate(
  dif_reason = not_residing_num - reasons_rs
)

df$dif_reason <- df$dif_reason %>% as.integer()
df$dif_reason  %>% unique()

df <- df %>% mutate(
  not_residing_married = case_when((dif_reason != 0 & !is.na(dif_reason))~ NA_integer_, T~not_residing_married),
  not_residing_seek_employment = case_when((dif_reason != 0 & !is.na(dif_reason)) ~ NA_integer_,T~not_residing_seek_employment),
  not_residing_study = case_when((dif_reason != 0 & !is.na(dif_reason)) ~ NA_integer_,T~not_residing_study),
  not_residing_armed_actors = case_when((dif_reason != 0 & !is.na(dif_reason)) ~ NA_integer_,T~not_residing_armed_actors),
  not_residing_kidnapped = case_when((dif_reason != 0 & !is.na(dif_reason)) ~ NA_integer_,T~not_residing_kidnapped),
  not_residing_missing = case_when((dif_reason != 0 & !is.na(dif_reason)) ~ NA_integer_,T~not_residing_missing),
  not_residing_detained = case_when((dif_reason != 0 & !is.na(dif_reason)) ~ NA_integer_,T~not_residing_detained)
) 




loop <- loop %>% mutate(age_class = case_when(age < 18 ~ "Children", 
                                              age < 60 ~ "Adult",
                                              age >= 60 ~ "Elderly"),
                        female = case_when(sex == "female" ~ 1, TRUE ~ 0),
                        single_headed = case_when(relationship == "head" & marital_status != "married" ~ 1,
                                                  TRUE ~ 0),
                        female_headed = case_when(relationship == "head" & sex == "female" ~ 1,
                                                  TRUE ~ 0),
                        male_headed = case_when(relationship == "head" & sex == "male" ~ 1,
                                                TRUE ~ 0),
                        hhh_gender = case_when(female_headed == 1 ~ "female",T~"male"),
                        child_married = case_when(marital_status %in% c("divorced", "married", "separated", "widowed") & age < 18 ~ 1, TRUE ~ 0),
                        
                        child_working = case_when(age < 18 & work == "yes" ~ 1, TRUE ~ 0),
                        child_work_non_structured = case_when(child_working == 1 & child_work_type.non_structured == 1 ~ 1, TRUE ~ 0),
                        child_work_structured = case_when(child_working == 1 & child_work_type.structured == 1 ~ 1, TRUE ~ 0),
                        child_work_family_work = case_when(child_working == 1 & child_work_type.family_work == 1 ~ 1, TRUE ~ 0),
                        child_work_other = case_when(child_working == 1 & child_work_type.other == 1 ~ 1, TRUE ~ 0),
                        
                        disabled_original = case_when(age < 5 ~ NA_real_,
                                                      difficulty_communicating %in% c("a_lot_of_difficulty", "cannot_do_at_all") | 
                                                        difficulty_seeing %in% c("a_lot_of_difficulty", "cannot_do_at_all") | 
                                                        difficulty_hearing %in% c("a_lot_of_difficulty", "cannot_do_at_all") | 
                                                        difficulty_walking %in% c("a_lot_of_difficulty", "cannot_do_at_all") | 
                                                        difficulty_remembering %in% c("a_lot_of_difficulty", "cannot_do_at_all") | 
                                                        difficulty_selfcare %in% c("a_lot_of_difficulty", "cannot_do_at_all") ~ 1, TRUE ~ 0),
                        disabled_new = case_when(age < 5 ~ NA_real_,
                                                 count_row_if("some_difficulty", loop[, c("difficulty_communicating",
                                                                                          "difficulty_seeing",
                                                                                          "difficulty_hearing",
                                                                                          "difficulty_walking",
                                                                                          "difficulty_remembering",
                                                                                          "difficulty_selfcare")]) >= 2 |
                                                   difficulty_communicating %in% c("a_lot_of_difficulty", "cannot_do_at_all") | 
                                                   difficulty_seeing %in% c("a_lot_of_difficulty", "cannot_do_at_all") | 
                                                   difficulty_hearing %in% c("a_lot_of_difficulty", "cannot_do_at_all") | 
                                                   difficulty_walking %in% c("a_lot_of_difficulty", "cannot_do_at_all") | 
                                                   difficulty_remembering %in% c("a_lot_of_difficulty", "cannot_do_at_all") | 
                                                   difficulty_selfcare %in% c("a_lot_of_difficulty", "cannot_do_at_all") ~ 1, TRUE ~ 0),
                        disabled_seeing = case_when(age < 5 ~ NA_real_,difficulty_seeing %in% c("a_lot_of_difficulty", "cannot_do_at_all") ~ 1, TRUE ~ 0),
                        disabled_hearing = case_when(age < 5 ~ NA_real_,difficulty_hearing %in% c("a_lot_of_difficulty", "cannot_do_at_all") ~ 1, TRUE ~ 0),
                        disabled_walking = case_when(age < 5 ~ NA_real_,difficulty_walking %in% c("a_lot_of_difficulty", "cannot_do_at_all") ~ 1, TRUE ~ 0),
                        disabled_remembering = case_when(age < 5 ~ NA_real_,difficulty_remembering %in% c("a_lot_of_difficulty", "cannot_do_at_all") ~ 1, TRUE ~ 0),
                        disabled_selfcare = case_when(age < 5 ~ NA_real_,difficulty_selfcare %in% c("a_lot_of_difficulty", "cannot_do_at_all") ~ 1, TRUE ~ 0),
                        disabled_communicating = case_when(age < 5 ~ NA_real_,difficulty_communicating %in% c("a_lot_of_difficulty", "cannot_do_at_all") ~ 1, TRUE ~ 0),
                       
                        attending_formal = case_when(school_regular_attendance_formal == "yes" ~ 1, age %in% c(6:17) ~ 0, TRUE ~ NA_real_),

                        
                        attending_informal = case_when(school_informal_attendance == "yes" ~ 1, age %in% c(6:17) ~ 0, TRUE ~ NA_real_),
                        not_attending_school = case_when(school_regular_attendance_formal == "no" & school_informal_attendance == "no" ~ 1, TRUE ~ 0),
                        school_enrollment = case_when(school_formal_enrollment == "yes"  ~ 1, age %in% c(6:17) ~ 0, TRUE ~ NA_real_),
                        
                        #no_distance_learning = case_when(school_distance_learning.no == 1 ~ 1, TRUE ~ 0),
                        child_work_study = case_when((school_regular_attendance_formal == "yes" | school_informal_attendance == "yes") & work == "yes" ~ 1, TRUE ~ 0),
                        unemployed_seek_work = case_when(
                          loop$age < 18 ~ NA_real_,
                          loop$work == "no" &
                            loop$actively_seek_work %in% c("yes", "no_opportunity") ~ 1,
                          loop$work == "yes" |
                            (loop$work == "no" &
                               !loop$actively_seek_work %in% c("yes", "no_opportunity")) ~ 0,
                          TRUE ~ NA_real_
                        ),
                        
                        adults = case_when(loop$age >= 18 ~ 1, TRUE ~ 0),
                        head_seek_work = case_when(work == "no" & actively_seek_work %in% c("yes", "no_opportunity") & relationship == "head" ~ 1, TRUE ~ 0),
                        school_aged_child = case_when(age >= 6 & age < 18 ~ 1, TRUE ~ 0),
                        women_working = case_when(age >= 18 & age < 60 & sex == "female" & work == "yes" ~ 1, 
                                                  age >= 18 & age < 60 & sex == "female" ~ 0,
                                                  TRUE ~ NA_real_))


loop <- loop%>% mutate(
  total_disable_score = rowSums(loop[c("disabled_seeing","disabled_hearing","disabled_walking","disabled_remembering","disabled_selfcare","disabled_communicating")],na.rm = T)
)

sum_loop <- loop %>% group_by(X_uuid) %>%
  summarize(children = sum(age_class == "Children", na.rm = T),
            adults = sum(age_class != "Children", na.rm = T),
            single_headed = sum(single_headed, na.rm = T),
            female_headed = sum(female_headed, na.rm = T),
            male_headed = sum(male_headed, na.rm = T),
            child_married = sum(child_married, na.rm = T),
            child_working = sum(child_working, na.rm = T),
            members_disabled = sum(disabled_original, na.rm = T),
            child_work_non_structured = sum(child_work_non_structured, na.rm = T),
            child_work_structured = sum(child_work_structured, na.rm = T),
            child_work_family_work = sum(child_work_family_work, na.rm = T),
            child_work_other = sum(child_work_other, na.rm = T),
            health_issue_chronic = sum(as.numeric(health_issue.chronic), na.rm = T),
            not_attending_school = sum(not_attending_school, na.rm = T),
            school_aged_child = sum(school_aged_child, na.rm = T),
            #no_distance_learning = sum(no_distance_learning, na.rm = T),
            child_work_study = sum(child_work_study, na.rm = T),
            unemployed_seek_work = sum(unemployed_seek_work, na.rm = T),
            adults = sum(adults, na.rm = T),              
            head_seek_work = sum(head_seek_work, na.rm = T),
            family_member = n(),
            household_disabled_sum = sum(total_disable_score,na.rm = T)
  ) 


df <- left_join(df, sum_loop) %>% mutate(
  household_disabled = case_when(household_disabled_sum > 0 ~1 ,T~0)
)

df <- df %>% mutate(
  single_headed = case_when(single_headed == 1 ~ 1, T~0),
  female_headed = case_when(female_headed == 1 ~ 1, T~0),
  male_headed = case_when(male_headed == 1 ~ 1, T~0)
)

df$hh_disabled <- case_when(df$members_disabled > 0 ~ 1, TRUE ~ 0)


loop <- loop %>%  mutate(
  single_headed = case_when(single_headed == 1 ~ 1, T~0),
  female_headed = case_when(female_headed == 1 ~ 1, T~0),
  male_headed = case_when(male_headed == 1 ~ 1, T~0)
)


##############################################################################

#df$unmet_healthcare <- df$health_accessed_number * df$weight

#df$not_residing_home <- case_when(df$not_residing_num > 0 ~ df$not_residing_num * df$weight, df$children > 0 ~ 0, TRUE ~ NA_real_)
#df$children_drop_out <- case_when(df$drop_out > 0 ~ df$drop_out * df$weight, df$school_aged_child > 0 ~ 0, TRUE ~ NA_real_)

##################################### A ######################################
df$a11 <- df$single_headed
df$a12 <- df$female_headed
df$a13 <- case_when(df$child_married > 0 ~ 1, df$children > 0 ~ 0, TRUE ~ NA_real_)

df$a14 <- case_when(df$child_working > 0 ~ 1, df$children > 0 ~ 0, TRUE ~ NA_real_)
df$a14_1 <- case_when(df$child_work_non_structured > 0 ~ 1, df$child_working > 0 ~ 0, TRUE ~ NA_real_)
df$a14_2 <- case_when(df$child_work_structured > 0 ~ 1, df$child_working > 0 ~ 0, TRUE ~ NA_real_)
df$a14_3 <- case_when(df$child_work_family_work > 0 ~ 1, df$child_working > 0 ~ 0, TRUE ~ NA_real_)
df$a14_4 <- case_when(df$child_work_other > 0 ~ 1, df$child_working > 0 ~ 0, TRUE ~ NA_real_)

df$a15_1  <- df$why_not_return.basic_services_not_enough
df$a15_2  <- df$why_not_return.children_enrolled_in_displacement
df$a15_3  <- df$why_not_return.civil_doc_required
df$a15_4  <- df$why_not_return.community_tension
df$a15_5  <- df$why_not_return.decline_to_answer
df$a15_6  <- df$why_not_return.discrimination
df$a15_7  <- df$why_not_return.dont_know
df$a15_8  <- df$why_not_return.family_assets_destroyed
df$a15_9  <- df$why_not_return.fear_trauma
df$a15_10 <- df$why_not_return.health_conditions
df$a15_11 <- df$why_not_return.hh_assets_stolen_damaged
df$a15_12 <- df$why_not_return.house_damaged_destroyed
df$a15_13 <- df$why_not_return.house_land_occupied
df$a15_14 <- df$why_not_return.immediate_family_wont_return
df$a15_15 <- df$why_not_return.lack_court
df$a15_16 <- df$why_not_return.lack_livelihoods_aoo
df$a15_17 <- df$why_not_return.lack_of_education_oppotunities
df$a15_18 <- df$why_not_return.lack_of_security_forces
df$a15_19 <- df$why_not_return.lack_security_women
df$a15_20 <- df$why_not_return.living_conditions_better
df$a15_21 <- df$why_not_return.movement_restrictions
df$a15_22 <- df$why_not_return.no_money_return
df$a15_23 <- df$why_not_return.no_transport_return
df$a15_24 <- df$why_not_return.presence_of_mines
df$a15_25 <- df$why_not_return.local_markets_not_working
df$a15_26 <- df$why_not_return.other







df$a16 <- case_when(
  df$shelter_type_area_assessed %in% c(
    "makeshift_shelter",
    "religious_building",
    "public_building",
    "non_residential",
    "tent",
    "unfinished_building",
    "rhu"
  ) ~ 1,
  TRUE ~ 0
)


df$a17 <- case_when( 
  df$enclosure_issues.lack_insulation == 0 & 
    df$enclosure_issues.leaks_light_rain == 0 & 
    df$enclosure_issues.leaks_heavy_rain == 0 & 
    df$enclosure_issues.limited_ventilation == 0 & 
    df$enclosure_issues.debris_removable == 0 & 
    df$enclosure_issues.debris_non_removable == 0 ~ 1, TRUE ~ 0
)

df$a18 <- case_when(df$idp_first_place == "yes" ~ 1, df$idp_first_place == "no" ~ 0, TRUE ~ NA_real_)

df$a22 <- case_when(df$displaced_again == "yes" ~ 1, is.na(df$displaced_again) ~ NA_real_, TRUE ~ 0)
df$a24 <- case_when(df$movement_intentions == "remain" ~ 1, is.na(df$movement_intentions) ~ NA_real_, TRUE ~ 0)
df$a25 <- case_when(df$movement_intentions12 == "current" ~ 1, is.na(df$movement_intentions12) ~ NA_real_, TRUE ~ 0)

df$a26_1 <- df$reason_to_return_to_aoo.basic_services
df$a26_2 <- df$reason_to_return_to_aoo.emotional_desire
df$a26_3 <- df$reason_to_return_to_aoo.facing_eviction
df$a26_4 <- df$reason_to_return_to_aoo.fam_released
df$a26_5 <- df$reason_to_return_to_aoo.forced_security
df$a26_6 <- df$reason_to_return_to_aoo.lack_safety_security_women_girls
df$a26_7 <- df$reason_to_return_to_aoo.limited_livelihoods_aod
df$a26_8 <- df$reason_to_return_to_aoo.limited_services
df$a26_9 <- df$reason_to_return_to_aoo.livelihood_availability_there
df$a26_10 <- df$reason_to_return_to_aoo.no_integrated_aod
df$a26_11 <- df$reason_to_return_to_aoo.no_safe_aod
df$a26_12 <- df$reason_to_return_to_aoo.other_members_returned
df$a26_13 <- df$reason_to_return_to_aoo.secure_civil_doc
df$a26_14 <- df$reason_to_return_to_aoo.secure_house_land
df$a26_15 <- df$reason_to_return_to_aoo.security_stable
df$a26_16 <- df$reason_to_return_to_aoo.uxo
df$a26_17 <- df$reason_to_return_to_aoo.other

df$a27 <- case_when(df$movement_intentions_b == "remain" ~ 1, is.na(df$movement_intentions_b) ~ NA_real_, TRUE ~ 0)
df$a28 <- case_when(df$movement_intentions_b12 == "remain" ~ 1, is.na(df$movement_intentions_b12) ~ NA_real_, TRUE ~ 0)
df$a29 <- case_when(df$local_integration == "yes" ~ 1, is.na(df$local_integration) ~ NA_real_, TRUE ~ 0)

df$a100 <- case_when(df$hh_pregnancy == "yes" & df$hh_pregnancy_location_birth %in% c("clinic", "hospital", "phcc") ~ 1, df$hh_pregnancy == "yes" ~ 0, TRUE ~ NA_real_)

df$a101_1 <- df$hh_pregnancy_home_delivery_barriers.financial_constraints
df$a101_2 <- df$hh_pregnancy_home_delivery_barriers.no_services_available
df$a101_3 <- df$hh_pregnancy_home_delivery_barriers.servcies_inappropriate
df$a101_4 <- df$hh_pregnancy_home_delivery_barriers.time_constraints
df$a101_5 <- df$hh_pregnancy_home_delivery_barriers.transportation_constraints
df$a101_6 <- df$hh_pregnancy_home_delivery_barriers.other

#################################### Coping Strategies ######################################
#they will stay same as last year
df$stress <-
  ifelse(
    df$selling_assets %in% c("no_already_did", "yes") |
      df$borrow_debt  %in% c("no_already_did", "yes") |
      df$reduce_spending %in% c("no_already_did", "yes"),
    1,
    0
  )
df$crisis <-
  ifelse(
    df$selling_transportation_means %in% c("no_already_did", "yes") |
      df$change_place  %in% c("no_already_did", "yes") |
      df$child_work_cs %in% c("no_already_did", "yes"),
    1,
    0
  )
df$emergency <-
  ifelse(
    df$child_dropout_school %in% c("no_already_did", "yes") |
      df$adult_risky  %in% c("no_already_did", "yes") |
      df$family_migrating %in% c("no_already_did", "yes") |
      df$child_forced_marriage %in% c("no_already_did", "yes"),
    1,
    0
  )



#################################### C ######################################

df$c9 <- case_when(df$difficulty_accessing_services == "yes" ~ 1,
                   is.na(df$difficulty_accessing_services) ~ NA_real_, TRUE ~ 0)

#################################### D ######################################

df$d1_1 <- df$info_aid.access_education
df$d1_2 <- df$info_aid.access_water_services
df$d1_3 <- df$info_aid.commodity_prices
df$d1_4 <- df$info_aid.complaint_mechanisms
df$d1_5 <- df$info_aid.cooking_fuel_firewood
df$d1_6 <- df$info_aid.covid_19
df$d1_7 <- df$info_aid.find_work
df$d1_8 <- df$info_aid.finding_missing_ppl
df$d1_9 <- df$info_aid.get_food
df$d1_10 <- df$info_aid.get_gbv_support
df$d1_11 <- df$info_aid.get_healthcare_medical_attention
df$d1_12 <- df$info_aid.get_more_money
df$d1_13 <- df$info_aid.get_psychosocial_services
df$d1_14 <- df$info_aid.get_shelter
df$d1_15 <- df$info_aid.get_transport
df$d1_16 <- df$info_aid.info_harassment
df$d1_17 <- df$info_aid.info_local_integration
df$d1_18 <- df$info_aid.info_nutrition
df$d1_19 <- df$info_aid.info_org_how_contribute
df$d1_20 <- df$info_aid.info_relocation
df$d1_21 <- df$info_aid.info_return_aoo
df$d1_22 <- df$info_aid.legal_rights_hlp
df$d1_23 <- df$info_aid.news_aoo
df$d1_24 <- df$info_aid.news_here
df$d1_25 <- df$info_aid.register_for_aid
df$d1_26 <- df$info_aid.request_renew_core_doc
df$d1_27 <- df$info_aid.none
df$d1_28 <- df$info_aid.other

df$d2_1 <- df$info_provider.camp_management
df$d2_2 <- df$info_provider.friends
df$d2_3 <- df$info_provider.local_authorities
df$d2_4 <- df$info_provider.mukhtars
df$d2_5 <- df$info_provider.national_authorities
df$d2_6 <- df$info_provider.ngo
df$d2_7 <- df$info_provider.none
df$d2_8 <- df$info_provider.other
df$d2_9 <- df$info_provider.outreach_volunteers
df$d2_10 <- df$info_provider.religious
df$d2_11 <- df$info_provider.schools
df$d2_12 <- df$info_provider.social_media

df$d3_1 <- df$info_mode.direct_obs
df$d3_2 <- df$info_mode.face_cmmunic
df$d3_3 <- df$info_mode.facebook_messenger
df$d3_4 <- df$info_mode.loud_speakers
df$d3_5 <- df$info_mode.mobile
df$d3_6 <- df$info_mode.newspapers
df$d3_7 <- df$info_mode.none
df$d3_8 <- df$info_mode.other
df$d3_9 <- df$info_mode.other_social
df$d3_10 <- df$info_mode.radio
df$d3_11 <- df$info_mode.telephone
df$d3_12 <- df$info_mode.television
df$d3_13 <- df$info_mode.viber
df$d3_14 <- df$info_mode.whatsapp
df$d3_15 <- df$info_mode.notice_board


df$d5_1 <- df$ideal_info_provider.camp_management
df$d5_2 <- df$ideal_info_provider.friends
df$d5_3 <- df$ideal_info_provider.local_authorities
df$d5_4 <- df$ideal_info_provider.mukhtars
df$d5_5 <- df$ideal_info_provider.national_authorities
df$d5_6 <- df$ideal_info_provider.ngo
df$d5_7 <- df$ideal_info_provider.none
df$d5_8 <- df$ideal_info_provider.other
df$d5_9 <- df$ideal_info_provider.outreach_volunteers
df$d5_10 <- df$ideal_info_provider.religious
df$d5_11 <- df$ideal_info_provider.schools
df$d5_12 <- df$ideal_info_provider.social_media


df$d4 <- case_when(is.na(df$aid_satisfaction) ~ NA_real_,
                   df$aid_satisfaction == "did_not_receive_aid_30_days" ~ 0,
                   T~1)  

df$d6 <- case_when(
  df$aid_satisfaction == "yes" ~ 1, 
  is.na(df$aid_satisfaction) ~ NA_real_,
  df$aid_satisfaction == "did_not_receive_aid_30_days" ~ NA_real_,
  TRUE ~ 0
)





df$d7 <- case_when(
  df$aid_not_satisfied.quantity == 1 ~ 1, is.na(df$aid_not_satisfied) ~ NA_real_, TRUE ~ 0
)

df$d8_1 <- df$ideal_info_mode.direct_obs
df$d8_2 <- df$ideal_info_mode.face_cmmunic
df$d8_3 <- df$ideal_info_mode.facebook_messenger
df$d8_4 <- df$ideal_info_mode.loud_speakers
df$d8_5 <- df$ideal_info_mode.mobile
df$d8_6 <- df$ideal_info_mode.newspapers
df$d8_7 <- df$ideal_info_mode.none
df$d8_8 <- df$ideal_info_mode.other
df$d8_9 <- df$ideal_info_mode.other_social
df$d8_10 <- df$ideal_info_mode.radio
df$d8_11 <- df$ideal_info_mode.telephone
df$d8_12 <- df$ideal_info_mode.television
df$d8_13 <- df$ideal_info_mode.viber
df$d8_14 <- df$ideal_info_mode.whatsapp
df$d8_15 <- df$ideal_info_mode.notice_board

df$d10 <- case_when(
  df$complaint_mechanisms == "yes" ~ 1, is.na(df$complaint_mechanisms) ~ NA_real_, TRUE ~ 0
)

df$d12_1 <-  df$people_community_unable_access_info.not_aware
df$d12_2 <-  df$people_community_unable_access_info.unaccompanied_children
df$d12_3 <-  df$people_community_unable_access_info.people_serious_health_conditions
df$d12_4 <-  df$people_community_unable_access_info.ppl_special_legal_physical_needs
df$d12_5 <-  df$people_community_unable_access_info.single_women 
df$d12_6 <-  df$people_community_unable_access_info.women_headed_households
df$d12_7 <-  df$people_community_unable_access_info.disabled_persons
df$d12_8 <-  df$people_community_unable_access_info.people_w_mh_pbms
df$d12_9 <-  df$people_community_unable_access_info.older_ppl
df$d12_10 <- df$people_community_unable_access_info.ppl_w_diverse_gender_identity
df$d12_11 <- df$people_community_unable_access_info.people_who_cannot_read
df$d12_12 <- df$people_community_unable_access_info.other


df$d13_1 <- df$need_priorities.drinking_water
df$d13_2 <- df$need_priorities.education_children_less_18
df$d13_3 <- df$need_priorities.food
df$d13_4 <- df$need_priorities.healthcare
df$d13_5 <- df$need_priorities.hygiene_nfis
df$d13_6 <- df$need_priorities.info_child_protection
df$d13_7 <- df$need_priorities.info_gbc
df$d13_8 <- df$need_priorities.legal_support
df$d13_9 <- df$need_priorities.livelihoods
df$d13_10 <- df$need_priorities.none
df$d13_11 <- df$need_priorities.other
df$d13_12 <- df$need_priorities.psychosocial_support
df$d13_13 <- df$need_priorities.repay_debt
df$d13_14 <- df$need_priorities.seeds_agricultural_inputs
df$d13_15 <- df$need_priorities.shelter_housing



# df$d14_1 <- df$need_priorities.drinking_water
# df$d14_2 <- df$need_priorities.education_children_less_18
# df$d14_3 <-df$need_priorities.food
# df$d14_4 <-df$need_priorities.healthcare
# df$d14_5 <-df$need_priorities.hygiene_nfis
# df$d14_6 <-df$need_priorities.info_child_protection
# df$d14_7 <-df$need_priorities.info_gbc
# df$d14_8 <-df$need_priorities.legal_support
# df$d14_9 <-df$need_priorities.livelihoods
# df$d14_10 <-df$need_priorities.none
# df$d14_11 <-df$need_priorities.other
# df$d14_12 <-df$need_priorities.psychosocial_support
# df$d14_13 <-df$need_priorities.repay_debt
# df$d14_14 <-df$need_priorities.seeds_agricultural_inputs
# df$d14_15 <- df$need_priorities.shelter_housing


df$drop_out_hh <- case_when(is.na(df$drop_out) ~ NA_real_, 
                            df$drop_out > 0 ~1,
                            T~ 0)

# df$d15_1 <- df$drop_out_reasons.
df$d15_2 <- df$drop_out_reasons.cannot_afford
df$d15_3 <- df$drop_out_reasons.lack_schools
df$d15_4 <- df$drop_out_reasons.moved_area
df$d15_5 <- df$drop_out_reasons.protection_risk_to_school
df$d15_6 <- df$drop_out_reasons.protection_risk_at_school
df$d15_7 <- df$drop_out_reasons.lack_interest_child
df$d15_8 <- df$drop_out_reasons.lack_interest_parents
df$d15_9 <- df$drop_out_reasons.unable_reg_entoll
df$d15_10 <- df$drop_out_reasons.missing_docs
df$d15_11 <- df$drop_out_reasons.working_home_farm
df$d15_12 <- df$drop_out_reasons.working_job
df$d15_13 <- df$drop_out_reasons.child_married
df$d15_14 <- df$drop_out_reasons.disability
df$d15_15 <- df$drop_out_reasons.overcrowded
df$d15_16 <- df$drop_out_reasons.lack_staff
df$d15_17 <- df$drop_out_reasons.infrastructure_bad
df$d15_18 <- df$drop_out_reasons.poor_quality
df$d15_19 <- df$drop_out_reasons.unsuitable_curriculum
df$d15_20 <- df$drop_out_reasons.unsuitable_teaching
df$d15_21 <- df$drop_out_reasons.covid_closure
df$d15_22 <- df$drop_out_reasons.no_remote_teaching
df$d15_23 <- df$drop_out_reasons.no_remote_equipment
df$d15_24 <- df$drop_out_reasons.no_tech_for_remote
df$d15_25 <- df$drop_out_reasons.no_elec_power
df$d15_26 <- df$drop_out_reasons.other
df$d15_27 <- df$drop_out_reasons.do_not_know
df$d15_28 <- df$drop_out_reasons.decline








df$d16_1 <- case_when(df$fetch_water == "on_premises" ~ 1, is.na(df$fetch_water) ~ NA_real_,T~0 )
df$d16_2 <- case_when(df$fetch_water == "less_than_5" ~ 1,  is.na(df$fetch_water) ~ NA_real_, T~0 )
df$d16_3 <- case_when(df$fetch_water == "515" ~ 1, is.na(df$fetch_water) ~ NA_real_, T~0 )
df$d16_4 <- case_when(df$fetch_water == "1630" ~ 1,is.na(df$fetch_water) ~ NA_real_, T~0 )
df$d16_5 <- case_when(df$fetch_water == "more_than_30" ~ 1,is.na(df$fetch_water) ~ NA_real_, T~0 )
df$d16_6 <- case_when(df$fetch_water == "don't_know" ~ 1,is.na(df$fetch_water) ~ NA_real_, T~0 )
 


##################################### F ######################################

#we want to report on those who have property damaged
df$f7 <- case_when(is.na(df$received_compensation) ~ NA_real_,
                   df$property_damaged == "no" ~ NA_real_,
                   df$received_compensation == "yes" ~ 1,
                   TRUE ~ 0)

df$f7b_1 <- df$reasons_no_compensations.not_aware
df$f7b_2 <- df$reasons_no_compensations.didnt_pay_bribe
df$f7b_3 <- df$reasons_no_compensations.difficult_procedure
df$f7b_4 <- df$reasons_no_compensations.unclear_information
df$f7b_5 <- df$reasons_no_compensations.delayed_compensation
df$f7b_6 <- df$reasons_no_compensations.dont_have_docs
df$f7b_7 <- df$reasons_no_compensations.other

df$f8 <- case_when(df$female_headed == 1 & df$hlp_document == "no" ~ 1, 
                   df$female_headed == 1 ~ 0, TRUE ~ NA_real_)

df$f10_1 <- df$reasons_missing_doc.lost_or_left_behind 
df$f10_2 <- df$reasons_missing_doc.pending_application
df$f10_3 <- df$reasons_missing_doc.destroyed_confiscated
df$f10_4 <- df$reasons_missing_doc.no_access_cad
df$f10_5 <- df$reasons_missing_doc.lack_information
df$f10_6 <- df$reasons_missing_doc.refused_by_authorities
df$f10_7 <- df$reasons_missing_doc.refused_by_security
df$f10_8 <- df$reasons_missing_doc.refused_security_clearance
df$f10_9 <- df$reasons_missing_doc.high_cost
df$f10_10 <- df$reasons_missing_doc.complex_process
df$f10_11 <- df$reasons_missing_doc.havent_tried
df$f10_12 <- df$reasons_missing_doc.other

df$f11 <- case_when(
  df$impact_presence_explosive_ordnance.hh_member_killed == 1 |
    df$impact_presence_explosive_ordnance.hh_member_injured_disabled == 1 ~ 1,
  is.na(df$impact_presence_explosive_ordnance) ~ NA_real_,
  TRUE ~ 0
)

df$f12 <- case_when( 
  df$impact_presence_explosive_ordnance.move_elsewhere == 1 |
    df$impact_presence_explosive_ordnance.limited_access_health == 1 |
    df$impact_presence_explosive_ordnance.limited_access_education == 1 |
    df$impact_presence_explosive_ordnance.limited_access_markets == 1 |
    df$impact_presence_explosive_ordnance.limited_access_livelihoods == 1 |
    df$impact_presence_explosive_ordnance.limited_movement == 1 |
    df$impact_presence_explosive_ordnance.hh_member_killed == 1 |
    df$impact_presence_explosive_ordnance.hh_member_injured_disabled == 1 |
    df$impact_presence_explosive_ordnance.impact_psychological_wellbeing == 1 |
    df$impact_presence_explosive_ordnance.other == 1 ~ 1, TRUE ~ 0
)

df$f13 <- case_when(
  df$play_role_local_decision_making == "yes" ~ 1 , is.na(df$play_role_local_decision_making) ~ NA_real_, TRUE ~ 0
)

##################################### G #######################################

df$g1 <- case_when(df$pop_group == "In-camp IDP HH" ~ NA_real_,
                   df$informal_site == "yes" ~ 1, TRUE ~ 0)

df$g4 <- case_when(df$not_attending_school > 0 ~ 1, df$school_aged_child > 0 ~ 0, TRUE ~ NA_real_)

df$g7_1 <- df$reasons_not_attend.cannot_affort_school_expenses
df$g7_2 <- df$reasons_not_attend.children_working
df$g7_3 <- df$reasons_not_attend.curriculum_not_adapted
df$g7_4 <- df$reasons_not_attend.health_condition_child
df$g7_5 <- df$reasons_not_attend.lack_interest_education
df$g7_6 <- df$reasons_not_attend.limited_access_school
df$g7_7 <- df$reasons_not_attend.no_access_distance_learning_no_alternative_education
df$g7_8 <- df$reasons_not_attend.no_access_distance_learning_no_required_resources
df$g7_9 <- df$reasons_not_attend.not_able_to_register_school
df$g7_10 <- df$reasons_not_attend.none
df$g7_11 <- df$reasons_not_attend.parental_refusal
df$g7_12 <- df$reasons_not_attend.school_infrastructure_poor
df$g7_13 <- df$reasons_not_attend.school_not_functioning
df$g7_14 <- df$reasons_not_attend.school_not_safe
df$g7_15 <- df$reasons_not_attend.school_overcrowded
df$g7_16 <- df$reasons_not_attend.other



df$no_food_score <- case_when(df$no_food == "yes" ~ 1, TRUE ~ 0)
df$no_food_score <-
  df$no_food_score * case_when(
    df$no_food_freq == "often" ~ 2,
    df$no_food_freq %in% c("rarely", "sometimes") ~ 1,
    TRUE ~ 0
  )

df$not_eating_score <-
  case_when(df$not_eating == "yes" ~ 1, TRUE ~ 0)
df$not_eating_score <-
  df$not_eating_score * case_when(
    df$not_eating_freq == "often" ~ 2,
    df$not_eating_freq %in% c("rarely", "sometimes") ~ 1,
    TRUE ~ 0
  )

df$hungry_score <- case_when(df$hungry == "yes" ~ 1, TRUE ~ 0)
df$hungry_score <-
  df$hungry_score * case_when(df$hungry_freq == "often" ~ 2,
                              df$hungry_freq %in% c("rarely", "sometimes") ~ 1,
                              TRUE ~ 0)

df$household_hunger_scale <-
  df$no_food_score + df$not_eating_score + df$hungry_score

df$g15 <- case_when(df$household_hunger_scale <= 1 ~ "Little to no hunger in the household (0-1)", 
                    df$household_hunger_scale <= 3 ~ "Moderate hunger in the household (2-3)",
                    df$household_hunger_scale <= 6 ~ "Severe hunger in the household (4-6)")

df$fcs <-
  (df$cereals * 2) + (df$nuts_seed * 3) + df$vegetables + df$fruits + (df$meat_fish_eggs * 4) + (df$milk_dairy * 4) + (df$sweets * 0.5) + (df$oil_fats * 0.5)

df$fcs_category <- case_when(df$fcs <= 28 ~ "Poor",
                             df$fcs <= 42 ~ "Borderline",
                             T ~ "Acceptable")

df$g16_1 <- case_when(df$fcs_category == "Acceptable" ~ 1, T ~ 0)
df$g16_2 <- case_when(df$fcs_category == "Borderline" ~ 1, T ~ 0)
df$g16_3 <- case_when(df$fcs_category == "Poor" ~ 1, T ~ 0)

df$g19 <- case_when(df$child_work_study > 1 ~ 1, df$school_aged_child == 0 ~ NA_real_, TRUE ~ 0)

df$g25 <- case_when(df$distance_clinic < 60 | df$distance_hospital < 60 ~ 1, TRUE ~ 0)
df$g26 <- case_when(df$distance_hospital < 60 & 
                      df$hospital_emergency_ser == "yes" & 
                      df$hospital_maternity_ser == "yes" & 
                      df$hospital_pediatric_ser == "yes" & 
                      df$hospital_surgical_ser == "yes" ~ 1, TRUE ~ 0)
df$g27 <- case_when(df$health_accessed == "yes" ~ as.numeric(df$health_accessed_number), TRUE ~ 0)
df$g28 <- case_when(df$distance_clinic < 60 ~ 1, TRUE ~ 0)
df$g29 <- case_when(df$distance_hospital < 60 ~ 1, TRUE ~ 0)

df$g32 <- case_when(df$health_barriers_repro.no_services == 1 |
                      df$health_barriers_repro.financial_constraints == 1 |
                      df$health_barriers_repro.transport_distance == 1 |
                      df$health_barriers_repro.lack_documentation == 1 |
                      df$health_barriers_repro.unappropriate_services == 1 |
                      df$health_barriers_repro.other == 1 ~ 1,
                    TRUE ~ 0)


#RM: it says among households reporting unmet health care needs which means it must be NA if they don't have a case where they tried to access healthcare and they couldn't
df$g34_1 <- case_when(df$difficulty_accessing_health_services.fear_covid == 1 & df$health_accessed_number > 0 ~ 1, df$health_accessed_number > 0 ~ 0, TRUE ~ NA_real_)
df$g34_2 <- case_when(df$difficulty_accessing_health_services.fear_distrust == 1 & df$health_accessed_number > 0 ~ 1, df$health_accessed_number > 0 ~ 0, TRUE ~ NA_real_)
df$g34_3 <- case_when(df$difficulty_accessing_health_services.high_costs == 1 & df$health_accessed_number > 0 ~ 1, df$health_accessed_number > 0 ~ 0, TRUE ~ NA_real_)
df$g34_4 <- case_when(df$difficulty_accessing_health_services.insufficient_nb_female_staff == 1 & df$health_accessed_number > 0 ~ 1, df$health_accessed_number > 0 ~ 0, TRUE ~ NA_real_)
df$g34_5 <- case_when(df$difficulty_accessing_health_services.long_distance_transportation_constraints == 1 & df$health_accessed_number > 0 ~ 1, df$health_accessed_number > 0 ~ 0, TRUE ~ NA_real_)
df$g34_6 <- case_when(df$difficulty_accessing_health_services.medical_staff_refused_treament == 1 & df$health_accessed_number > 0 ~ 1, df$health_accessed_number > 0 ~ 0, TRUE ~ NA_real_)
df$g34_7 <- case_when(df$difficulty_accessing_health_services.no_access_qualified_staff == 1 & df$health_accessed_number > 0 ~ 1, df$health_accessed_number > 0 ~ 0, TRUE ~ NA_real_)
df$g34_8 <- case_when(df$difficulty_accessing_health_services.no_referral == 1 & df$health_accessed_number > 0 ~ 1, df$health_accessed_number > 0 ~ 0, TRUE ~ NA_real_)
df$g34_9 <- case_when(df$difficulty_accessing_health_services.no_medicine_available == 1 & df$health_accessed_number > 0 ~ 1, df$health_accessed_number > 0 ~ 0, TRUE ~ NA_real_)
df$g34_10 <- case_when(df$difficulty_accessing_health_services.no_treatment_my_disability == 1 & df$health_accessed_number > 0 ~ 1, df$health_accessed_number > 0 ~ 0, TRUE ~ NA_real_)
df$g34_11 <- case_when(df$difficulty_accessing_health_services.not_inclusive_disabilities == 1 & df$health_accessed_number > 0 ~ 1, df$health_accessed_number > 0 ~ 0, TRUE ~ NA_real_)
df$g34_12 <- case_when(df$difficulty_accessing_health_services.not_open == 1 & df$health_accessed_number > 0 ~ 1, df$health_accessed_number > 0 ~ 0, TRUE ~ NA_real_)
df$g34_13 <- case_when(df$difficulty_accessing_health_services.not_tried == 1 & df$health_accessed_number > 0 ~ 1, df$health_accessed_number > 0 ~ 0, TRUE ~ NA_real_)
df$g34_14 <- case_when(df$difficulty_accessing_health_services.other == 1 & df$health_accessed_number > 0 ~ 1, df$health_accessed_number > 0 ~ 0, TRUE ~ NA_real_)
df$g34_15 <- case_when(df$difficulty_accessing_health_services.pb_civil_documents == 1 & df$health_accessed_number > 0 ~ 1, df$health_accessed_number > 0 ~ 0, TRUE ~ NA_real_)
df$g34_16 <- case_when(df$difficulty_accessing_health_services.waiting_time_too_long == 1 & df$health_accessed_number > 0 ~ 1, df$health_accessed_number > 0 ~ 0, TRUE ~ NA_real_)
df$g34_17 <- case_when(df$difficulty_accessing_health_services.no_issues == 1 & df$health_accessed_number > 0 ~ 1, df$health_accessed_number > 0 ~ 0, TRUE ~ NA_real_)

df$g35 <- case_when(df$health_issue_chronic > 0 ~ 1, TRUE ~ 0)


df$g36 <- case_when(
  df$disciplinary_measures.shouted == 1 | df$disciplinary_measures.spanked == 1 ~ 1, is.na(df$disciplinary_measures) ~ NA_real_, TRUE ~ 0
)

df$g37 <- case_when(df$how_much_debt/df$num_hh_member > 90000 ~ 1, TRUE ~ 0)

df$g38 <- case_when(
  df$reasons_for_debt %in% c("basic_hh_expenditure", "health", "food", "education") ~ 1, 
  is.na(df$reasons_for_debt) ~ NA_real_, TRUE ~ 0
)

df$g41 <- case_when(
  df$market_barriers.too_far == 1 |
    df$market_barriers.other == 1 |
    df$market_barriers.lack_transportation == 1 |
    df$market_barriers.too_expensive == 1 |
    df$market_barriers.limited_variety_quantity == 1 |
    df$market_barriers.security_concerns == 1 ~ 1,
    is.na(df$market_barriers) ~ NA_real_,
  TRUE ~ 0
)

df$g42 <- case_when(df$head_seek_work > 0 ~ 1, TRUE ~ 0)
df$g44 <- case_when(df$unemployed_seek_work > 0 ~ 1,  TRUE ~ 0)

df$g45_1 <- df$employment_primary_barriers.fear_of_gbv
df$g45_2 <- df$employment_primary_barriers.increased_competition
df$g45_3 <- df$employment_primary_barriers.jobs_far
df$g45_4 <- df$employment_primary_barriers.lack_jobs_women
df$g45_5 <- df$employment_primary_barriers.lack_of_connections
df$g45_6 <- df$employment_primary_barriers.none
df$g45_7 <- df$employment_primary_barriers.only_low_available
df$g45_8 <- df$employment_primary_barriers.underqualified_for_jobs
df$g45_9 <- df$employment_primary_barriers.other

df$g48 <- case_when(
  df$primary_livelihood == "ngo_charity_assistance" ~ 1, is.na(df$primary_livelihood) ~ NA_real_ , TRUE ~ 0
)

df$g51 <- case_when(
  df$pds_card == "no" |
    df$id_card_a18 == "no" |
    df$nationality_cert_a18 == "no" |
    df$id_card_u18 == "no" |
    df$nationality_cert_u18 == "no" |
    df$birth_cert_u18 == "no" ~ 1, 
  TRUE ~ 0
)

df$g52 <- case_when(
  df$id_card_u18 == "no" |
    df$nationality_cert_u18 == "no" |
    df$birth_cert_u18 == "no" ~ 1,
  TRUE ~ 0
)

df$adult_missing_document <- case_when(
  df$id_card_a18 == "no" |
    df$nationality_cert_a18 == "no"  ~ 1,
  TRUE ~ 0
)

not_residing_reason<- c("not_residing_armed_actors","not_residing_detained",
                        "not_residing_kidnapped","not_residing_married","not_residing_missing",
                       "not_residing_seek_employment", "not_residing_study")


df <- df %>% mutate(
  not_residing_num = rowSums(df[not_residing_reason],na.rm = T)
) %>% mutate(
  not_residing_armed_actors = case_when(not_residing_num == 0 ~NA_integer_,T~not_residing_armed_actors),
  not_residing_detained = case_when(not_residing_num == 0 ~NA_integer_,T~not_residing_detained),
  not_residing_kidnapped = case_when(not_residing_num == 0 ~NA_integer_,T~not_residing_kidnapped),
  not_residing_married = case_when(not_residing_num == 0 ~NA_integer_,T~not_residing_married),
  not_residing_missing = case_when(not_residing_num == 0 ~NA_integer_,T~not_residing_missing),
  not_residing_seek_employment = case_when(not_residing_num == 0 ~NA_integer_,T~not_residing_seek_employment),
  not_residing_study = case_when(not_residing_num == 0 ~NA_integer_,T~not_residing_study)
) %>% mutate(
  not_residing = case_when(not_residing == "yes" ~ NA_character_,T~not_residing)
) %>% mutate(
  not_residing = case_when(not_residing_num > 0~ "yes",T~not_residing)
) %>% mutate(
  not_residing_num = case_when(not_residing_num == 0 ~ NA_real_,T~not_residing_num)
) %>% mutate(
  not_residing = case_when(is.na(not_residing) ~ "no", T~not_residing)
)

df$calc_separate_hh <- df$not_residing

df$g53b_1 <- case_when(df$not_residing != "yes" ~ NA_real_, df$not_residing_armed_actors > 0 ~ 1,  TRUE ~ 0) 
df$g53b_2 <- case_when(df$not_residing != "yes" ~ NA_real_, df$not_residing_detained > 0 ~ 1, TRUE ~ 0) 
df$g53b_3 <- case_when(df$not_residing != "yes" ~ NA_real_, df$not_residing_kidnapped > 0 ~ 1,  TRUE ~ 0) 
df$g53b_4 <- case_when(df$not_residing != "yes" ~ NA_real_, df$not_residing_married > 0 ~ 1,  TRUE ~ 0) 
df$g53b_5 <- case_when(df$not_residing != "yes" ~ NA_real_, df$not_residing_missing > 0 ~ 1, TRUE ~ 0) 
df$g53b_6 <- case_when(df$not_residing != "yes" ~ NA_real_, df$not_residing_seek_employment > 0 ~ 1,  TRUE ~ 0) 
df$g53b_7 <- case_when(df$not_residing != "yes" ~ NA_real_, df$not_residing_study > 0 ~ 1,  TRUE ~ 0) 

df$g54 <- case_when(
  df$restriction_clearance == "yes" | 
    df$restriction_time == "yes" |
    df$restriction_reason == "yes" |
    df$restriction_physical == "yes"|
    df$restriction_documents == "yes"|
    df$restriction_other == "yes" ~ 1,
  TRUE ~ 0
)

df$health_accessed_number_at_least_one <- case_when(is.na(df$health_accessed_number) ~ NA_real_,
                    df$health_accessed_number > 0 ~ 1, T~0)


df$g56 <- case_when(df$hh_member_distress == "yes" & df$child_distress_number > 0 ~ 1, df$children > 0 ~ 0, TRUE ~ NA_real_)
df$g57 <- case_when(df$hh_member_distress == "yes" & df$adult_distress_number > 0 ~ 1, TRUE ~ 0)

df$g63 <- case_when(df$feel_unsafe.distribution_areas == 1 |
                      df$feel_unsafe.facilities == 1 |
                      df$feel_unsafe.markets == 1 | 
                      df$feel_unsafe.polica_stations == 1 | 
                      df$feel_unsafe.social_areas == 1 | 
                      df$feel_unsafe.water_points == 1 | 
                      df$feel_unsafe.way_to_centers == 1 |
                      df$feel_unsafe.way_to_school == 1 | 
                      df$feel_unsafe.way_to_work == 1 ~ 1, 
                    TRUE ~ 0
)

df$g64 <- case_when(df$hh_risk_eviction == "yes" ~ 1, TRUE ~ 0)

df$g65_1 <- df$hh_main_risks.authorities_request
df$g65_2 <- df$hh_main_risks.confiscation
df$g65_3 <- df$hh_main_risks.dispute
df$g65_4 <- df$hh_main_risks.lack_funds
df$g65_5 <- df$hh_main_risks.no_agreement
df$g65_6 <- df$hh_main_risks.no_longer_hosted
df$g65_7 <- df$hh_main_risks.occupied
df$g65_8 <- df$hh_main_risks.other
df$g65_9 <- df$hh_main_risks.owner_request
df$g65_10 <- df$hh_main_risks.unaccepted_by_community

df$g66 <- case_when(df$hlp_document == "no" ~ 1, is.na(df$hlp_document) ~ NA_real_, TRUE ~ 0)

#we're not asking specifically about all the HLP issues, we rather ask for the
#reason not intending to return because there are a few types of HLP issues
df$g67 <-
  case_when(
    df$why_not_return.house_land_occupied == 1 |
      df$why_not_return.house_damaged_destroyed == 1 |
      df$why_not_return.family_assets_destroyed == 1 ~ 1,
    is.na(df$why_not_return) ~ NA_real_,
    TRUE ~ 0
  )
df$g68 <- case_when(df$hh_dispute == "yes" ~ 1, is.na(df$hh_dispute) ~ NA_real_, TRUE ~ 0)
df$g73 <- case_when(df$why_not_return.presence_of_mines == 1 ~ 1, is.na(df$why_not_return) ~ NA_real_, TRUE ~ 0)
df$g74 <- case_when(df$risk_education == "yes" ~ 1, is.na(df$risk_education) ~ NA_real_, TRUE ~ 0)

df$g85 <- case_when(
  df$nfi_priority_needs.bedding_items == 1 |
    df$nfi_priority_needs.mattresses_sleeping_mats == 1 |
    df$nfi_priority_needs.blankets == 1 |
    df$nfi_priority_needs.cooking_utensils == 1 |
    df$nfi_priority_needs.cooking_stove == 1 |
    df$nfi_priority_needs.winter_heaters == 1 |
    df$nfi_priority_needs.clothing == 1 |
    df$nfi_priority_needs.heating_cooking_fuel == 1 ~ 1,
  TRUE ~ 0
)

df$g89 <- case_when(
  df$shelter_better.protec_hazards +
    df$shelter_better.improve_safety +
    df$shelter_better.improve_privacy +
    df$shelter_better.protect_climate >= 2 ~ 1, 
  TRUE ~ 0
)


df$g94 <- case_when(
  df$sufficient_water_drinking == "yes" &
    df$sufficient_water_cooking == "yes" &
    df$sufficient_water_hygiene == "yes" &
    df$sufficient_water_other == "yes" ~ 1, 
  TRUE ~ 0
)

#using water trucking by personal preference is considered as improved by WASH Cluster
df$g95 <- case_when(
  df$drinking_water_source %in% 
    c("improved_network_private",
      "improved_network_comm",
      "improved_borehole",
      "improved_prot_well",
      "improved_prot_tank", 
      "improved_prot_spring") |
    (df$drinking_water_source == "improved_bottled_water" & 
       df$bottled_water_reason == "personal_preference")|
    (df$drinking_water_source == "unimproved_water_trucking" & 
       df$water_trucking_reason == "personal_preference") ~ 1,
  TRUE ~ 0
)

df$g96_a <- case_when(  is.na(df$treat_drink_water) ~ NA_real_,
                        df$drinking_water_source %in% c("improved_bottled_water","unimproved_water_trucking") ~ NA_real_, 
                        df$treat_drink_water %in% c("always", "sometimes") ~ 1, 
                        TRUE ~ 0)

df$g96_b <- case_when(df$problems_water_quality.not_clear == 1|
                        df$problems_water_quality.taste == 1|
                        df$problems_water_quality.smell == 1|
                        df$problems_water_quality.other == 1 ~ 1, TRUE ~ 0)

df$g97 <- case_when(
  df$latrines %in% c(
    "vip_pit",
    "flush",
    "pit_slab"
  ) ~ 1,
  TRUE ~ 0
)

df$g98 <- case_when(
  df$handwashing_facility %in% c(
    "sink_tap_water",
    "tippy_tap",
    "buckets_taps"
  ) ~ 1,
  TRUE ~ 0
)


df$g99 <- case_when(df$access_soap == "yes" ~ 1, TRUE ~ 0)

df$g101 <- case_when(df$food_exp/df$tot_expenses >= 0.4 ~ 1, TRUE ~ 0)
df$g102 <- case_when(df$medical_exp/df$tot_expenses >= 0.25 ~ 1, TRUE ~ 0)

df$g103 <- case_when(df$inc_employment_pension < 440000 ~ 1, TRUE ~ 0)
df$g104 <- case_when(df$female_headed == 1 & df$g103 == 1 ~ 1, df$female_headed == 1 ~ 0, TRUE ~ NA_real_)

df$g105 <- case_when(df$drop_out > 1 ~ 1, TRUE ~ 0)


df$number_missing_docs_below18_at_least_one <- case_when(is.na(df$number_missing_docs_below18) ~ NA_real_,
                                                         df$number_missing_docs_below18 > 0 ~1 , T~0 )

df$number_missing_docs_above18_at_least_one <- case_when(is.na(df$number_missing_docs_above18) ~ NA_real_,
                                                         df$number_missing_docs_above18 > 0 ~1 , T~0 )


df$not_all_adult_vote <- case_when(df$over_18_eligible_vote == "not_all_because_legal_reg_missdocs" ~ 1, T~0)



############################################### Post ECHO ################################################################

loop <- loop %>% mutate(
  school_enrolment_female = case_when(!age %in% 6:17 ~ NA_real_,
                                       sex  == "male" ~ NA_real_,
                                       is.na(school_formal_enrollment) ~ NA_real_,
                                       school_formal_enrollment == "decline_to_answer" ~ NA_real_,
                                       school_formal_enrollment == "dont_know" ~ NA_real_,
                                       school_formal_enrollment == "yes" ~ 1,
                                       T~ 0),
  
  school_enrolment_male = case_when(!age %in% 6:17 ~ NA_real_,
                                     sex  == "female" ~ NA_real_,
                                     is.na(school_formal_enrollment) ~ NA_real_,
                                     school_formal_enrollment == "decline_to_answer" ~ NA_real_,
                                     school_formal_enrollment == "dont_know" ~ NA_real_,
                                     school_formal_enrollment == "yes" ~ 1,
                                     T~ 0)
)


# df <- df %>% mutate(
#   rcsi_score_4 = cheaper_food*1 + reduce_meals*2 + consuming_less_per_meal*1 + reduce_for_adults*2,
#   rcsi_score_4_up = cheaper_food*1 + reduce_meals*1 + consuming_less_per_meal*1 + reduce_for_adults*3,
#   
# )




##########################################################################################################################

