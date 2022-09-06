


df <- read.csv("clean_data_with_rs_indicators.csv")
df <- df %>% filter(!is.infinite(Distance)) %>% filter(Distance != 0)

df$need_priorities.drinking_water <- case_when(df$need_priorities.drinking_water  == T~ 1,T~ 0)
df$need_priorities.food <- df$need_priorities.food %>% as.integer()

df$Distance <- df$Distance %>% log()





a <- weighted_t_test(data = df,dependent_variables = c("need_priorities.drinking_water","need_priorities.food") ,
                     independent_variable = "Distance",
                     strata = "strata_and_pop_group",
                     survey_weights = "survey_weight")










weighted_t_test <- function(data,dependent_variables,independent_variable,strata,survey_weights){

  
  data <- data %>% filter(!is.na(data[[independent_variable]]))
  data <- data %>% filter(! is.infinite(data[[independent_variable]]))
  
  out_liers_independent_variable <- boxplot.stats(data[[independent_variable]])$out
  data <- data %>% filter(!data[[independent_variable]] %in% out_liers_independent_variable)

dfsvy <- as_survey(data,strata = strata,weights = survey_weights)

test_result <- list()

for(i in dependent_variables){

f_mula <- formula(paste0(independent_variable, "~",i ))
test <- survey::svyttest(f_mula,design = dfsvy)



test_result[[i]] <- data.frame(
  independent_variable = independent_variable,
  dependent_variables = i,
  t_value = test$statistic %>% as.numeric(),
  p_value = test$p.value %>% as.numeric(),
  df = test$parameter %>% as.numeric(),
  difference_in_mean = test$estimate %>% as.numeric(),
  method = test$method
)


}

final_result <- do.call("bind_rows",test_result)

return(final_result)


}





