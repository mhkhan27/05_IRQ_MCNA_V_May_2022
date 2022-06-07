calculateDifferences <- function(data, tool.survey){
  # 1) convert all columns to character
  data <- mutate_all(data, as.character)
  # 2) remove columns that are naturally different in each survey
  cols <- data.frame(column=colnames(data)) %>% 
    left_join(dplyr::select(tool.survey, name, type), by=c("column"="name")) %>% 
    filter(!(type %in% c("date", "start", "end",  
                         "audit", "note", "calculate", "geopoint")) &
             !str_starts(column, "_") &
             !str_detect(column, "/"))
  data <- data[, all_of(cols$column)]
  # 3) convert NA to "NA" and all columns to factor
  data[is.na(data)] <- "NA"
  data <- data %>% mutate_if(is.character, factor)
  # 4) calculate gower distance
  gower_dist <- daisy(data, metric="gower", warnBin=F, warnAsym=F, warnConst=F)
  gower_mat <- as.matrix(gower_dist)
  # 5) convert distance to number of differences and determine closest matching survey
  r <- c()
  for (i in 1:nrow(data)) r <- c(r, sort(gower_mat[i,]*ncol(data))[2])
  # 6) store results
  data[["n.col.not.NA"]] <- rowSums(data!="NA")
  data[["survey.id"]] <- 1:dim(data)[1]
  data[["most.similar.survey"]] <- names(r)
  data[["number.different.columns"]] <- as.numeric(r)
  data <- data %>% arrange(number.different.columns, survey.id)
  return(data)
}

# silhouette analysis based on gower distance between surveys
# METHOD: check for anomalies using the silhouette function. We assume the dataset is clustered using the 
# enumerator IDs as the cluster IDs and we calculate the silhouette for this clustering scenario. A 
# silhouette value close to 1 indicates that the entries of the cluster are very similar to each other and 
# very dissimilar from entries of other clusters. Thus, we need to raise a flag if the silhouette value gets 
# close to 1 for any of the clusters/enumerators.
# https://en.wikipedia.org/wiki/Silhouette_(clustering)
# https://dpmartin42.github.io/posts/r/cluster-mixed-types
# https://medium.com/@rumman1988/clustering-categorical-and-numerical-datatype-using-gower-distance-ab89b3aa90d9
calculateEnumeratorSimilarity <- function(data, tool.survey, col_enum, col_admin="adm"){
  
  # helper function
  convertColTypes <- function(data, tool.survey){
    # select_multiple: numeric or factor?
    col.types <- data.frame(column=colnames(data)) %>% 
      left_join(dplyr::select(tool.survey, name, type), by=c("column"="name")) %>% 
      mutate(type.edited = case_when(
        type %in% c("integer", "decimal", "calculate") ~ "numeric",
        str_starts(type, "select_") ~ "factor",
        str_detect(column, "/") ~ "factor",
        TRUE ~ "text"))
    
    cols <- col.types[col.types$type.edited=="numeric", "column"]
    data[,cols] <- lapply(data[,cols], as.numeric)
    cols <- col.types[col.types$type.edited=="text", "column"]
    data[,cols] <- lapply(data[,cols], as.character)
    cols <- col.types[col.types$type.edited=="factor", "column"]
    data[,cols] <- lapply(data[,cols], as.factor)
    
    return(data)
  }
  
  # convert columns using the tool
  data <- convertColTypes(data, tool.survey)
  # keep only relevant columns
  cols <- data.frame(column=colnames(data)) %>% 
    left_join(dplyr::select(tool.survey, name, type), by=c("column"="name")) %>% 
    filter(!(type %in% c("date", "start", "end", "today", 
                         "audit", "note", "calculate", "deviceid", "geopoint")) &
             !str_starts(column, "_"))
  # convert character columns to factor and add enum.id
  data <- data[, all_of(cols$column)] %>% 
    mutate_if(is.character, factor) %>% 
    arrange(!!sym(col_enum)) %>%
    mutate(enum.id=as.numeric(!!sym(col_enum)), .after=!!sym(col_enum))
  # add "SY" column in case col_admin is not specified
  if (col_admin=="adm") data <- mutate(data, adm="DRC", .before=cols$column[1])
  # calculate similarity (for enumerators who completed at least 5 surveys)
  res <- data %>% split(data[[col_admin]]) %>% 
    lapply(function(gov){
      df <- gov %>% 
        group_by(enum.id) %>% mutate(n=n()) %>% filter(n>=5) %>% ungroup() %>% 
        select_if(function(x) any(!is.na(x)))
      if (length(unique(df$enum.id)) > 1){
        # calculate gower distance
        gower_dist <- daisy(dplyr::select(df, -c(!!sym(col_enum), enum.id)), 
                            metric = "gower", warnBin = F, warnAsym = F, warnConst = F)
        # gower_mat <- as.matrix(gower_dist)
        # calculate silhouette
        si <- silhouette(df$enum.id, gower_dist)
        res.si <- summary(si)
        # create output
        r <- data.frame(enum.id=as.numeric(names(res.si$clus.avg.widths)), si=res.si$clus.avg.widths) %>% 
          left_join(distinct(dplyr::select(df, !!sym(col_admin), !!sym(col_enum), enum.id)), by="enum.id") %>% 
          left_join(group_by(df, enum.id) %>% summarise(num.surveys=n(), .groups="drop_last"), by="enum.id") %>% 
          dplyr::select(!!sym(col_admin), !!sym(col_enum), num.surveys, si) %>% arrange(-si)
        return(r)}})
  return(do.call(rbind, res))
}

################################### Audit ######################################
load_audit<-function(data,
                     path.to.zip,path.to.unzip,
                     copy.zip=TRUE,
                     path.to.copy.zip,
                     filter.column="consent",
                     filter.on= "yes",
                     uuid.column="X_uuid",
                     delete.unzipped=TRUE,
                     days.ago.reported=0){
  if(copy.zip==TRUE){
    file.copy(path.to.zip, path.to.copy.zip)}
  
  unzip(path.to.zip, exdir = path.to.unzip)
  all_uuid_df<-data.frame(all_uuids=basename(dirname(list.files(path_unzip, recursive=TRUE))),
                          all_paths=dirname(list.files(path_unzip, recursive=TRUE, full.names = TRUE)))
  data$filter.col<- data[[filter.column]]
  filtered_uuid_df<- all_uuid_df[all_uuid_df$all_uuids %in% data[data$filter.col==filter.on,uuid.column],]
  filtered_audit_dirs<-filtered_uuid_df[,"all_paths"] %>% as.character()
  filtered_audit_csvs<-list.files(filtered_audit_dirs, recursive = TRUE, full.names=TRUE)
  data<-filtered_audit_csvs %>%
    purrr::map(readr::read_csv)
  names(data)<-filtered_uuid_df$all_uuids
  if(delete.unzipped==TRUE){
    delete_dir<-list.files(path_unzip,full.names = TRUE)
    unlink(delete_dir, recursive=TRUE)
  }
  return(data)
  
}


mutate_nearest_feature<- function(x, y){
  y_index <- st_nearest_feature(x= x, y= y)
  distance <- st_distance(x= x, y= y[y_index,], by_element=T)
  x %>%
    cbind(y_index= y_index, distance= distance)
}





load_audit<-function(data,
                     path.to.zip,path.to.unzip,
                     copy.zip=TRUE,
                     path.to.copy.zip,
                     filter.column="informed_consent",
                     filter.on= "yes",
                     uuid.column="X_uuid",
                     delete.unzipped=TRUE,
                     days.ago.reported=0){
  if(copy.zip==TRUE){
    file.copy(path.to.zip, path.to.copy.zip)}
  
  unzip(path.to.zip, exdir = path.to.unzip)
  all_uuid_df<-data.frame(all_uuids=basename(dirname(list.files(path_unzip, recursive=TRUE))),
                          all_paths=dirname(list.files(path_unzip, recursive=TRUE, full.names = TRUE)))
  data$filter.col<- data[[filter.column]]
  filtered_uuid_df<- all_uuid_df[all_uuid_df$all_uuids %in% data[data$filter.col==filter.on,uuid.column],]
  filtered_audit_dirs<-filtered_uuid_df[,"all_paths"] %>% as.character()
  filtered_audit_csvs<-list.files(filtered_audit_dirs, recursive = TRUE, full.names=TRUE)
  data<-filtered_audit_csvs %>%
    purrr::map(read.csv)
  names(data)<-filtered_uuid_df$all_uuids
  if(delete.unzipped==TRUE){
    delete_dir<-list.files(path_unzip,full.names = TRUE)
    unlink(delete_dir, recursive=TRUE)
  }
  return(data)
  
}






quick_survey2<-function(df,
                       consent_column_name,
                       audit_node,
                       uuid_col_name = "X_uuid",
                       start_question_node,
                       end_question_node,
                       min_allowable_duration,
                       audit_yes){
  
  
  
  retur_list <- list()
  dfl<-list()
  for (i in 1: length(audit_yes)){
    print(i)
    d<-audit_yes[[i]]

    d$node<-gsub("\\[1]","",d$node)
    d$node<-gsub("\\[2]","",d$node)
    d$node<-gsub("\\[3]","",d$node)
    
    start_question <- d %>% filter(node==paste0(audit_node,start_question_node)) %>%
      select(end)
    start_question<-min(start_question$end)
    end_question<-d %>% filter(node==paste0(audit_node,end_question_node)& !is.na(node)) %>%
      select(end)
    
    end_question<-max(end_question$end)
    duration_ms<-end_question-start_question
    duration_secs<-duration_ms/1000
    duration_minutes<- round(duration_secs/60,1)
    dfl[[i]]<-data.frame(uuid=names(audit_yes)[i],duration_ms=duration_ms,durations_secs=duration_secs,duration_minutes= duration_minutes)
  }
  
  duration_df2<-do.call("rbind", dfl)
  surveys_with_duration<- duration_df2
  quick_survey_df <- duration_df2 %>% dplyr::filter(duration_minutes < min_allowable_duration)
  quick_survey_df <- quick_survey_df %>% dplyr::select(-c("duration_ms","durations_secs"))
  
  retur_list[["surveys_with_duration"]] <- surveys_with_duration
  retur_list[["quick_survey_df"]] <- quick_survey_df
  list2env(retur_list,.GlobalEnv)
}
