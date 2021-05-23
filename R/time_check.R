time_check <- function (data, duration_threshold_lower = 15, duration_threshold_upper = 100) 
{
  if (!is.data.frame(data)) 
    stop("data needs to be a dataframe!")
  if (!("start" %in% names(data))) 
    stop("data needs to have a column called 'start' for this function work")
  if (!("end" %in% names(data))) 
    stop("data needs to have a column called 'end' for this function work")
  df_col_separated <- data %>% separate(start, c("start_date", 
                                                 "start_time"), "T") %>% separate(end, c("end_date", 
                                                                                         "end_time"), "T")
  df_col_separated$days_diff <- difftime(as.POSIXct(df_col_separated$end_date), 
                                         as.POSIXct(df_col_separated$start_date), units = "days")
  same_date <- df_col_separated[((as.numeric(df_col_separated$days_diff)) == 
                                   0), ]
  different_date <- df_col_separated[((as.numeric(df_col_separated$days_diff)) > 
                                        0), ]
  same_date$start_time <- round(as.difftime(same_date$start_time, 
                                            units = "mins"), 2)
  same_date$end_time <- round(as.difftime(same_date$end_time, 
                                          units = "mins"), 2)
  same_date$duration_min <- as.numeric(same_date$end_time - 
                                         same_date$start_time)
  same_date$time_short_flag <- ifelse(same_date$duration_min <= 
                                        duration_threshold_lower, 1, 0)
  same_date$time_long_flag <- ifelse(same_date$duration_min >= 
                                       duration_threshold_upper, 1, 0)
  too_short <- same_date %>% dplyr::filter(time_short_flag == 
                                             1)
  too_short$issue_type <- "form duration too short"
  too_long <- same_date %>% dplyr::filter(time_long_flag == 
                                            1)
  too_long$issue_type <- "form duration too long"
  time_problems <- rbind(too_short, too_long)
  colnames(time_problems)[grep("uuid", colnames(time_problems))] <- "uuid"
  time_problems$variable <- "Completion Duration (min)"
  time_problems$has_issue <- "TRUE"
  time_grab <- time_problems %>% dplyr::select(uuid, duration_min, 
                                               variable, has_issue, issue_type)
  names(time_grab) <- c("index", "value", "variable", 
                        "has_issue", "issue_type")
  return(time_grab)
}




