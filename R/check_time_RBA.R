check.time <- function(data, duration_threshold_lower=15, duration_threshold_upper=100){
  ## Input sanitation
  if(!is.data.frame(data))stop("data needs to be a dataframe!")
  if(!("start" %in% names(data)))stop("data needs to have a column called 'start' for this function work")
  if(!("end" %in% names(data)))stop("data needs to have a column called 'end' for this function work")
  ## pick apart the date and time parts of the start and end columns and identify those survey completed on different days
  df_col_separated <- data %>% separate(start, c("start_date", "start_time"), "T") %>%
    separate(end, c("end_date", "end_time"), "T")
  df_col_separated$days_diff <- difftime(as.POSIXct(df_col_separated$end_date), as.POSIXct(df_col_separated$start_date), units="days")
  same_date <- df_col_separated[((as.numeric(df_col_separated$days_diff))==0),]
  different_date <- df_col_separated[((as.numeric(df_col_separated$days_diff))>0),]
  
  different_date$start_time <- round(as.difftime(different_date$start_time, units = "mins"),2)
  different_date$end_time <- round(as.difftime(different_date$end_time, units = "mins"),2)
  different_date$duration_min <- as.numeric(different_date$days_diff)*24*60 + as.numeric(different_date$end_time) + 1440-as.numeric(different_date$start_time)
  
  #Making time into numeric so that calculations can be done
  same_date$start_time <- round(as.difftime(same_date$start_time, units = "mins"),2)
  same_date$end_time <- round(as.difftime(same_date$end_time, units = "mins"),2)
  same_date$duration_min <- as.numeric(same_date$end_time-same_date$start_time)
  same_date$time_short_flag <- ifelse(same_date$duration_min<=duration_threshold_lower,1,0)
  same_date$time_long_flag <- ifelse(same_date$duration_min>=duration_threshold_upper,1,0)
  
  ## Add Issue column
  same_date <- mutate(same_date, issue = ifelse((time_short_flag >= 1), "form duration too short",
                                                     ifelse((time_long_flag >= 1), "form duration too long", ""))) # Update original code to put issue_type too long
  different_date <- different_date %>%
    mutate(issue = "The survey length is greater than one day. Potentially check if there were problems/multiple updates in the form needed.")
  ## seperate the filtering into those surveys that are too long and those that are too short
  too_short <- same_date %>% dplyr::filter(time_short_flag == 1)
  #too_short$issue_type <- "form duration too short"
  too_long <- same_date %>% dplyr::filter(time_long_flag == 1)
  too_long <- bind_rows(too_long, different_date)
  #too_long$issue_type <- "form duration too long"

  
  ## bind and reformat the two culprits !!!
  time_problems <- rbind(too_short, too_long)
  
  if(nrow(time_problems)>=1){
    colnames(time_problems)[grep("uuid", colnames(time_problems))] <- "uuid"
    time_problems$variable <- "Completion Duration (min)"
    time_problems$has_issue <- "TRUE"
    # time_problems$issue_type <- "form duration too short"                     # Deleted as it would put too short for both too long and short surveys.
  } else {
    time_problems$variable <- as.character()
    time_problems$has_issue <- as.character()
  }
  time_grab <- time_problems %>%
    mutate(agency = q0_3_organization, area = a4_site_name, old_value = duration_min, new_value = "", 
           fix="Checked with partner", checked_by="ON") %>%
    dplyr::select(uuid, agency, area, variable,	issue, old_value, new_value, fix, checked_by)
  return(time_grab)
}
