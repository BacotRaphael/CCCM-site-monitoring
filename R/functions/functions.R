### functions

add_p_value_to_summary_table<-function(result){
  p.value <-  result$hypothesis.test$result$p.value
  result$summary.statistic$p.value <- p.value
  if(is.null(p.value)){
    result$summary.statistic$p.value <- NA
  }
  result
}


ggtitle_w_pvalue <-function(result){
  
  title_text<-paste(result$parameters$dependent.var,": ", round(result$hypothesis.test$result$p.value,3))
  ggtitle(title_text)
}



resultlist_summary_statistics_as_one_table<-function(results){
  results %>% 
    lapply(function(x){x$summary.statistic}) %>% do.call(rbind,.)
  
  
}


result_format_numbers <- function (result){ 
  
  if(is.null(result)){return(NULL)}
  if (!is.list(result)) {
    stop("result must be a list")
  }
  if(is.null(result$summary.statistic)){
    return(result)
  }
  
  if(!is.data.frame(result$summary.statistic)){
    stop("result$summary.statistic is not a data frame.. something went horribly wrong earlier!!")
  }
  
  # if(!is.numeric(result$summary.statistic$numbers)){stop("resultl summary statistic numbers are not numeric")}
  result$summary.statistic$numbers <- as.numeric(result$summary.statistic$numbers)
  result$summary.statistic$min <- as.numeric(result$summary.statistic$min)
  result$summary.statistic$max <- as.numeric(result$summary.statistic$max)
  
  
  is_percent <- grepl("\\_categorical\\_", result$parameters$case)
  has_confints <- !all(is.na(c(result$summary.statistic$min, 
                               result$summary.statistic$max)))
  has_dependent_var_values <- !all(is.na(result$summary.statistic$dependent.var.value))
  has_independent_var_values <- !all(is.na(result$summary.statistic$independent.var.value))
  nums_formated <- round(as.numeric(result$summary.statistic$numbers), 
                         2)
  if (is_percent) {
    nums_formated <- paste0(round(nums_formated * 100), "%")
  }
  if (!is_percent) {
    nums_formated <- as.character(round(nums_formated, 2))
  }
  if (has_confints) {
    if (is_percent) {
      min_formated <- paste0(round(result$summary.statistic$min * 
                                     100), "%")
      max_formated <- paste0(round(result$summary.statistic$max * 
                                     100), "%")
    }
    else {
      min_formated <- paste0(round(result$summary.statistic$min, 
                                   2), "")
      max_formated <- paste0(round(result$summary.statistic$max), 
                             "")
    }
    interval <- paste0("(", min_formated, "-", 
                       max_formated, ")")
  }
  else {
    interval <- ""
  }
  formated_nums <- paste(nums_formated, interval)
  formated_nums[formated_nums == "0 (0-0)"] <- "0"
  formated_nums[formated_nums == "0% (0%-0%)"] <- "0%"
  result$summary.statistic$numbers <- formated_nums
  
  return(result)
}
