

#### DEPRECIATED::
# function moved into hypegrammaR (internal)
# keeping here for now to not break things but should migrate fully eventually

  
#' subset a list of results based on analysis parameters
#' @param results list of results (output from `from_analysisplan_map_to_output()`)
#' @param repeat.vars optional: vector of character strings: keeps only results where repeat.var in this list
#' @param repeat.var.values optional: vector of character strings: keeps only results where repeat.var.vaues in this list
#' @param dependent.vars optional: vector of character strings: keeps only results where dependent.var in this list
#' @param logical optional: subset by a logical vector (same length as list of results)
#' @details if multiple parameters are given to subset by, only those are kept where all conditions apply
#' @return a resultlist in same format as from_analysisplan_map_to_output() only including those results with matching analysis parameters
results_subset<-function(results, repeat.vars = NULL,repeat.var.values = NULL,dependent.vars = NULL,logical = NULL){
  keep<-rep(TRUE,nrow(results$analysisplan))
  if(!is.null(logical)){
    keep[!logical]<-FALSE
  }
  if(!is.null(repeat.vars)){
    assertthat::assert_that(is.vector(repeat.vars))
    assertthat::assert_that(is.character(repeat.vars))
    keep[!(results$analysisplan$repeat.var %in% repeat.vars)]<-FALSE
  }
  if(!is.null(repeat.var.values)){
    assertthat::assert_that(is.vector(repeat.var.values))
    assertthat::assert_that(is.character(repeat.var.values))
    keep[!(results$analysisplan$repeat.var.value %in% repeat.var.values)]<-FALSE
  }
  if(!is.null(dependent.vars)){
    assertthat::assert_that(is.vector(dependent.vars))
    assertthat::assert_that(is.character(dependent.vars))
    keep[!(results$analysisplan$dependent.var %in% dependent.vars)]<-FALSE
  }
  small_results<-list()
  small_results$results<-purrr::keep(results$results,keep)
  small_results$analysisplan<-results$analysisplan[keep,]  
  small_results
}



results_rbind_summary_statistic<-function(results){
  
results$results %>% lapply(function(x){x$summary.statistic}) %>% do.call(rbind , .)

}











map_to_datamerge_keep_confints <- function (results, rows = c("repeat.var", "repeat.var.value"), 
          values = c("numbers","min","max"), ignore = c("se"), questionnaire = NULL) 
{
  all_summary_statistics <- results %>% lapply(function(x) {
    x$summary.statistic %>% lapply(function(x) {
      if (is.factor(x)) {
        return(as.character(x))
      }
      x
    }) %>% as.data.frame(stringsAsFactors = F)
  }) %>% do.call(rbind, .)
  if (!is.null(questionnaire)) {
    all_summary_statistics_labeled <- results %>% lapply(map_to_labeled(questionnaire)) %>% 
      lapply(function(x) {
        x$summary.statistic
      }) %>% do.call(rbind, .)
  }
  else {
    all_summary_statistics_labeled <- all_summary_statistics
  }
  if (nrow(all_summary_statistics) < nrow(all_summary_statistics_labeled)) {
    warning("labelising made some analysis definition indistinguishable (identical question labels or same label for different choices in the same question?")
    .write_to_log("mapping resultlist to datamerge csv could not be done correctly with labels - some analysis definitions became indistinguishable ")
  }
  columns <- names(all_summary_statistics)[!(names(all_summary_statistics) %in% 
                                               c(rows, ignore, values))]
  all_summary_statistics_labeled$master_table_column_name <- 
    all_summary_statistics[, columns] %>% as.list %>% c(sep = ":::") %>% do.call(paste, .)
  
  wide_format <- all_summary_statistics_labeled %>% unique %>% 
    .[, c(rows, "master_table_column_name", values)] %>% 
    spread(key = master_table_column_name, value = values[1])
  
  wide_format <- wide_format %>% unique %>% 
    # .[, c(rows, "master_table_column_name", values)] %>% 
    spread()
  
  
  return(wide_format)
}

