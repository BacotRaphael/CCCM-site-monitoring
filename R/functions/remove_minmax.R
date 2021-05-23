remove_minmax <- function(x){
  
  x$summary.statistic[["min"]] <- NA
  x$summary.statistic[["max"]] <- NA
  x
}