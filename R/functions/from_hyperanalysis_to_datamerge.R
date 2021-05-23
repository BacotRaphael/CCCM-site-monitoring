from_hyperanalysis_to_datamerge <- function(analysis_output){
  
  ### delete useles columns
  analysis_output$X <- NULL
  analysis_output$se <- NULL
  analysis_output$min <- NULL
  analysis_output$max <- NULL
  analysis_output$repeat.var <- NULL
  analysis_output$repeat.var.value <- NULL
  analysis_output$independent.var <- NULL
  
  ### Paste question + options variable to ease the melt
  analysis_output$var <- paste0(analysis_output$dependent.var, ".", analysis_output$dependent.var.value)
  
  ### Delete single vars after the paste
  analysis_output$dependent.var <- NULL
  analysis_output$dependent.var.value <- NULL
  
  ### Melt and dcast!
  x <- reshape2::melt(analysis_output, c("independent.var.value", "var"), variable_name = "x", value.name = "y" )
  x$variable <- NULL
  
  data_merge <- reshape2::dcast(x, formula = independent.var.value~var)
  data_merge
  
}


