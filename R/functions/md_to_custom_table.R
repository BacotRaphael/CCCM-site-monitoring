my_custom_md_table <- function (result) {
  
  md_out <- ""
  
  names <- list(name = c("a1_1", "a1_2", "a1_3", "a1_4", "a1_5"),
                label = c("Non displaced", "IDPS", "Returnees", "Refugees", "Migrants"))
  
  
  if (!is.null(result$summary.statistic)) {
    if (!is.null(result$parameters$independent.var)) {
    
      hypegrammaR:::md_add_lines(md_out) <- (paste("by", names$label[match(result$summary.statistic$repeat.var.value[1], names$name)]))
    
    }
    table <- result %>% (hypegrammaR:::map_to_table)
    hypegrammaR:::md_add_lines(md_out) <- knitr::kable(table, format = "html") %>% 
      kable_styling()
  }
  if (!is.null(result$hypothesis.test$result$p.value)) {
    hypegrammaR:::md_add_lines(md_out) <- c(as.character(unique(result$hypothesis.test$name)), 
                              "P Value:")
    result$hypothesis.test$result$p.value %>% print
  }
  hypegrammaR:::md_add_lines(md_out) <- knitr::kable(result$hypothesis.test %>% 
                                         as.data.frame, format = "html")
  md_out
}




