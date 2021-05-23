cleanHead <- function(data) {
  
  colnames(data) <- stringr::str_replace_all(colnames(data), pattern = "^.*?\\.", replacement = "")
  data
  
}
