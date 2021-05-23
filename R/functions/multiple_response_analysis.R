

aggreagate_multiple_response <- function(data, question.prefix) {
  
  
  
  # Find the columns with the questions
  a <- grep(c(disagg,question.prefix), names(data))%>%
       group_by(.$disagg)
  # Find the total number of responses
  b <- sum(data[, a] != 0)
  # Find the totals for each question
  d <- colSums(data[, a] != 0)
  # Find the number of respondents
  e <- sum(rowSums(data[,a]) !=0)
  # d + b as a vector. This is the overfall frequency 
  f <- as.numeric(c(d, b))
  result <- data.frame(question = c(names(d), "Total"),
                       freq = f,
                       percent = (f/b)*100,
                       percentofcases = (f/e)*100)
  result


}



