# Cleaning functions

## Numerical outlier 
detect.outliers <- function(df, method="sd-linear", n.sd=3, n.iqr=3){
  res <- data.frame()
  for (col in colnames(df)[colnames(df)!="uuid"]){
    df.temp <- data.frame(uuid=df$uuid, value=as.numeric(df[[col]])) %>% filter(!is.na(value) & value>0)
    if (method=="sd-linear"){
      df.temp <- df.temp %>%
        mutate(is.outlier=ifelse(value > mean(value, na.rm=T) + n.sd*sd(value, na.rm=T) | 
                                   value < mean(value, na.rm=T) - n.sd*sd(value, na.rm=T), T, F))
      } else if (method=="iqr-linear") {
      df.temp <- df.temp %>%
        mutate(col=value,
               is.outlier=ifelse(col > quantile(col, 0.75) + n.iqr*IQR(col) |
                                   col < quantile(col, 0.25) - n.iqr*IQR(col), T, F))
      } else if (method=="sd-log"){
      df.temp <- df.temp %>%
        mutate(col.log=log(value),
               is.outlier=ifelse(col.log > mean(col.log, na.rm=T) + n.sd*sd(col.log, na.rm=T) | 
                                   col.log < mean(col.log, na.rm=T) - n.sd*sd(col.log, na.rm=T), T, F))
      } else if (method=="iqr-log") {
        df.temp <- df.temp %>%
          mutate(col.log=log(value),
                 is.outlier=ifelse(col.log > quantile(col.log, 0.75) + n.iqr*IQR(col.log) |
                                     col.log < quantile(col.log, 0.25) - n.iqr*IQR(col.log), T, F))
        } else stop("Method unknown")
    df.temp <- filter(df.temp, is.outlier) %>% 
      mutate(variable=col, old_value=value) %>%
      select(uuid, variable, old_value)
    res <- rbind(res, df.temp)
  }
  return(res)
}

## Priority needs check
priority.check <- function(df, var){
  col.priority.needs <- c("i1_top_priority_need", "i2_second_priority_need", "i3_third_priority_need")
  service.level.var <- response %>% select(c("shelter_maintenance_services":"waste_disposal_services")) %>% colnames
  priority.need <- c("shelter_maintenance_assistance", "non_food_items", "food", "cash_assistance", 
                     "water", "medical_assistance", "education", "livelihood_assistance",
                     "protection_services", "nutrition_services", "sanitation_services")
  logical.inconsistencies <- data.frame(service.level.var, priority.need) %>%
    mutate(issue=paste0(service.level.var, " reported as adequate but ", priority.need, " cited as top three priority need. To be checked."))
  df <- df %>% 
    mutate(flag = ifelse((!!sym(var) == "adequate") &
                           ((i1_top_priority_need==logical.inconsistencies[logical.inconsistencies$service.level.var==var, "priority.need"]) |
                           (i2_second_priority_need==logical.inconsistencies[logical.inconsistencies$service.level.var==var, "priority.need"]) |
                           (i3_third_priority_need==logical.inconsistencies[logical.inconsistencies$service.level.var==var, "priority.need"])), T, F),
           issue = ifelse(flag, logical.inconsistencies[logical.inconsistencies$service.level.var==var, "issue"], "")) %>%
    dplyr::select("uuid", "q0_3_organization", "a4_site_name", any_of(c(var, col.priority.needs)), flag, issue) %>% filter(flag) %>%
    dplyr::rename(agency=q0_3_organization, area=a4_site_name)  
  return(df)
}

priority.need.checks <- function(df){
  for (var in service.level.var){
    check_var <- priority.check(df, var)
    add.to.cleaning.log(check_var, check_id = paste0("check_priority_", var), question.names=c(var, col.priority.needs), issue = "issue")
  }
  adequacy_log <<- cleaning.log %>% filter(grepl("check_priority", check_id))
}

clean.gps <- function(df, x, y){
  df.temp <- df %>%
    mutate(flag_lon = ifelse(grepl(",|-|\\/|\\\\|،", !!sym(x)), T, F),
           issue_lon = ifelse(flag_lon, "Longitude include invalid characters", ""),
           long_clean = unlist(lapply(ifelse(flag_lon, gsub("،", "\\.", gsub(",|-|\\/|\\\\", "", !!sym(x))), !!sym(x)), arabic.tonumber)),
           
           flag_lat = ifelse(grepl(",|-|\\/|\\\\|،", !!sym(y)), T, F),
           issue_lat = ifelse(flag_lat, "Latitude include invalid characters", ""),
           lat_clean = unlist(lapply(ifelse(flag_lat, gsub("،", "\\.", gsub(",|-|\\/|\\\\", "", !!sym(y))), !!sym(y)), arabic.tonumber)),
           
           issue_lon = case_when(
             is.na(long_clean) ~ paste0(issue_lon, "; No coordinates entered: follow-up"),
             (long_clean %>% as.numeric == 0) | (long_clean %>% as.numeric %>% abs > 180) ~ paste0(issue_lon, "; Invalid Longitude coordinate"),
             near(long_clean %>% as.numeric, lat_clean %>% as.numeric , tol = 0.2) ~ paste0(issue_lon, "; Longitude and Latitude almost equal"),
             (long_clean %>% as.numeric < 20) & (lat_clean %>% as.numeric %>% abs < 90) &
               (!near(long_clean %>% as.numeric, lat_clean %>% as.numeric , tol = 0.2)) ~ paste0(issue_lon, "; Latitude entered instead of Longitude"),
             grepl("E|°|\"|LON", long_clean) & 
               !is.na(parzer::parse_lon(gsub("E|\"|LON", "", long_clean))) ~ paste0(issue_lon, "; Longitude invalid format: mix beteen DD and DMS"),
             is.na(gsub("E|°|\"|LON|LAT", "", long_clean) %>% as.numeric)  ~ paste0(issue_lon, "; Longitude format not recognized"),
             TRUE ~ issue_lon),
           
           Longitude_clean = case_when(
             is.na(long_clean) ~ NA_real_,
             grepl("Invalid Longitude coordinate", issue_lon) ~ NA_real_,
             grepl("Latitude entered instead of Longitude", issue_lon) ~ lat_clean %>% as.numeric,
             grepl("Longitude invalid format: mix beteen DD and DMS", issue_lon) ~ parzer::parse_lon(gsub("E|\"|LON|LAT", "", long_clean)) %>% as.numeric,
             grepl("Longitude format not recognized", issue_lon) ~ NA_real_,
             TRUE ~ long_clean %>% as.numeric),
           
           issue_lat = case_when(
             is.na(lat_clean) ~ paste0(issue_lat, "; No coordinates entered: follow-up"),
             (lat_clean %>% as.numeric == 0) | (lat_clean %>% as.numeric %>% abs > 90) ~ paste0(issue_lat, "; Invalid Latitude coordinate"),
             near(long_clean %>% as.numeric, lat_clean %>% as.numeric , tol = 0.2) ~ paste0(issue_lat, "; Longitude and Latitude almost equal"),
             (lat_clean %>% as.numeric > 40) & (long_clean %>% as.numeric %>% abs < 180) &
               (!near(lat_clean %>% as.numeric, long_clean %>% as.numeric , tol = 0.2)) ~ paste0(issue_lat, "; Longitude entered instead of Latitude"),
             grepl("N|°|\"|LAT", lat_clean) & 
               !is.na(parzer::parse_lat(gsub("LON|N|\"|LAT", "", lat_clean))) ~ paste0(issue_lat, "; Latitude invalid format: mix beteen DD and DMS"),
             is.na(gsub("N|°|\"|LAT", "", lat_clean) %>% as.numeric)  ~ paste0(issue_lat, "; Latitude format not recognized"),
             TRUE ~ issue_lat),
           
           Latitude_clean = case_when(
             is.na(lat_clean) ~ NA_real_,
             grepl("Invalid Latitude coordinate", issue_lat) ~ NA_real_,
             grepl("Longitude entered instead of Latitude", issue_lat) ~ long_clean %>% as.numeric,
             grepl("Latitude invalid format: mix beteen DD and DMS", issue_lat) ~ parzer::parse_lat(gsub("LON|N|\"|LAT", "", lat_clean)) %>% as.numeric,
             grepl("Latitude format not recognized", issue_lat) ~ NA_real_,
             TRUE ~ lat_clean %>% as.numeric),
           issue.gps = ifelse((issue_lon!="") | (issue_lat!=""), paste0(issue_lon,"; ", issue_lat), ""),
           issue.gps = gsub("^; ; |^; |; $| ;","", issue.gps)
          )
  return(df.temp)
}

partial_join <- function(x, y, pattern_x, by_y){
  z <- x[[pattern_x]] %>% str_split(" |,")                                      # splits the sitename with space as separator to see if any part of the sitename has a corresponding match
  z <- lapply(z, function(x) paste(x, collapse="|")) %>% unlist                 # collapse all substring together with "|" statements
  idy_y <- sapply(z, function(s) {                                              # Return the indice for any match from any of the substring in the sitename
    if (grepl("NA|\\(|\\)", s)) {s <- gsub("NA", "", s)}                            # If there is a NA, clean it
    if (s == "" | is.na(s)) {s} else {                                          # If there is a blank of NA, just return NA
      res <- grep(s, y[[by_y]])}})
  idx_x <- sapply(seq_along(idy_y), function(i) rep(i, length(idy_y[[i]])))     # calls the corresponding row numbers in x to join them to y (repeats the x match as many time as partial match in y)
  df <- dplyr::bind_cols(x[unlist(idx_x), , drop = F],                          # calls all partial matches rows of x, repeating the multiple matches if necessary
                         y[unlist(idy_y), , drop = F]) %>%                      # calls the partially matching rows of y => bind_cols creates the output dataframe
    select(all_of(pattern_x), all_of(by_y), everything())
  df <- plyr::rbind.fill(df, x[-unlist(idx_x), , drop = F])                     # add all other non matching lines 
  
  return(df)
}

# Cleaning log functions
initialise.cleaning.log <- function() {
  return(data.frame(uuid = as.character(), 
                    agency = as.character(), 
                    area = as.character(), 
                    variable = as.character(), 
                    issue = as.character(), 
                    old_value = as.character(),
                    new_value = as.character(), 
                    fix = as.character(), 
                    checked_by = as.character()))
}

add.to.cleaning.log <- function(checks, check_id, question.names=c(), issue="", new.value="" , fix="Checked with partner", checked_by="ON", add.col=c("")){
  df <- initialise.cleaning.log()
  if (nrow(checks)>0){
    for(q.n in question.names){
      new.entries <- checks %>% filter(flag) %>% 
        mutate(uuid=uuid %>% as.character,
               variable=q.n %>% as.character,
               issue=issue %>% as.character,
               check_id=check_id %>% as.character,
               old_value=!!sym(q.n) %>% as.character,
               new_value=new.value %>% as.character,
               fix=fix %>% as.character,
               checked_by= checked_by %>% as.character)
      new.entries <- new.entries %>% select(all_of(col.cl), any_of(add.col))
      df <- bind_rows(df, new.entries)
    }
    cleaning.log <<- bind_rows(cleaning.log, df %>% arrange(uuid, variable, agency))
  }
}

save.org.name.follow.up <- function(cl, filename.out="output/test.xlsx"){
  # save organisation name follow-up requests
  wb <- createWorkbook()
  addWorksheet(wb, "Follow-up")
  writeData(wb = wb, x = cl, sheet = "Follow-up", startRow = 1)
  
  style.col.color <- createStyle(fgFill="#E5FFCC", border="TopBottomLeftRight", borderColour="#000000")
  style.col.color.first <- createStyle(textDecoration="bold", fgFill="#E5FFCC",
                                       border="TopBottomLeftRight", borderColour="#000000", wrapText=F)
  
  addStyle(wb, "Follow-up", style = style.col.color, rows = 1:(nrow(cl)+1), cols=4)
  addStyle(wb, "Follow-up", style = style.col.color, rows = 1:(nrow(cl)+1), cols=5)
  col.style <- createStyle(textDecoration="bold", fgFill="#CECECE", halign="left",
                           border="TopBottomLeftRight", borderColour="#000000")
  
  setColWidths(wb, "Follow-up", cols=1, widths=35)
  setColWidths(wb, "Follow-up", cols=2, widths=8)
  setColWidths(wb, "Follow-up", cols=3, widths=8)
  setColWidths(wb, "Follow-up", cols=4, widths=30)
  setColWidths(wb, "Follow-up", cols=5, widths=20)
  setColWidths(wb, "Follow-up", cols=6:8, widths=20)
  setColWidths(wb, "Follow-up", cols=9, widths=5)
  setColWidths(wb, "Follow-up", cols=10, widths=8)
  setColWidths(wb, "Follow-up", cols=(11:12), widths=30)
  setColWidths(wb, "Follow-up", cols=13:19, widths=15)
  
  addStyle(wb, "Follow-up", style = createStyle(wrapText=T), rows = 1:(ncol(cl)+1), cols=6)
  addStyle(wb, "Follow-up", style = createStyle(wrapText=T), rows = 1:(ncol(cl)+1), cols=7)
  addStyle(wb, "Follow-up", style = col.style, rows = 1, cols=1:dim(cl)[2])
  addStyle(wb, "Follow-up", style = style.col.color.first, rows = 1, cols=4)
  addStyle(wb, "Follow-up", style = style.col.color.first, rows = 1, cols=5)
  
  col.id <- which(colnames(cl) %in% c("ngo_code_partial_match"))
  random.color <- ""
  for (r in 2:nrow(cl)){
    if(as.character(cl[r, "uuid"])==as.character(cl[r-1, "uuid"])){
      if (random.color == "") random.color <- randomColor(1, luminosity = "light")
      addStyle(wb, "Follow-up", style = createStyle(fgFill=random.color, wrapText=F), 
               rows = r:(r+1), cols=col.id)
    } else random.color=""
  }
  
  saveWorkbook(wb, filename.out, overwrite = TRUE)
} 

save.sitename.follow.up <- function(cl, filename.out="output/test.xlsx"){
  # save sitename follow-up requests
  wb <- createWorkbook()
  addWorksheet(wb, "Follow-up")
  writeData(wb = wb, x = cl, sheet = "Follow-up", startRow = 1)
  
  style.col.color <- createStyle(fgFill="#E5FFCC", border="TopBottomLeftRight", borderColour="#000000")
  style.col.color.first <- createStyle(textDecoration="bold", fgFill="#E5FFCC",
                                       border="TopBottomLeftRight", borderColour="#000000", wrapText=F)
  
  addStyle(wb, "Follow-up", style = style.col.color, rows = 1:(nrow(cl)+1), cols=4)
  addStyle(wb, "Follow-up", style = style.col.color, rows = 1:(nrow(cl)+1), cols=5)
  col.style <- createStyle(textDecoration="bold", fgFill="#CECECE", halign="left",
                           border="TopBottomLeftRight", borderColour="#000000")
  
  setColWidths(wb, "Follow-up", cols=1, widths=25)
  setColWidths(wb, "Follow-up", cols=2, widths=8)
  setColWidths(wb, "Follow-up", cols=3, widths=12.5)
  setColWidths(wb, "Follow-up", cols=4, widths=31)
  setColWidths(wb, "Follow-up", cols=5, widths=20)
  setColWidths(wb, "Follow-up", cols=6, widths=22)
  setColWidths(wb, "Follow-up", cols=7, widths=6.5)
  setColWidths(wb, "Follow-up", cols=8, widths=8.5)
  setColWidths(wb, "Follow-up", cols=9, widths=8.5)
  setColWidths(wb, "Follow-up", cols=c(10:11), widths=15)
  setColWidths(wb, "Follow-up", cols=(12:17), widths=15)
  setColWidths(wb, "Follow-up", cols=18:20, widths=25)
  
  # addStyle(wb, "Follow-up", style = createStyle(wrapText=T), rows = 1:(ncol(cl)+1), cols=8)
  # addStyle(wb, "Follow-up", style = createStyle(wrapText=T), rows = 1:(ncol(cl)+1), cols=9)
  addStyle(wb, "Follow-up", style = col.style, rows = 1, cols=1:dim(cl)[2])
  addStyle(wb, "Follow-up", style = style.col.color.first, rows = 1, cols=4)
  addStyle(wb, "Follow-up", style = style.col.color.first, rows = 1, cols=5)
  
  col.id <- which(colnames(cl) %in% c("Site_ID_partial_match"))
  random.color <- ""
  for (r in 2:nrow(cl)){
    if(as.character(cl[r, "uuid"])==as.character(cl[r-1, "uuid"])){
      if (random.color == "") random.color <- randomColor(1, luminosity = "light")
      addStyle(wb, "Follow-up", style = createStyle(fgFill=random.color, wrapText=F), 
               rows = r:(r+1), cols=col.id)
    } else random.color=""
  }
  
  saveWorkbook(wb, filename.out, overwrite = TRUE)
} 

data.validation.list <- function(){
  choicelist <- get.select.db() %>% dplyr::select(name, choices)                  # extract list of valid answer for all survey
  choice_validation <- choicelist %>% transpose %>% setNames(.[1,]) %>% slice(-1) %>% mutate_all(~str_split(.,";\n")) 
  nrow_validation <- lapply(choice_validation, function(x) length(x[[1]])) %>% unlist %>% max
  data.val <<- data.frame(matrix(NA, nrow = nrow_validation, ncol = 0))
  for (c in colnames(choice_validation)){
    data.val <<- data.val %>% mutate(!!sym(c) := c(unlist(choice_validation[[c]]), rep(NA, nrow_validation-length(choice_validation[[c]][[1]]))))
  }
}

get.col.range <- function(variable){
  column.number <- which(colnames(data.val)==variable)
  all <- expand.grid(LETTERS, LETTERS)
  all <- all[order(all$Var1,all$Var2),]
  alphabet <- c(LETTERS, do.call('paste0',all))
  col.excel <- alphabet[column.number]
  nrow <- nrow(data.val %>% filter(!is.na(!!sym(variable))))
  range.vect <- c("$", col.excel, "$2:$", col.excel, "$", (nrow + 1))             ## if nrow + 2 => will keep an additionnal field in drop down list to be updated if needed by partner
  range <- paste(range.vect, sep="", collapse="")
  value.sheet <- paste("'Choices validation'!")
  value <- paste(value.sheet, range, sep="", collapse="")
  return(value)
}

save.follow.up.requests <- function(cl, filename.out="output/test.xlsx"){
  # save follow-up requests
  wb <- createWorkbook()
  addWorksheet(wb, "Follow-up")
  addWorksheet(wb, "Choices validation")
  writeData(wb = wb, x = cl, sheet = "Follow-up", startRow = 1)
  
  data.validation.list()
  writeData(wb = wb, x = data.val, sheet = "Choices validation", startRow = 1)
  
  style.col.color <- createStyle(fgFill="#E5FFCC", border="TopBottomLeftRight", borderColour="#000000")
  style.col.color.first <- createStyle(textDecoration="bold", fgFill="#E5FFCC",
                                       border="TopBottomLeftRight", borderColour="#000000", wrapText=F)
  
  addStyle(wb, "Follow-up", style = style.col.color, rows = 1:(nrow(cl)+1), cols=4)
  addStyle(wb, "Follow-up", style = style.col.color, rows = 1:(nrow(cl)+1), cols=5)
  col.style <- createStyle(textDecoration="bold", fgFill="#CECECE", halign="center",
                           border="TopBottomLeftRight", borderColour="#000000")
  
  setColWidths(wb, "Follow-up", cols=1, widths=20)
  setColWidths(wb, "Follow-up", cols=2, widths=15)
  setColWidths(wb, "Follow-up", cols=3, widths=15)
  setColWidths(wb, "Follow-up", cols=c(4), widths="auto")
  setColWidths(wb, "Follow-up", cols=5, widths=60)
  setColWidths(wb, "Follow-up", cols=6, widths=15)
  setColWidths(wb, "Follow-up", cols=7, widths=15)
  setColWidths(wb, "Follow-up", cols=8, widths=8)
  setColWidths(wb, "Follow-up", cols=9, widths=20)
  setColWidths(wb, "Follow-up", cols=10, widths=15)
  
  addStyle(wb, "Follow-up", style = createStyle(wrapText=T), rows = 1:(ncol(cl)+1), cols=6)
  addStyle(wb, "Follow-up", style = createStyle(wrapText=T), rows = 1:(ncol(cl)+1), cols=7)
  addStyle(wb, "Follow-up", style = col.style, rows = 1, cols=1:dim(cl)[2])
  addStyle(wb, "Follow-up", style = style.col.color.first, rows = 1, cols=4)
  addStyle(wb, "Follow-up", style = style.col.color.first, rows = 1, cols=5)
  
  col.id <- which(colnames(cl)=="old_value")
  random.color <- ""
  if (nrow(cl)>1) {for (r in 2:nrow(cl)){
    if(as.character(cl[r, "uuid"])==as.character(cl[r-1, "uuid"]) & 
       as.character(cl[r, "check_id"])==as.character(cl[r-1, "check_id"])){
      if (random.color == "") random.color <- randomColor(1, luminosity = "light")
      addStyle(wb, "Follow-up", style = createStyle(fgFill=random.color, wrapText=T), 
               rows = r:(r+1), cols=col.id)
    } else random.color=""
  }}
  
  for (r in 1:nrow(cl)){
    if (cl[r,"variable"] %in% colnames(data.val)){
      dataValidation(wb, "Follow-up", cols = which(colnames(cl)=="new_value"),
                     rows = r+1, type = "list",
                     value = get.col.range(cl[r,"variable"]))
    }
  }
  saveWorkbook(wb, filename.out, overwrite = TRUE)
} 

save.new.sites <- function(cl, filename.out="output/test.xlsx") {
  wb <- createWorkbook()
  addWorksheet(wb, "New sites")
  writeData(wb = wb, x = cl, sheet = "New sites", startRow = 1)
  style.col.color <- createStyle(fgFill="#E5FFCC", border="TopBottomLeftRight", borderColour="#000000")
  style.col.color.first <- createStyle(textDecoration="bold", fgFill="steelblue1", border="TopBottomLeftRight", borderColour="#000000", wrapText=F)
  col.style <- createStyle(textDecoration="bold", fgFill="#CECECE", halign="center", border="TopBottomLeftRight", borderColour="#000000")
  
  addStyle(wb, "New sites", style = col.style, rows = 1, cols=1:ncol(cl))
  addStyle(wb, "New sites", style = style.col.color.first, rows = 2:(nrow(cl)+1), cols=9)
  addStyle(wb, "New sites", style = style.col.color, rows = 2:(nrow(cl)+1), cols=10)
  addStyle(wb, "New sites", style = style.col.color, rows = 2:(nrow(cl)+1), cols=11)
  
  setColWidths(wb, "New sites", cols=1, widths=20)
  setColWidths(wb, "New sites", cols=2, widths=7)
  setColWidths(wb, "New sites", cols=3, widths=7)
  setColWidths(wb, "New sites", cols=4, widths=55)
  setColWidths(wb, "New sites", cols=5, widths=20)
  setColWidths(wb, "New sites", cols=6, widths=20)
  setColWidths(wb, "New sites", cols=7, widths=5)
  setColWidths(wb, "New sites", cols=8, widths=8)
  setColWidths(wb, "New sites", cols=9, widths=15)
  setColWidths(wb, "New sites", cols=10:11, widths=20)
  setColWidths(wb, "New sites", cols=12:ncol(cl), widths=10)
  
  saveWorkbook(wb, filename.out, overwrite = TRUE)
  
}

get.ref.question <- function(x){
  x.1 <- str_split(x, "\\{")[[1]][2]
  return(str_split(x.1, "\\}")[[1]][1])
}
get.choice.list.name <- function(x){
  x.1 <- str_split(x, " ")[[1]]
  if (length(x.1)==1) return(NA)
  else return(x.1[2])
}
get.q.type <- function(x) return(str_split(x, " ")[[1]][1])

get.select.db <- function(){
  # list of choices for each list_name (from TOOL_CHOICES)
  list.choices <- choices_all %>% filter(!is.na(list_name)) %>% group_by(list_name) %>% 
    mutate(choices=paste(name, collapse=";\n"),
           choices.label=paste(`label::english`, collapse=";\n")) %>% 
    summarise(choices=choices[1], choices.label=choices.label[1])
  select.questions <- tool %>% select(type, name) %>% 
    mutate(q.type=as.character(lapply(type, get.q.type)),
           list_name=as.character(lapply(type, get.choice.list.name))) %>% 
    filter(list_name!="NA" & list_name!="group" & list_name!="repeat") %>% 
    left_join(list.choices, by="list_name") %>% 
    filter(!is.na(choices))
  return(select.questions)
}

get.other.db <- function(){
  select.questions <- get.select.db()
  # for each "other" question, get ref.question and list of choices
  other.choice.name <- choices %>% 
    filter(grepl("Other|other",`label::english`)|grepl("أخرى",`label::arabic`)) %>% pull(name)
  other.db <- tool %>% filter(grepl(paste(other.choice.name,collapse="|"),relevant)) %>% 
    select("type", "name", "label::english", "relevant") %>% 
    mutate(ref.question=as.character(lapply(relevant, get.ref.question))) %>% 
    left_join(select(select.questions, "name", "q.type", "list_name", "choices", "choices.label"),
              by=c("ref.question"="name")) %>% 
    left_join(select(var.labels, "name", "label.full"), by="name") %>% 
    select(name, ref.question, label.full, q.type, list_name, choices, choices.label)
  
  return(other.db)
}

get.dependencies <- function(){
  # determine dependencies of "other" questions 
  # (i.e. if there is a change in any of the "other" question, we need to follow-up on other questions)
  relevant.cleaned <- filter(tool.survey, !is.na(relevant) & !str_ends(relevant, "\\'other\\'\\)")) %>% 
    filter(str_starts(type, "select") | str_starts(type, "text") | str_starts(type, "integer"))
  dependencies <- other.db %>% 
    mutate(questions.other.affected=as.numeric(lapply(name, 
                                                      function(x){sum(str_detect(relevant.cleaned$relevant,
                                                                                 paste("\\{",x,"\\}", sep="")))}))) %>%
    mutate(questions.affected=as.numeric(lapply(ref.question, 
                                                function(x){sum(str_detect(relevant.cleaned$relevant, 
                                                                           paste("\\{",x,"\\}", sep="")))}))) %>%
    filter(questions.other.affected > 0 | questions.affected > 0) %>% 
    select(name, ref.question, questions.other.affected, questions.affected)
  return(dependencies)
}

# Finish the generate.custom label function
# Then go back to the functions that puts together all choices for the survey and make sure in the changes done in the cleaning log that they match an existing choice
# Then continue updating the loop that incorporate cleaning log changes for new cl format with conflicting values + gps location update
generate.custom.made.label <- function(filename="data/labels.csv"){
  var.labels <- read.csv(filename)
  var.labels <- var.labels %>%
    mutate(label.full=case_when(
      !grepl(substring(ref.variable, 1, 4), label) ~ paste(ref.variable, label, sep = " "),
      grepl(substring(ref.variable, 1, 4), label), ~ paste(ref.variable, sub(".*? ", "", label), sep = " "),
      TRUE ~ ""
    ))
  # var.labels[["label.full"]] <- ""
  # for (r in 1:dim(var.labels)[1]){
  #   v <- var.labels[r, "name"]
  #   v.l <- var.labels[var.labels$name==v,"label"]
  #   ref.v <- var.labels[r, "ref.variable"]
  #   ref.v.l <- ifelse(ref.v %in% var.labels$name, var.labels[var.labels$name==ref.v,"label"],
  #                     ifelse(ref.v=="", "", ref.v))
  #   ref.v.2 <- ifelse(ref.v %in% var.labels$name, var.labels[var.labels$name==ref.v,"ref.variable"], "")
  #   ref.v.2.l <- ifelse(ref.v.2 %in% var.labels$name, var.labels[var.labels$name==ref.v.2,"label"],
  #                       ifelse(ref.v.2=="", "", ref.v.2))
  #   label <- ""
  #   
  #   if (ref.v.2.l != "") label <- ifelse(label=="", ref.v.2.l, paste(label, ref.v.2.l, sep=" "))
  #   if (ref.v.l != "") label <- ifelse(label=="", ref.v.l, paste(label, ref.v.l, sep=" "))
  #   if (grepl(substring(var.labels[r, "ref.variable"],1,3), var.labels[r, "label"])) label <- paste(var.labels[r, "ref.variable"], sub(".*? ", "", var.labels[r, "label"]), sep=" ")
  #   var.labels[r, "label.full"] <- ifelse(label=="", v.l, label)
  # }
  # var.labels$label.full <- sub("^ - ", "", sub("^ - ", "", sub(":*", "", var.labels$label.full))) 
  return(var.labels)
}

get.label <- function(variable){
  return(var.labels[var.labels$name==variable, "label.full"])
}

choice.name2label <- function(list_name, name){
  return(as.character(choices_all[choices_all$list_name==list_name & 
                                    choices_all$name==name, "label::english"]))
}

choice.label2name <- function(list_name, label){
  return(as.character(choices_all[choices_all$list_name==list_name & 
                                    choices_all$`label::english`==label, "name"]))
}

get.old.value.label <- function(cl){
  for (r in 1:dim(cl)[1]){
    list_name <- as.character(cl[r, "list_name"])
    old_value <- as.character(cl[r, "old_value"])
    if (!is.na(list.name)){
        cl[r, "old_value"] <- choice.name2label(list_name, old_value)
    }
  }
  return(cl)
}

arabic.tonumber <- function(s){
  arabic <- c("\u0660\u0661\u0662\u0663\u0664\u0665\u0666\u0667\u0668\u0669\u06F0\u06F1\u06F2\u06F3\u06F4\u06F5\u06F6\u06F7\u06F8\u06F9")
  english <- c("01234567890123456789")
  suppressWarnings(res.int <- as.numeric(chartr(arabic,english,s)))
  if (is.na(res.int)){res <- s} else {res <- res.int}
  return(res)
}

is.Arabic<-function(utf8char) #returns TRUE if utf8char is within the Arabic Unicode ranges
{
  v<-utf8ToInt(utf8char)
  if (v %in% 0x0600: 0x06FF) return (TRUE)
  if (v %in% 0x0750: 0x077F) return (TRUE)
  if (v %in% 0x08A0: 0x08FF) return (TRUE)
  if (v %in% 0xFB50: 0xFDFF) return (TRUE)
  if (v %in% 0xFE70: 0xFEFF) return (TRUE)
  if (v %in% 0x1EE00:0x1EEFF) return (TRUE)
  FALSE
}

## Archived code
# 
# add.to.cleaning.log.old <- function(checks, question.names=c(), issue="", new.value="" , fix="Checked with partner", checked_by="ON", add.col=c("")){
#   for(q.n in question.names){
#     new.entries <- checks %>% filter(flag) %>% 
#       mutate(uuid=uuid,
#              variable=q.n,
#              issue=issue,
#              old_value=!!sym(q.n),
#              new_value=new.value,
#              fix=fix,
#              checked_by=checked_by)
#     new.entries <- new.entries %>% select(all_of(col.cl), any_of(add.col))
#     cleaning.log <<- bind_rows(cleaning.log, new.entries)
#   }
# }
# 
# cleaning.log.new.entries <- function(df, var, issue_type ="", new_value=" ", fix="Checked with partner", checked_by="ON") {
#   new.entries <- df %>% ungroup() %>%
#     mutate(uuid = uuid,
#            agency = q0_3_organization, 
#            area = a4_site_name3, 
#            variable = var, 
#            issue = issue_type, 
#            old_value = !!sym(var), 
#            new_value = new_value, 
#            fix = fix, 
#            checked_by = checked_by) %>% 
#     select(uuid, agency, area, variable, issue, old_value, new_value, fix, checked_by)
# }


# clean.gps.old <- function(df, x, y){
#   df.temp <- df %>%
#     mutate(issue_lon = case_when(
#       is.na(!!sym(x)) ~ "No coordinates entered: follow-up",
#       (!!sym(x) %>% as.numeric == 0) | (!!sym(x) %>% as.numeric %>% abs > 180) ~ "Invalid Longitude coordinate",
#       near(!!sym(x) %>% as.numeric, !!sym(y) %>% as.numeric , tol = 0.2) ~ "Longitude and Latitude almost equal",
#       (!!sym(x) %>% as.numeric < 20) & (!!sym(y) %>% as.numeric < 90) &
#         (!near(!!sym(x) %>% as.numeric, !!sym(y) %>% as.numeric , tol = 0.2)) ~ "Latitude entered instead of Longitude",
#       !!sym(x) %>% as.numeric > 80 ~ "Longitude not in Decimal Degree format",
#       grepl("E", !!sym(x)) ~ "Longitude invalid format: mix beteen DD and DMS",
#       grepl("°", !!sym(x)) & !is.na(gsub("°", "", !!sym(x)) %>% as.numeric)  ~ "Longitude invalid format: mix beteen DD and DMS",
#       grepl("°|'", !!sym(x)) ~ "Longitude not in DD but in DMS",
#       grepl(",|-|\\/|\\\\|،", !!sym(x)) ~ "Longitude include invalid characters",
#       TRUE ~ ""),
#       Longitude_clean = case_when(
#         issue_lon == "Invalid Longitude coordinate" ~ NA_real_,
#         issue_lon == "Latitude entered instead of Longitude" ~ !!sym(y) %>% as.numeric,
#         issue_lon == "Longitude not in Decimal Degree format" ~ NA_real_,
#         issue_lon == "Longitude invalid format: mix beteen DD and DMS" ~ gsub("E|?", "", gsub(" ", "", !!sym(x))) %>% as.numeric,
#         issue_lon == "Longitude not in DD but in DMS" ~ parzer::parse_lon(gsub("\"|E|LON", "", !!sym(x))) %>% as.numeric,
#         issue_lon == "Longitude include invalid characters" ~ gsub(",|-|\\/|\\\\| |،", "", !!sym(x)) %>% as.numeric,
#         TRUE ~ !!sym(x) %>% arabic.tonumber %>% as.numeric),
#       issue_lat = case_when(
#         is.na(!!sym(y)) ~ "No coordinates entered: follow-up",
#         (!!sym(y) %>% as.numeric == 0) | (!!sym(y) %>% as.numeric %>% abs > 90) ~ "Invalid Latitude coordinate",
#         near(!!sym(x) %>% as.numeric, !!sym(y) %>% as.numeric , tol = 0.2) ~ "Longitude and Latitude almost equal",
#         !!sym(y) %>% as.numeric > 80 ~ "Latitude not in Decimal Degree format",
#         (!!sym(y) %>% as.numeric > 20) & (!!sym(x) %>% as.numeric < 180) &
#           (!near(!!sym(x) %>% as.numeric, !!sym(y) %>% as.numeric , tol = 0.2)) ~ "Longitude entered instead of Latitude",
#         grepl("N", !!sym(y)) ~ "Latitude invalid format: mix beteen DD and DMS",
#         grepl("°", !!sym(y)) & !is.na(gsub("°", "", !!sym(x)) %>% as.numeric)  ~ "Latitude invalid format: mix beteen DD and DMS",
#         grepl("°|'", !!sym(y)) ~ "Latitude not in DD but in DMS",
#         grepl(",|-|\\/|\\\\| |،", !!sym(y)) ~ "Latitude include invalid characters",
#         TRUE ~ ""),
#       Latitude_clean = case_when(
#         issue_lat == "Invalid Latitude coordinate" ~ NA_real_,
#         issue_lat == "Latitude not in Decimal Degree format" ~ NA_real_,
#         issue_lat == "Longitude entered instead of Latitude" ~ !!sym(x) %>% as.numeric,
#         issue_lat == "Latitude invalid format: mix beteen DD and DMS" ~ gsub("N|?", "", gsub(" ", "", !!sym(y))) %>% as.numeric,
#         issue_lat == "Latitude not in DD but in DMS" ~ parzer::parse_lat(gsub("\"|N|LAT", "", !!sym(y))) %>% as.numeric,
#         issue_lat == "Latitude include invalid characters" ~ gsub(",|-|\\/|\\\\| |،", "", !!sym(y)) %>% as.numeric,
#         TRUE ~ !!sym(y) %>% arabic.tonumber %>% as.numeric),
#       issue.gps = ifelse((issue_lon!="") | (issue_lat!=""), paste0(issue_lon,", ", issue_lat), "")
#     )
#   return(df.temp)
# }

# partial_join.archived <- function(x, y, pattern_x, by_y){
#   # idy_y <- sapply(x[[pattern_x]], grep, y[[by_y]])                            # for each sitename in x, get the row indices in y with corresponding partial matches (as a list)
#   # idy_y <- sapply(x, grep, y[[by_y]])
#   # idy_y <- sapply(x[[pattern_x]], grep, y[[by_y]])
#   # idy_y <- lapply(idy_y, function(x) if (sum(!is.na(x)) > 0) {x[!is.na(x) & x!="(" & x!=")"]} )
#   z <- x[[pattern_x]] %>% str_split(" |,")                                      # splits the sitename with space as separator to see if any part of the sitename has a corresponding match
#   z <- lapply(z, function(x) paste(x, collapse="|")) %>% unlist                 # collapse all substring together with "|" statements
#   idy_y <- sapply(x[[pattern_x]], function(s) {                                 # Return the indice for any match from any of the substring in the sitename
#     if (grepl("NA|\\(|\\)", s)) {s <- gsub("NA", "", s)}                            # If there is a NA, clean it
#     if (s == "" | is.na(s)) {s} else {                                        # If there is a blank of NA, just return NA
#       res <- grep(s, y[[by_y]])}})                                          # Otherwise return indices of any of the substring of the sitename entered in arabic
#   idx_x <- sapply(seq_along(idy_y), function(i) rep(i, length(idy_y[[i]])))     # calls the corresponding row numbers in x to join them to y (repeats the x match as many time as partial match in y)
#   df <- dplyr::bind_cols(x[unlist(idx_x), , drop = F],                          # calls all partial matches rows of x, repeating the multiple matches if necessary
#                          y[unlist(idy_y), , drop = F]) %>%                      # calls the partially matching rows of y => bind_cols creates the output dataframe
#     select(all_of(pattern_x), all_of(by_y), everything())
#   df <- plyr::rbind.fill(df, x[-unlist(idx_x), , drop = F])                     # add all other non matching lines 
#   
#   return(df)
# }

# partial_join_old <- function(x, y, pattern_x, by_y){
#   # idy_y <- sapply(x[[pattern_x]], grep, y[[by_y]])                            # for each sitename in x, get the row indices in y with corresponding partial matches (as a list)
#   # idy_y <- sapply(x, grep, y[[by_y]])
#   # z <- x[[pattern_x]] %>% str_split(" ") %>% gsub("\\(|\\)", "", .)
#   z <- x[[pattern_x]] %>% str_split(" ")                                        # splits the sitename with space as separator to see if any part of the sitename has a corresponding match
#   idy_y <- sapply(z, pmatch, y[[by_y]])
#   idy_y <- lapply(idy_y, function(x) if (sum(!is.na(x))> 0) {x[!is.na(x) & x!="(" & x!=")"]} )
#   idx_x <- sapply(seq_along(idy_y), function(i) rep(i, length(idy_y[[i]])))     # calls the corresponding row numbers in x to join them to y (repeats the x match as many time as partial match in y)
#   df <- dplyr::bind_cols(x[unlist(idx_x), , drop = F],                          # calls all partial matches rows of x, repeating the multiple matches if necessary
#                          y[unlist(idy_y), , drop = F]) %>%                      # calls the partially matching rows of y => bind_cols creates the output dataframe
#     select(all_of(pattern_x), all_of(by_y), everything())
#   df <- plyr::rbind.fill(df, x[-unlist(idx_x), , drop = F])                     # add all non matching
#   # add the non matching lines 
#   return(df)
# }

## save.workbook cleaning log archive
# 
# save.follow.up.requests.old <- function(cl, filename.out="output/test.xlsx"){
#   # save follow-up requests
#   wb <- createWorkbook()
#   addWorksheet(wb, "Follow-up")
#   writeData(wb = wb, x = cl, sheet = "Follow-up", startRow = 1)
#   
#   style.col.color <- createStyle(fgFill="#E5FFCC", border="TopBottomLeftRight", borderColour="#000000")
#   style.col.color.first <- createStyle(textDecoration="bold", fgFill="#E5FFCC",
#                                        border="TopBottomLeftRight", borderColour="#000000", wrapText=F)
#   
#   addStyle(wb, "Follow-up", style = style.col.color, rows = 1:(nrow(cl)+1), cols=4)
#   addStyle(wb, "Follow-up", style = style.col.color, rows = 1:(nrow(cl)+1), cols=5)
#   col.style <- createStyle(textDecoration="bold", fgFill="#CECECE",halign="center",
#                            border="TopBottomLeftRight", borderColour="#000000")
#   setColWidths(wb, "Follow-up", cols=2, widths=15)
#   setColWidths(wb, "Follow-up", cols=3, widths=15)
#   setColWidths(wb, "Follow-up", cols=c(1,4,8:9), widths="auto")
#   setColWidths(wb, "Follow-up", cols=5, widths=60)
#   setColWidths(wb, "Follow-up", cols=6, widths=15)
#   setColWidths(wb, "Follow-up", cols=7, widths=15)
#   setColWidths(wb, "Follow-up", cols=10, widths=15)
#   
#   addStyle(wb, "Follow-up", style = createStyle(wrapText=T), rows = 1:(ncol(cl)+1), cols=6)
#   addStyle(wb, "Follow-up", style = createStyle(wrapText=T), rows = 1:(ncol(cl)+1), cols=7)
#   
#   addStyle(wb, "Follow-up", style = col.style, rows = 1, cols=1:dim(cl)[2])
#   
#   col.id <- which(colnames(cl)=="old_value")
#   random.color <- ""
#   for (r in 2:nrow(cl)){
#     if(as.character(cl[r, "uuid"])==as.character(cl[r-1, "uuid"]) & 
#        as.character(cl[r, "check_id"])==as.character(cl[r-1, "check_id"])){
#       if (random.color == "") random.color <- randomColor(1, luminosity = "light")
#       addStyle(wb, "Follow-up", style = createStyle(fgFill=random.color, wrapText=T), 
#                rows = r:(r+1), cols=col.id)
#     } else random.color=""
#   }
#   addStyle(wb, "Follow-up", style = style.col.color.first, rows = 1, cols=4)
#   addStyle(wb, "Follow-up", style = style.col.color.first, rows = 1, cols=5)
#   saveWorkbook(wb, filename.out, overwrite = TRUE)
# } 

# var.to.be.checked <- unique(cl$variable)[unique(cl$variable) %in% colnames(data.val)]
# rows.1 <- which(cl$variable == "c9_primary_shelter_type")
# for (var in var.to.be.checked){
#   dataValidation(wb, "Follow-up", cols = which(colnames(cl)=="new_value"),
#                  rows = which(cl$variable == var), type = "list",
#                  value = get.col.range(variable=var))    
# }
# dataValidation(wb, "Follow-up", cols = which(colnames(cl)=="new_value"),
#                rows = 1:(nrow(cl)+1), type = "list",
#                value = get.col.range(variable="c9_primary_shelter_type"))



# add.to.cleaning.log.old <- function(checks, question.names=c(), issue="", new.value="" , fix="Checked with partner", checked_by="ON", add.col=c("")){
#   df <- initialise.cleaning.log()
#   if (nrow(checks)>0){
#     for(q.n in question.names){
#       new.entries <- checks %>% filter(flag) %>% 
#         mutate(uuid=uuid,
#                variable=q.n,
#                issue=issue,
#                old_value=!!sym(q.n),
#                new_value=new.value,
#                fix=fix,
#                checked_by=checked_by)
#       new.entries <- new.entries %>% select(all_of(col.cl), any_of(add.col))
#       df <- bind_rows(df, new.entries)
#     }
#     cleaning.log <<- bind_rows(cleaning.log, df %>% arrange(uuid, variable, agency))
#   }
# }

# Priority need checks old version [archived because it was taking forever and written with my feet]
# priority.need.checks.old <- function(df){
#   col.priority.needs <- c("i1_top_priority_need", "i2_second_priority_need", "i3_third_priority_need")
#   service.level.var <- df %>% select(c("shelter_maintenance_services":"waste_disposal_services")) %>% colnames
#   priority.need <- c("shelter_maintenance_assistance", "non_food_items", "food", "cash_assistance", 
#                      "water", "medical_assistance", "education", "livelihood_assistance",
#                      "protection_services", "nutrition_services", "sanitation_services")
#   logical.inconsistencies <- data.frame(service.level.var, priority.need)                               # Enable calling conditions between service question header and corresponding priority need choice 
#   for (var in service.level.var) {
#     check.col <- paste0(var, "_check")
#     issue.check.col <- paste0(var, "_issue")
#     df <- df %>%
#       mutate(!!check.col := ifelse((!!sym(var) == "adequate") &
#                                      ((i1_top_priority_need==logical.inconsistencies[logical.inconsistencies$service.level.var==var, "priority.need"]) |
#                                         (i2_second_priority_need==logical.inconsistencies[logical.inconsistencies$service.level.var==var, "priority.need"]) |
#                                         (i3_third_priority_need==logical.inconsistencies[logical.inconsistencies$service.level.var==var, "priority.need"])), 1, 0),
#              !!issue.check.col := ifelse(!!sym(check.col) == 1,
#                                          paste0(var, " reported as adequate but ", logical.inconsistencies[logical.inconsistencies$service.level.var==var, "priority.need"], " cited as top three priority need. To be checked."), ""))    
#   }
#   df <- df %>% select("uuid", "q0_3_organization", "a4_site_name", all_of(c(col.priority.needs,service.level.var)), matches("_issue|_check"), -rrm_distributions)
#   list.checks <- colnames(df)[grepl("_check", colnames(df))]
#   df.long <- data.frame()
#   for (i in seq_len(nrow(df))) {
#     for (col in list.checks){
#       conflicting_variable_old_value <- logical.inconsistencies[logical.inconsistencies$service.level.var==gsub("_check","",col),"priority.need"]
#       conflicting_variable <- df[i,] %>% select(matches("priority")) %>% pivot_longer(cols=colnames(.)) %>% filter(value==conflicting_variable_old_value) %>% select(name) %>% as.character
#       df.long <- df.long %>% rbind(data.frame(
#         df[i,c("uuid", "q0_3_organization", "a4_site_name")],
#         variable = gsub("_check", "", col),
#         old_value = df[i, gsub("_check", "", col)],
#         has.issue = df[i, col],
#         issue = df[i, gsub("_check", "_issue", col)])
#       ) %>% rbind(data.frame(
#         df[i,c("uuid", "q0_3_organization", "a4_site_name")],
#         variable = conflicting_variable,
#         old_value = conflicting_variable_old_value,
#         has.issue = df[i, col],
#         issue = df[i, gsub("_check", "_issue", col)]
#       )
#       )
#     }
#   }
#   res <- df.long %>%
#     filter(has.issue==1) %>%
#     mutate(new_value = "",fix="Checked with partner", checked_by="ON", 
#            check_id = paste0(lapply(issue, function(x) str_split(x, " ")[[1]][1]), "_priority_need_check")) %>%
#     dplyr::rename(agency=q0_3_organization, area=a4_site_name) %>%
#     dplyr::select(uuid, agency, area, variable, issue, check_id, old_value, new_value, fix, checked_by)
#   return(res)
# }

## phone number check [was not efficient and needed calling colnames manually ]

# phonenumber2 <- phonenumber %>% filter(!is.na(b3_exu_fp_mobile_number)) %>% 
#   mutate(exu_fb_wrong_number = ifelse(grep("^[70|71|73|77|79]", b3_exu_fp_mobile_number), 0, 1))
# phonenumber <- select(response, "uuid","q0_3_organization", "a4_site_name3", "b3_exu_fp_mobile_number", "b6_cccm_agency_fp_mobile_number")
# phonenumber2 <- phonenumber %>% filter(!is.na(b3_exu_fp_mobile_number)) %>% 
#                                          mutate(exu_fb_wrong_number = ifelse(grep("^[70|71|73|77|79]", b3_exu_fp_mobile_number), 0, 1))
# phonenumber2[, c("a4_site_name3", "b3_exu_fp_mobile_number", "b6_cccm_agency_fp_mobile_number")] <- NULL
# phonenumber3 <- phonenumber %>% filter(!is.na(b6_cccm_agency_fp_mobile_number)) %>% 
#                                          mutate(smc_agency_wrong_number = ifelse(grep("^[70|71|73|77|79]", b6_cccm_agency_fp_mobile_number), 0, 1))
# phonenumber3[, c("a4_site_name3", "b3_exu_fp_mobile_number", "b6_cccm_agency_fp_mobile_number")] <- NULL
# phonenumber_df <- plyr::join_all(list(phonenumber, phonenumber2, phonenumber3), 
#                                  by = "uuid", 
#                                  type = "left")
# phonenumber_df <- phonenumber_df[, !duplicated(colnames(phonenumber_df), fromLast = FALSE)] 
# phone_melt <- phonenumber_df %>% melt(id.vars = c("uuid","q0_3_organization", "a4_site_name3", "q1_3_key_informat_mobile_number", "b6_cccm_agency_fp_mobile_number", "b3_exu_fp_mobile_number", "phone.number"))


# phone_melt <- phone_melt %>% mutate(variable = ifelse(variable == "exu_fb_wrong_number", "b3_exu_fp_mobile_number",
#                                                            ifelse(variable == "smc_agency_wrong_number", "b6_smc_agency_fp_mobile_number", NA)))
#  
# phone_melt <- phone_melt %>% mutate(old_value = ifelse(value == ifelse((value == 1 & variable == "b3_exu_fp_mobile_number"), b3_exu_fp_mobile_number,
#                                                        ifelse((value == 1 & variable == "b6_smc_agency_fp_mobile_number"), b6_smc_agency_fp_mobile_number, NA))))