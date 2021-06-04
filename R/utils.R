# Cleaning functions

priority.need.checks <- function(df, check=""){
  col.priority.needs <- c("i1_top_priority_need", "i2_second_priority_need", "i3_third_priority_need")
  service.level.var <- df %>% select(c("shelter_maintenance_services":"waste_disposal_services")) %>% colnames
  priority.need <- c("shelter_maintenance_assistance", "non_food_items", "food", "cash_assistance", 
                    "water", "medical_assistance", "education", "livelihood_assistance",
                    "protection_services", "nutrition_services", "sanitation_services")
  logical.inconsistencies <- data.frame(service.level.var, priority.need)                               # Enable calling conditions between service question header and corresponding priority need choice 
  for (var in service.level.var) {
    check.col <- paste0(var, "_check")
    issue.check.col <- paste0(var, "_issue")
    df <- df %>%
      mutate(!!check.col := ifelse((!!sym(var) == "adequate") &
                                     ((i1_top_priority_need==logical.inconsistencies[logical.inconsistencies$service.level.var==var, "priority.need"]) |
                                        (i2_second_priority_need==logical.inconsistencies[logical.inconsistencies$service.level.var==var, "priority.need"]) |
                                        (i3_third_priority_need==logical.inconsistencies[logical.inconsistencies$service.level.var==var, "priority.need"])), 1, 0),
             !!issue.check.col := ifelse(!!sym(check.col) == 1,
                                         paste0(var, " reported as adequate but ", logical.inconsistencies[logical.inconsistencies$service.level.var==var, "priority.need"], " cited as top three priority need. To be checked."), ""))    
  }
  df<-df %>% select("uuid", "q0_3_organization", "a4_site_name", all_of(c(col.priority.needs,service.level.var)), matches("_issue|_check"), -rrm_distributions)
  list.checks <- colnames(df)[grepl("_check", colnames(df))]
  df.long <- data.frame()
  for (i in seq_len(nrow(df))) {
    for (col in list.checks){
      conflicting_variable_old_value <- logical.inconsistencies[logical.inconsistencies$service.level.var==gsub("_check","",col),"priority.need"]
      conflicting_variable <- df[i,] %>% select(matches("priority")) %>% pivot_longer(cols=colnames(.)) %>% filter(value==conflicting_variable_old_value) %>% select(name) %>% as.character
      df.long <- df.long %>% rbind(data.frame(
        df[i,c("uuid", "q0_3_organization", "a4_site_name")],
        variable = gsub("_check", "", col),
        old_value = df[i, gsub("_check", "", col)],
        has.issue = df[i, col],
        issue = df[i, gsub("_check", "_issue", col)])
        ) %>% rbind(data.frame(
          df[i,c("uuid", "q0_3_organization", "a4_site_name")],
          variable = conflicting_variable,
          old_value = conflicting_variable_old_value,
          has.issue = df[i, col],
          issue = df[i, gsub("_check", "_issue", col)]
          )
        )
    }
  }
  res <- df.long %>%
    filter(has.issue==1) %>%
    mutate(new_value = "",fix="Checked with partner", checked_by="ON", 
           check_id = paste0(lapply(issue, function(x) str_split(x, " ")[[1]][1]), "_priority_need_check")) %>%
    dplyr::rename(agency=q0_3_organization, area=a4_site_name) %>%
    dplyr::select(uuid, agency, area, variable, issue, check_id, old_value, new_value, fix, checked_by)
  return(res)
}

clean.gps <- function(df,x,y){
  df.temp<-df %>%
    mutate(issue_lon = case_when(
      is.na(!!sym(x)) ~ "No coordinates entered: follow-up",
      !!sym(x) %>% as.numeric == 0 ~ "Invalid Longitude coordinate (0)",
      near(!!sym(x) %>% as.numeric, !!sym(y) %>% as.numeric , tol = 0.2) ~ "Longitude and Latitude almost equal",
      (!!sym(x) %>% as.numeric < 20) & (!near(!!sym(x) %>% as.numeric, !!sym(y) %>% as.numeric , tol = 0.2)) ~ "Latitude entered instead of Longitude",
      !!sym(x) %>% as.numeric > 80 ~ "Longitude not in Decimal Degree format",
      grepl("E", !!sym(x)) ~ "Longitude invalid format: mix beteen DD and DMS",
      grepl("?", !!sym(x)) & !is.na(gsub("?", "", !!sym(x)) %>% as.numeric)  ~ "Longitude invalid format: mix beteen DD and DMS",
      grepl("?|'", !!sym(x)) ~ "Longitude not in DD but in DMS",
      grepl(",|-|\\/|\\\\", !!sym(x)) ~ "Longitude include invalid characters",
      TRUE ~ ""),
      Longitude_clean = case_when(
        issue_lon == "Invalid Longitude coordinate (0)" ~ NA_real_,
        issue_lon == "Latitude entered instead of Longitude" ~ !!sym(y) %>% as.numeric,
        issue_lon == "Longitude not in Decimal Degree format" ~ NA_real_,
        issue_lon == "Longitude invalid format: mix beteen DD and DMS" ~ gsub("E|?", "", gsub(" ", "", !!sym(x))) %>% as.numeric,
        issue_lon == "Longitude not in DD but in DMS" ~ parzer::parse_lon(!!sym(x)) %>% as.numeric,
        issue_lon == "Longitude include invalid characters" ~ gsub(",|-|\\/|\\\\| ", "", !!sym(x)) %>% as.numeric,
        TRUE ~ !!sym(x) %>% as.numeric),
      issue_lat = case_when(
        is.na(!!sym(y)) ~ "No coordinates entered: follow-up",
        !!sym(y) %>% as.numeric == 0 ~ "Invalid Latitude coordinate (0)",
        near(!!sym(x) %>% as.numeric, !!sym(y) %>% as.numeric , tol = 0.2) ~ "Longitude and Latitude almost equal",
        !!sym(y) %>% as.numeric > 80 ~ "Latitude not in Decimal Degree format",
        (!!sym(y) %>% as.numeric > 20) & (!near(!!sym(x) %>% as.numeric, !!sym(y) %>% as.numeric , tol = 0.2)) ~ "Longitude entered instead of Latitude",
        grepl("N", !!sym(y)) ~ "Latitude invalid format: mix beteen DD and DMS",
        grepl("?", !!sym(y)) & !is.na(gsub("?", "", !!sym(x)) %>% as.numeric)  ~ "Latitude invalid format: mix beteen DD and DMS",
        grepl("?|'", !!sym(y)) ~ "Latitude not in DD but in DMS",
        grepl(",|-|\\/|\\\\| ", !!sym(y)) ~ "Latitude include invalid characters",
        TRUE ~ ""),
      Latitude_clean = case_when(
        issue_lat == "Invalid Latitude coordinate (0)" ~ NA_real_,
        issue_lat == "Latitude not in Decimal Degree format" ~ NA_real_,
        issue_lat == "Longitude entered instead of Latitude" ~ !!sym(x) %>% as.numeric,
        issue_lat == "Latitude invalid format: mix beteen DD and DMS" ~ gsub("N|?", "", gsub(" ", "", !!sym(y))) %>% as.numeric,
        issue_lat == "Latitude not in DD but in DMS" ~ parzer::parse_lat(!!sym(y)) %>% as.numeric,
        issue_lat == "Latitude include invalid characters" ~ gsub(",|-|\\/|\\\\| ", "", !!sym(y)) %>% as.numeric,
        TRUE ~ !!sym(y) %>% as.numeric),
      issue.gps = ifelse((issue_lon!="") | (issue_lat!=""), paste0(issue_lon,", ", issue_lat), NA)
    )
  return(df.temp)
}

# x = response
# y = masterlist
# pattern_x = "a4_other_site"
# by_y = "Site_Name_In_Arabic"

partial_join <- function(x, y, pattern_x, by_y){
  # idy_y <- sapply(x[[pattern_x]], grep, y[[by_y]])                            # for each sitename in x, get the row indices in y with corresponding partial matches (as a list)
  # idy_y <- sapply(x, grep, y[[by_y]])
  # idy_y <- sapply(x[[pattern_x]], grep, y[[by_y]])
  # idy_y <- lapply(idy_y, function(x) if (sum(!is.na(x)) > 0) {x[!is.na(x) & x!="(" & x!=")"]} )
  z <- x[[pattern_x]] %>% str_split(" ")                                        # splits the sitename with space as separator to see if any part of the sitename has a corresponding match
  z <- lapply(z, function(x) paste(x, collapse="|"))
  idy_y <- sapply(x[[pattern_x]], function(s) {                                 # Return the indice for any match from any of the substring in the sitename
    if (grepl("NA|(|)", s)) {s <- gsub("NA", "", s)}                            # If there is a NA, clean it
      if (s == "" | is.na(s)) {s} else {                                        # If there is a blank of NA, just return NA
          res <- grep(s, y[[by_y]])}})                                          # Otherwise return indices of any of the substring of the sitename entered in arabic
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
        mutate(uuid=uuid,
               variable=q.n,
               issue=issue,
               check_id=check_id,
               old_value=!!sym(q.n),
               new_value=new.value,
               fix=fix,
               checked_by=checked_by)
      new.entries <- new.entries %>% select(all_of(col.cl), any_of(add.col))
      df <- bind_rows(df, new.entries)
    }
    cleaning.log <<- bind_rows(cleaning.log, df %>% arrange(uuid, variable, agency))
  }
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
  range.vect <- c("$", col.excel, "$2:$", col.excel, "$", (nrow+2))             ## nrow + 2 => to keep an additionnal field in drop down list to be updated if needed by partner // if you want to keep it strict, nrow + 1
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
  col.style <- createStyle(textDecoration="bold", fgFill="#CECECE",halign="center",
                           border="TopBottomLeftRight", borderColour="#000000")
  
  setColWidths(wb, "Follow-up", cols=2, widths=15)
  setColWidths(wb, "Follow-up", cols=3, widths=15)
  setColWidths(wb, "Follow-up", cols=c(1,4,8:9), widths="auto")
  setColWidths(wb, "Follow-up", cols=5, widths=60)
  setColWidths(wb, "Follow-up", cols=6, widths=15)
  setColWidths(wb, "Follow-up", cols=7, widths=15)
  setColWidths(wb, "Follow-up", cols=10, widths=15)
  
  addStyle(wb, "Follow-up", style = createStyle(wrapText=T), rows = 1:(ncol(cl)+1), cols=6)
  addStyle(wb, "Follow-up", style = createStyle(wrapText=T), rows = 1:(ncol(cl)+1), cols=7)
  addStyle(wb, "Follow-up", style = col.style, rows = 1, cols=1:dim(cl)[2])
  addStyle(wb, "Follow-up", style = style.col.color.first, rows = 1, cols=4)
  addStyle(wb, "Follow-up", style = style.col.color.first, rows = 1, cols=5)
  
  col.id <- which(colnames(cl)=="old_value")
  random.color <- ""
  for (r in 2:nrow(cl)){
    if(as.character(cl[r, "uuid"])==as.character(cl[r-1, "uuid"]) & 
       as.character(cl[r, "check_id"])==as.character(cl[r-1, "check_id"])){
      if (random.color == "") random.color <- randomColor(1, luminosity = "light")
      addStyle(wb, "Follow-up", style = createStyle(fgFill=random.color, wrapText=T), 
               rows = r:(r+1), cols=col.id)
    } else random.color=""
  }
  
  for (r in 1:nrow(cl)){
    if (cl[r,"variable"] %in% colnames(data.val)){
      dataValidation(wb, "Follow-up", cols = which(colnames(cl)=="new_value"),
                     rows = r+1, type = "list",
                     value = get.col.range(cl[r,"variable"]))
    }
  }
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
  # list of external choices for each list_name
  # list.choices.external <- external_choices %>% filter(!is.na(list_name)) %>% group_by(list_name) %>%
  #   mutate(choices=paste(name, collapse=";\n"),
  #          choices.label=paste(`label::english`, collapse=";\n")) %>%
  #   summarise(choices=choices[1], choices.label=choices.label[1])
  # list.choices <- bind_rows(list.choices, list.choices.external)
  # list of choices for each question
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


add.to.cleaning.log.old <- function(checks, question.names=c(), issue="", new.value="" , fix="Checked with partner", checked_by="ON", add.col=c("")){
  for(q.n in question.names){
    new.entries <- checks %>% filter(flag) %>% 
      mutate(uuid=uuid,
             variable=q.n,
             issue=issue,
             old_value=!!sym(q.n),
             new_value=new.value,
             fix=fix,
             checked_by=checked_by)
    new.entries <- new.entries %>% select(all_of(col.cl), any_of(add.col))
    cleaning.log <<- bind_rows(cleaning.log, new.entries)
  }
}

cleaning.log.new.entries <- function(df, var, issue_type ="", new_value=" ", fix="Checked with partner", checked_by="ON") {
  new.entries <- df %>% ungroup() %>%
    mutate(uuid = uuid,
           agency = q0_3_organization, 
           area = a4_site_name3, 
           variable = var, 
           issue = issue_type, 
           old_value = !!sym(var), 
           new_value = new_value, 
           fix = fix, 
           checked_by = checked_by) %>% 
    select(uuid, agency, area, variable, issue, old_value, new_value, fix, checked_by)
}
## Archived code

save.follow.up.requests.original <- function(){
  cols <- c("uuid", "agency", "area", 
            "variable", "label::english", 
            "issue", "old_value", "new_value", "explanation", "fix", "checked_by")
  # prepare cleaning log
  cl <- cleaning.log %>%
    mutate(explanation="")
  cl <- left_join(cl, select(get.select.db(), name, list_name), by=c("variable"="name"))
  cl <- get.old.value.label(cl)
  cl <- left_join(cl, select(choices, "name", "label::english"), by=c("variable"="name")) %>% 
    mutate(variable.label=ifelse(is.na(`label::english`), "", `label::english`)) %>% 
    select(all_of(cols)) %>% 
    arrange(uuid)
  # save follow-up requests
  wb <- loadWorkbook(filename.follow.up.readme)
  renameWorksheet(wb, 1, "ReadMe")
  addWorksheet(wb, "Follow-up")
  writeData(wb = wb, x = cl, sheet = "Follow-up", startRow = 1)
  input.style <- createStyle(fgFill="#DDFFDD", border="TopBottomLeftRight", borderColour="#000000")
  addStyle(wb, "Follow-up", style = input.style, rows = 2:(dim(cl)[1]+1), cols=(dim(cl)[2]-1))
  addStyle(wb, "Follow-up", style = input.style, rows = 2:(dim(cl)[1]+1), cols=dim(cl)[2])
  col.style <- createStyle(textDecoration="bold", fgFill="#CECECE", halign="center",
                           border="TopBottomLeftRight", borderColour="#000000")
  setColWidths(wb, "Follow-up", cols=3, widths=16)
  setColWidths(wb, "Follow-up", cols=4, widths=18)
  setColWidths(wb, "Follow-up", cols=5:9, widths=12)
  setColWidths(wb, "Follow-up", cols=10, widths=15)
  setColWidths(wb, "Follow-up", cols=11, widths=13)
  setColWidths(wb, "Follow-up", cols=12, widths=11)
  setColWidths(wb, "Follow-up", cols=13:14, widths=50)
  setColWidths(wb, "Follow-up", cols=15, widths=25)
  setColWidths(wb, "Follow-up", cols=16, widths=20)
  setColWidths(wb, "Follow-up", cols=17, widths=30)
  addStyle(wb, "Follow-up", style = createStyle(wrapText=T), rows = 1:(dim(cl)[1]+1), cols=13)
  addStyle(wb, "Follow-up", style = createStyle(wrapText=T), rows = 1:(dim(cl)[1]+1), cols=14)
  addStyle(wb, "Follow-up", style = createStyle(wrapText=T), rows = 1:(dim(cl)[1]+1), cols=15)
  addStyle(wb, "Follow-up", style = col.style, rows = 1, cols=1:dim(cl)[2])
  col.id <- which(colnames(cl)=="old_value")
  for (r in 2:dim(cl)[1]){
    if(as.character(cl[r, "check.id"])!="Outlier" &
       as.character(cl[r, "check.id"])!="Other.response" &
       as.character(cl[r, "check.id"])!="Typing" &
       as.character(cl[r, "id"])==as.character(cl[r-1, "id"]) &
       as.character(cl[r, "check.id"])==as.character(cl[r-1, "check.id"])){
      random.color <- randomColor(1, luminosity = "light")
      addStyle(wb, "Follow-up", style = createStyle(fgFill=random.color, wrapText=T), 
               rows = r:(r+1), cols=col.id)
    }
  }
  saveWorkbook(wb, filename.out.follow.up.requests, overwrite = TRUE)
}


partial_join_old <- function(x, y, pattern_x, by_y){
  # idy_y <- sapply(x[[pattern_x]], grep, y[[by_y]])                            # for each sitename in x, get the row indices in y with corresponding partial matches (as a list)
  # idy_y <- sapply(x, grep, y[[by_y]])
  # z <- x[[pattern_x]] %>% str_split(" ") %>% gsub("\\(|\\)", "", .)
  z <- x[[pattern_x]] %>% str_split(" ")                                        # splits the sitename with space as separator to see if any part of the sitename has a corresponding match
  idy_y <- sapply(z, pmatch, y[[by_y]])
  idy_y <- lapply(idy_y, function(x) if (sum(!is.na(x))> 0) {x[!is.na(x) & x!="(" & x!=")"]} )
  idx_x <- sapply(seq_along(idy_y), function(i) rep(i, length(idy_y[[i]])))     # calls the corresponding row numbers in x to join them to y (repeats the x match as many time as partial match in y)
  df <- dplyr::bind_cols(x[unlist(idx_x), , drop = F],                          # calls all partial matches rows of x, repeating the multiple matches if necessary
                         y[unlist(idy_y), , drop = F]) %>%                      # calls the partially matching rows of y => bind_cols creates the output dataframe
    select(all_of(pattern_x), all_of(by_y), everything())
  df <- plyr::rbind.fill(df, x[-unlist(idx_x), , drop = F])                     # add all non matching
  # add the non matching lines 
  return(df)
}

clean.gps.old <- function(df,x,y){
  df.temp<-df %>%
    mutate(issue_lon = case_when(
      is.na(!!sym(x)) ~ "No coordinates entered: follow-up",
      near(!!sym(x) %>% as.numeric, !!sym(y) %>% as.numeric , tol = 0.2) ~ "Longitude and Latitude almost equal",
      !!sym(x) %>% as.numeric == 0 ~ "Invalid Longitude coordinate (0)",
      (!!sym(x) %>% as.numeric < 20) & (!near(!!sym(x) %>% as.numeric, !!sym(y) %>% as.numeric , tol = 0.2)) ~ "Latitude entered instead of Longitude",
      !!sym(x) %>% as.numeric > 80 ~ "Longitude not in Decimal Degree format",
      grepl("E", !!sym(x)) ~ "Longitude invalid format: mix beteen DD and DMS",
      grepl("?", !!sym(x)) & !is.na(gsub("?", "", !!sym(x)) %>% as.numeric)  ~ "Longitude invalid format: mix beteen DD and DMS",
      grepl("?|'", !!sym(x)) ~ "Longitude not in DD but in DMS",
      TRUE ~ ""),
      Longitude_clean = case_when(
        issue_lon == "Invalid Longitude coordinate (0)" ~ NA_real_,
        issue_lon == "Latitude entered instead of Longitude" ~ !!sym(y) %>% as.numeric,
        issue_lon == "Longitude not in Decimal Degree format" ~ NA_real_,
        issue_lon == "Longitude invalid format: mix beteen DD and DMS" ~ gsub("E", "", gsub(" ", "", !!sym(x))) %>% as.numeric,
        issue_lon == "Longitude not in DD but in DMS" ~ parzer::parse_lon(!!sym(x)) %>% as.numeric,
        TRUE ~ !!sym(x) %>% as.numeric),
      issue_lat = case_when(
        is.na(!!sym(y)) ~ "No coordinates entered: follow-up",
        near(!!sym(x) %>% as.numeric, !!sym(y) %>% as.numeric , tol = 0.2) ~ "Longitude and Latitude almost equal",
        !!sym(y) %>% as.numeric == 0 ~ "Invalid Latitude coordinate (0)",
        !!sym(y) %>% as.numeric > 80 ~ "Latitude not in Decimal Degree format",
        (!!sym(y) %>% as.numeric > 20) & (!near(!!sym(x) %>% as.numeric, !!sym(y) %>% as.numeric , tol = 0.2)) ~ "Longitude entered instead of Latitude",
        grepl("N", !!sym(y)) ~ "Latitude invalid format: mix beteen DD and DMS",
        grepl("?", !!sym(y)) & !is.na(gsub("?", "", !!sym(x)) %>% as.numeric)  ~ "Latitude invalid format: mix beteen DD and DMS",
        grepl("?|'", !!sym(y)) ~ "Latitude not in DD but in DMS",
        TRUE ~ ""),
      Latitude_clean = case_when(
        issue_lat == "Invalid Latitude coordinate (0)" ~ NA_real_,
        issue_lat == "Latitude not in Decimal Degree format" ~ NA_real_,
        issue_lat == "Longitude entered instead of Latitude" ~ !!sym(x) %>% as.numeric,
        issue_lat == "Latitude invalid format: mix beteen DD and DMS" ~ gsub("N", "", gsub(" ", "", !!sym(y))) %>% as.numeric,
        issue_lat == "Latitude not in DD but in DMS" ~ parzer::parse_lat(!!sym(y)) %>% as.numeric,
        TRUE ~ !!sym(y) %>% as.numeric),
      issue.gps = ifelse((issue_lon!="") |  (issue_lat!=""), paste0(issue_lon," - ", issue_lat), NA)
    )
  return(df.temp)
}

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


## adequacy log [multiple columns format]
# priority.need.checks.old <- function(df, check=""){
#   col.priority.needs <- c("i1_top_priority_need", "i2_second_priority_need", "i3_third_priority_need")
#   service.level.var = df %>% select(c("shelter_maintenance_services":"waste_disposal_services")) %>% colnames
#   priority.need = c("shelter_maintenance_assistance", "non_food_items", "food", "cash_assistance", 
#                     "water", "medical_assistance", "education", "livelihood_assistance",
#                     "protection_services", "nutrition_services", "sanitation_services")
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
#   df<-df %>% select("uuid", "q0_3_organization", "a4_site_name", all_of(c(col.priority.needs,service.level.var)), matches("_issue|_check"), -rrm_distributions)
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
#         issue_type = df[i, gsub("_check", "_issue", col)],
#         conflicting_variable = conflicting_variable,
#         conflicting_variable_old_value = conflicting_variable_old_value
#       )
#       )
#     }
#   }
#   adequacy.log <- df.long %>%                                                                           # Creating adequacy cleaning log
#     filter(has.issue==1) %>% 
#     ungroup() %>%
#     mutate(uuid = uuid,
#            agency = q0_3_organization, 
#            area = a4_site_name3, 
#            variable = var, 
#            issue = issue_type, 
#            old_value = old_value, 
#            new_value = " ",
#            fix="Checked with partner",
#            checked_by="ON",
#            conflicting_variable=conflicting_variable,
#            conflicting_variable_old_value=conflicting_variable_old_value,
#            conflicting_variable_new_value="" ) %>% 
#     select(uuid, agency, area, variable, issue, old_value, new_value, fix, checked_by,conflicting_variable, conflicting_variable_old_value, conflicting_variable_new_value)
#   return(adequacy.log)
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