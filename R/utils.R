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

priority.need.checks <- function(df, check=""){
  col.priority.needs <- c("i1_top_priority_need", "i2_second_priority_need", "i3_third_priority_need")
  service.level.var = df %>% select(c("shelter_maintenance_services":"waste_disposal_services")) %>% colnames
  priority.need = c("shelter_maintenance_assistance", "non_food_items", "food", "cash_assistance", 
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
  df<-df %>% select("uuid", "q0_3_organization", "a4_site_name3", all_of(c(col.priority.needs,service.level.var)), matches("_issue|_check"), -rrm_distributions)
  list.checks <- colnames(df)[grepl("_check", colnames(df))]
  df.long <- data.frame()
  for (i in seq_len(nrow(df))) {
    for (col in list.checks){
      conflicting_variable_old_value <- logical.inconsistencies[logical.inconsistencies$service.level.var==gsub("_check","",col),"priority.need"]
      conflicting_variable <- df[i,] %>% select(matches("priority")) %>% pivot_longer(cols=colnames(.)) %>% filter(value==conflicting_variable_old_value) %>% select(name) %>% as.character
      df.long <- df.long %>% rbind(data.frame(
        df[i,c("uuid", "q0_3_organization", "a4_site_name3")],
        variable = gsub("_check", "", col),
        old_value = df[i, gsub("_check", "", col)],
        has.issue = df[i, col],
        issue_type = df[i, gsub("_check", "_issue", col)],
        conflicting_variable = conflicting_variable,
        conflicting_variable_old_value = conflicting_variable_old_value
        )
      )
    }
  }
  adequacy.log <- df.long %>%                                                                           # Creating adequacy cleaning log
    filter(has.issue==1) %>% 
    ungroup() %>%
    mutate(uuid = uuid,
           agency = q0_3_organization, 
           area = a4_site_name3, 
           variable = var, 
           issue = issue_type, 
           old_value = old_value, 
           new_value = " ",
           fix="Checked with partner",
           checked_by="ON",
           conflicting_variable=conflicting_variable,
           conflicting_variable_old_value=conflicting_variable_old_value,
           conflicting_variable_new_value="" ) %>% 
    select(uuid, agency, area, variable, issue, old_value, new_value, fix, checked_by,conflicting_variable, conflicting_variable_old_value, conflicting_variable_new_value)
  return(adequacy.log)
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
      grepl("°", !!sym(x)) & !is.na(gsub("°", "", !!sym(x)) %>% as.numeric)  ~ "Longitude invalid format: mix beteen DD and DMS",
      grepl("°|'", !!sym(x)) ~ "Longitude not in DD but in DMS",
      grepl(",|-|\\/|\\\\", !!sym(x)) ~ "Longitude include invalid characters",
      TRUE ~ ""),
      Longitude_clean = case_when(
        issue_lon == "Invalid Longitude coordinate (0)" ~ NA_real_,
        issue_lon == "Latitude entered instead of Longitude" ~ !!sym(y) %>% as.numeric,
        issue_lon == "Longitude not in Decimal Degree format" ~ NA_real_,
        issue_lon == "Longitude invalid format: mix beteen DD and DMS" ~ gsub("E|°", "", gsub(" ", "", !!sym(x))) %>% as.numeric,
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
        grepl("°", !!sym(y)) & !is.na(gsub("°", "", !!sym(x)) %>% as.numeric)  ~ "Latitude invalid format: mix beteen DD and DMS",
        grepl("°|'", !!sym(y)) ~ "Latitude not in DD but in DMS",
        grepl(",|-|\\/|\\\\| ", !!sym(y)) ~ "Latitude include invalid characters",
        TRUE ~ ""),
      Latitude_clean = case_when(
        issue_lat == "Invalid Latitude coordinate (0)" ~ NA_real_,
        issue_lat == "Latitude not in Decimal Degree format" ~ NA_real_,
        issue_lat == "Longitude entered instead of Latitude" ~ !!sym(x) %>% as.numeric,
        issue_lat == "Latitude invalid format: mix beteen DD and DMS" ~ gsub("N|°", "", gsub(" ", "", !!sym(y))) %>% as.numeric,
        issue_lat == "Latitude not in DD but in DMS" ~ parzer::parse_lat(!!sym(y)) %>% as.numeric,
        issue_lat == "Latitude include invalid characters" ~ gsub(",|-|\\/|\\\\| ", "", !!sym(y)) %>% as.numeric,
        TRUE ~ !!sym(y) %>% as.numeric),
      issue.gps = ifelse((issue_lon!="") |  (issue_lat!=""), paste0(issue_lon,", ", issue_lat), NA)
    )
  return(df.temp)
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
      grepl("°", !!sym(x)) & !is.na(gsub("°", "", !!sym(x)) %>% as.numeric)  ~ "Longitude invalid format: mix beteen DD and DMS",
      grepl("°|'", !!sym(x)) ~ "Longitude not in DD but in DMS",
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
        grepl("°", !!sym(y)) & !is.na(gsub("°", "", !!sym(x)) %>% as.numeric)  ~ "Latitude invalid format: mix beteen DD and DMS",
        grepl("°|'", !!sym(y)) ~ "Latitude not in DD but in DMS",
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

# Additionnal helper function to streamline labels based on the tool / call on questions choicelist, etc...

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
  list.choices <- choices %>% filter(!is.na(list_name)) %>% group_by(list_name) %>% 
    mutate(choices=paste(name, collapse=";\n"),
           choices.label=paste(`label::english`, collapse=";\n")) %>% 
    summarise(choices=choices[1], choices.label=choices.label[1])
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
    filter(grepl("Other|other",`label::english`)|grepl("????????",`label::arabic`)) %>% pull(name)
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
  return(as.character(tool.choices[tool.choices$list_name==list_name & 
                                     tool.choices$name==name, "label::English"]))
}

choice.label2name <- function(list_name, label){
  return(as.character(tool.choices[tool.choices$list_name==list_name & 
                                     tool.choices$`label::English`==label, "name"]))
}

get.old.value.label <- function(cl){
  for (r in 1:dim(cl)[1]){
    list.name <- as.character(cl[r, "list_name"])
    old.value <- as.character(cl[r, "old.value"])
    if (!is.na(list.name)){
      cl[r, "old.value"] <- choice.name2label(list.name, old.value)
    }
  }
  return(cl)
}

## Archived code

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