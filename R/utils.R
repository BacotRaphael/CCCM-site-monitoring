# Cleaning functions

## 1. Numerical outlier 
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

## 2. Priority needs check
priority.check <- function(df, var){
  patt_rep <- logical.inconsistencies %>% 
    mutate(replacement = ifelse(service.level.var==var, priority.need, NA)) %>% 
    select(-issue, -service.level.var) %>%
    rbind(c("legal_services", NA)) 
  ## Choice in priority need that has no corresponding service level var in survey. to be checked
  keep <- logical.inconsistencies %>% filter(service.level.var==var) %>% pull(priority.need) %>% as.character
  df <- df %>% 
    mutate(flag = ifelse((!!sym(var) == "adequate") &
                           ((i1_top_priority_need==logical.inconsistencies[logical.inconsistencies$service.level.var==var, "priority.need"]) |
                           (i2_second_priority_need==logical.inconsistencies[logical.inconsistencies$service.level.var==var, "priority.need"]) |
                           (i3_third_priority_need==logical.inconsistencies[logical.inconsistencies$service.level.var==var, "priority.need"])), T, F),
           issue = ifelse(flag, logical.inconsistencies[logical.inconsistencies$service.level.var==var, "issue"], "")) %>%
    dplyr::select("uuid", "q0_3_organization", "a4_site_name", any_of(c(var, col.priority.needs)), flag, issue) %>% filter(flag) %>%
    dplyr::rename(agency=q0_3_organization, area=a4_site_name) %>%
    mutate_at(vars(matches("priority_need")), ~str_replace_all(., setNames(patt_rep$replacement,patt_rep$priority.need)))
  return(df)
}

priority.need.checks <- function(df){
  col.priority.needs <<- c("i1_top_priority_need", "i2_second_priority_need", "i3_third_priority_need")
  service.level.var <<- response %>% select(c("shelter_maintenance_services":"waste_disposal_services")) %>% colnames
  priority.need <<- c("shelter_maintenance_assistance", "non_food_items", "food", "cash_assistance", 
                     "water", "medical_assistance", "education", "livelihood_assistance",
                     "protection_services", "nutrition_services", "sanitation_services")
  logical.inconsistencies <<- data.frame(service.level.var, priority.need) %>%
    mutate(issue=paste0(service.level.var, " reported as adequate but ", priority.need, " cited as top three priority need. To be checked."))
  for (var in service.level.var){
    check_var <- priority.check(df, var)
    add.to.cleaning.log(check_var, check_id = paste0("5_check_priority_", var), question.names=c(var, col.priority.needs), issue = "issue")
  }
  cleaning.log <<- cleaning.log %>% filter(!(grepl("check_priority", check_id) & is.na(old_value)))
  adequacy_log <<- cleaning.log %>% filter(grepl("check_priority", check_id))
}

## 3. Gps checks
arabic.tonumber <- function(s){
  arabic <- c("\u0660\u0661\u0662\u0663\u0664\u0665\u0666\u0667\u0668\u0669\u06F0\u06F1\u06F2\u06F3\u06F4\u06F5\u06F6\u06F7\u06F8\u06F9")
  english <- c("01234567890123456789")
  suppressWarnings(res <- lapply(s, function(x) as.numeric(chartr(arabic, english, x))) %>% unlist)
  # suppressWarnings(res <- as.numeric(chartr(arabic,english,s)))
  res[which(is.na(res))] <- s[which(is.na(res))]
  return(res)
}

clean.invalid.char.gps <- function(df, x, y){
  df.temp <- df %>%
    mutate(flag_lon = ifelse(grepl("،", !!sym(x)) | grepl(",|-|\\/|\\\\", !!sym(x)), T, F),
           issue_lon = ifelse(flag_lon, "Longitude include invalid characters", ""),
           long_clean = arabic.tonumber(ifelse(flag_lon, gsub("،", "\\.", gsub(",|-|\\/|\\\\", "", !!sym(x))), !!sym(x))),
           
           flag_lat = ifelse(grepl("،", !!sym(y)) | grepl(",|-|\\/|\\\\", !!sym(y)), T, F),
           issue_lat = ifelse(flag_lat, "Latitude include invalid characters", ""),
           lat_clean = arabic.tonumber(ifelse(flag_lat, gsub("،", "\\.", gsub(",|-|\\/|\\\\", "", !!sym(y))), !!sym(y))))
  return(df.temp)
}

sanitize.gps <- function(df, x, y){
  df.temp <- df %>%
    mutate(issue_lon = case_when(
             is.na(long_clean) ~ paste0(issue_lon, "; No coordinates entered: follow-up"),
             (long_clean %>% as.numeric == 0) | (long_clean %>% as.numeric %>% abs > 180) ~ paste0(issue_lon, "; Invalid Longitude coordinate"),
             near(long_clean %>% as.numeric, lat_clean %>% as.numeric , tol = 0.2) ~ paste0(issue_lon, "; Longitude and Latitude almost equal"),
             (long_clean %>% as.numeric < 20) & (lat_clean %>% as.numeric %>% abs < 90) &
               (!near(long_clean %>% as.numeric, lat_clean %>% as.numeric , tol = 0.2)) ~ paste0(issue_lon, "; Latitude entered instead of Longitude"),
             grepl("°|E|\"|LON", long_clean) & 
               (!is.na(parzer::parse_lon(gsub("E|\"|LON", "", long_clean)))) ~ paste0(issue_lon, "; Longitude invalid format: mix beteen DD and DMS"),
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
             grepl("°|LON|N|\"|LAT", lat_clean) & 
               (!is.na(parzer::parse_lat(gsub("LON|N|\"|LAT", "", lat_clean)))) ~ paste0(issue_lat, "; Latitude invalid format: mix beteen DD and DMS"),
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

clean.gps <- function(df, x, y){
  df.temp <- df %>%
    mutate(flag_lon = ifelse(grepl("،", !!sym(x)) | grepl(",|-|\\/|\\\\", !!sym(x)), T, F),
           issue_lon = ifelse(flag_lon, "Longitude include invalid characters", ""),
           # long_clean = unlist(lapply(ifelse(flag_lon, gsub("،", "\\.", gsub(",|-|\\/|\\\\", "", !!sym(x))), !!sym(x)), arabic.tonumber)),
           long_clean = arabic.tonumber(ifelse(flag_lon, gsub("،", "\\.", gsub(",|-|\\/|\\\\", "", !!sym(x))), !!sym(x))),
           
           flag_lat = ifelse(grepl("،", !!sym(y)) | grepl(",|-|\\/|\\\\", !!sym(y)), T, F),
           issue_lat = ifelse(flag_lat, "Latitude include invalid characters", ""),
           lat_clean = arabic.tonumber(ifelse(flag_lat, gsub("،", "\\.", gsub(",|-|\\/|\\\\", "", !!sym(y))), !!sym(y))),
           
           issue_lon = case_when(
             is.na(long_clean) ~ paste0(issue_lon, "; No coordinates entered: follow-up"),
             (long_clean %>% as.numeric == 0) | (long_clean %>% as.numeric %>% abs > 180) ~ paste0(issue_lon, "; Invalid Longitude coordinate"),
             near(long_clean %>% as.numeric, lat_clean %>% as.numeric , tol = 0.2) ~ paste0(issue_lon, "; Longitude and Latitude almost equal"),
             (long_clean %>% as.numeric < 20) & (lat_clean %>% as.numeric %>% abs < 90) &
               (!near(long_clean %>% as.numeric, lat_clean %>% as.numeric , tol = 0.2)) ~ paste0(issue_lon, "; Latitude entered instead of Longitude"),
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

gps.check.admin <- function(){ ## Flag GPS coordinates that lies in a different sub-district than the one entered - [st_intersection]
  response <<- response %>%                                                     # Match admin3 pcode with corresponding english name 
    left_join(adm3 %>% select(admin3Pcode, admin3Name_en) %>% dplyr::rename(admin3Name_en_df=admin3Name_en), by = c("a3_sub_district" = "admin3Pcode"))
  empty.gps.sites <<- response %>%
    filter(is.na(Longitude_clean) | is.na(Latitude_clean) | Longitude_clean == 0 | Latitude_clean == 0)
  
  ### Check in which admin boundaries the GPS location is supposed to fall
  response.sf <<- response %>%
    dplyr::mutate(admin2Pcode_df = a2_district,
                  admin3Pcode_df = a3_sub_district) %>%                           # Rename (sub)district column to know it shows district information as in survey (df)               
    filter(!is.na(Longitude_clean) & !is.na(Latitude_clean) & Longitude_clean != 0, Latitude_clean != 0) %>%
    mutate(SHAPE = mapply(c, as.numeric(Longitude_clean), as.numeric(Latitude_clean), SIMPLIFY = F) %>%
             map(st_point)) %>%                                                   # Add a geometry column to the response dataframe
    st_sf(crs = 4326, sf_column_name = "SHAPE")
  response.df <<- response.sf %>% st_drop_geometry                                 # Drop the geometry column to be able to do intersect/non-intersection between the two
  
  ## Get match original data with the sbd boundaries corresponding to the entered GPS location
  valid.gps.sf <<- st_intersection(response.sf, adm3) %>% st_as_sf()               # will subset to only keep gps entries falling in adm3 existing boundaries [nrows will be lower or equal to original file nrows]
  valid.gps.df <<- valid.gps.sf %>% st_drop_geometry %>%                           # Drop the geometry column to be able to do intersect/non-intersection between the two
    select(intersect(colnames(valid.gps.sf), colnames(response.df)))              # select the original df's columns to be able to use setdiff()
  
  ## => list the GPS location that falls outside of Yemen boundaries // Here we used the cleaned gps coordinates so it should be zero.
  ##  => Displays the gps entries outside of all adm3 boundaries [shows rows that disappeared when doing st_instersect i.e.]
  non.valid.gps.entries <<- 
    setdiff(response.df %>% select(intersect(colnames(response.df), colnames(valid.gps.df))), valid.gps.df) 
  response.df <<- response.df %>%
    mutate(issue.gps = ifelse(uuid %in% non.valid.gps.entries$uuid,
                              "The gps location falls outside of Yemen boundaries", issue.gps))
  
  ## => list all entries that have wrong admin names [column a2_district in original data not corresponding to the boundaries in which GPS location falls]
  ## Can add this after joining the admin level from site_name list
  valid.gps.sf <<- valid.gps.sf %>%
    mutate(issue.gps = ifelse(admin3Pcode_df != admin3Pcode,
                              "The sub-district name entered is not corresponding to the gps location entered", issue.gps))
  response.df.test <<- response.df %>%
    left_join(valid.gps.sf %>% st_drop_geometry %>%
                select(uuid, admin2Pcode, admin2Name_en, admin3Pcode, admin3Name_en) %>% 
                rename_at(vars(-matches("uuid")), ~paste0(., ".gps.matched")), by = "uuid")
  response.df <<- response.df %>%
    mutate(issue.admin3 = ifelse(is.na(admin3Pcode.gps.matched), "",
                                 ifelse(admin3Pcode.gps.matched != admin3Pcode_df,
                                        paste0("The sub-district name entered (", admin3Name_en_df, ") is not corresponding to the gps location entered (", admin3Name_en.gps.matched, ")."), "")))
  response.with.gps <<- valid.gps.sf %>% 
    bind_rows(non.valid.gps.entries %>%
                left_join(response.sf %>% select(a5_1_gps_longitude, a5_2_gps_latitude, SHAPE),
                          by = c("a5_1_gps_longitude", "a5_2_gps_latitude"))) 
}

## 4. Arabic name matching
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

partial_join_ngo <- function(x, y, pattern_x, by_y){
  z <- x[[pattern_x]] %>% str_squish %>% str_split(" |\\|,|\\+")                                      # splits the sitename with space as separator to see if any part of the sitename has a corresponding match
  z <- lapply(z, function(x) paste(x, collapse="|")) %>% unlist %>%
    gsub("\\|\\|\\|", "\\|", .) %>% gsub("\\|\\|", "\\|", .) %>% gsub(",", "",.)# collapse all substring together with "|" statements
  idy_y <- sapply(z, function(s) {                                              # Return the indice for any match from any of the substring in the sitename
    if (grepl("NA", s)) {s <- gsub("NA", "", s)}                                # If there is a NA, clean it
    if (s == "" | is.na(s)) {s} else {                                          # If there is a blank of NA, just return NA
      res <- grep(s, y[[by_y]])}})
  idx_x <- sapply(seq_along(idy_y), function(i) rep(i, length(idy_y[[i]])))     # calls the corresponding row numbers in x to join them to y (repeats the x match as many time as partial match in y)
  df <- dplyr::bind_cols(x[unlist(idx_x), , drop = F],                          # calls all partial matches rows of x, repeating the multiple matches if necessary
                         y[unlist(idy_y), , drop = F]) %>%                      # calls the partially matching rows of y => bind_cols creates the output dataframe
    select(all_of(pattern_x), all_of(by_y), everything())
  df <- plyr::rbind.fill(df, x[-unlist(idx_x), , drop = F])                     # add all other non matching lines 
  
  return(df)
}

## 5. Cleaning log functions
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
      new.entries <- checks %>%  filter(flag) %>%
        # setnames(old = c("q0_3_organization", "a4_site_name"), new = c("agency", "area"), skip_absent = T) %>%
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

save.service.provider.follow.up <- function(cl, filename.out="output/test.xlsx"){
  wb <- createWorkbook()
  addWorksheet(wb, "Follow-up")
  addWorksheet(wb, "Choices validation")
  
  choices.ngo <- choices.ngo %>% arrange(ngo_code)

  # ngo.choices <- sort(data.val$b4_site_cccm_agency_name[!is.na(data.val$b4_site_cccm_agency_name)])
  
  writeData(wb = wb, x = cl, sheet = "Follow-up", startRow = 1)
  writeData(wb = wb, x = choices.ngo, sheet = "Choices validation", startRow = 1)
  
  col.style <- createStyle(textDecoration="bold", fgFill="#CECECE", halign="center",
                           border="TopBottomLeftRight", borderColour="#000000")
  addStyle(wb, "Follow-up", style = col.style, rows = 1, cols=1:dim(cl)[2])
  dataValidation(wb, "Follow-up", cols = which(colnames(cl)=="new_value"),
                 rows = 2:nrow(cl), type = "list",
                 value = paste0("'Choices validation'!$A$2:$A", nrow(choices.ngo)+1))
  
  saveWorkbook(wb, filename.out, overwrite = TRUE)
  }

save.org.name.follow.up <- function(cl, filename.out="output/test.xlsx"){
  # save organisation name follow-up requests
  
  if (nrow(cl)==0) {print("There is no cleaning log to import. all is fine.")} else {
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

save.follow.up.requests <- function(cl, filename.out="output/test.xlsx"){       # save follow-up requests
  wb <- createWorkbook()
  addWorksheet(wb, "Follow-up")
  addWorksheet(wb, "Choices validation")
  writeData(wb = wb, x = cl, sheet = "Follow-up", startRow = 1)
  
  data.validation.list()
  writeData(wb = wb, x = data.val, sheet = "Choices validation", startRow = 1)
  
  # style.col.color <- createStyle(fgFill="#E5FFCC", border="TopBottomLeftRight", borderColour="#000000")
  # style.col.color.first <- createStyle(textDecoration="bold", fgFill="#E5FFCC",
                                       # border="TopBottomLeftRight", borderColour="#000000", wrapText=F)
  
  # addStyle(wb, "Follow-up", style = style.col.color, rows = 1:(nrow(cl)+1), cols=4)
  # addStyle(wb, "Follow-up", style = style.col.color, rows = 1:(nrow(cl)+1), cols=5)
  col.style <- createStyle(textDecoration="bold", fgFill="#CECECE", halign="center",
                           border="TopBottomLeftRight", borderColour="#000000")
  
  setColWidths(wb, "Follow-up", cols=1, widths=20)
  setColWidths(wb, "Follow-up", cols=2, widths=15)
  setColWidths(wb, "Follow-up", cols=3, widths=15)
  setColWidths(wb, "Follow-up", cols=4, widths=25)
  setColWidths(wb, "Follow-up", cols=5, widths=60)
  setColWidths(wb, "Follow-up", cols=6, widths=15)
  setColWidths(wb, "Follow-up", cols=7, widths=15)
  setColWidths(wb, "Follow-up", cols=8, widths=8)
  setColWidths(wb, "Follow-up", cols=9, widths=20)
  setColWidths(wb, "Follow-up", cols=10, widths=15)
  
  addStyle(wb, "Follow-up", style = createStyle(wrapText=T), rows = 1:(ncol(cl)+1), cols=6)
  addStyle(wb, "Follow-up", style = createStyle(wrapText=T), rows = 1:(ncol(cl)+1), cols=7)
  addStyle(wb, "Follow-up", style = col.style, rows = 1, cols=1:dim(cl)[2])
  # addStyle(wb, "Follow-up", style = style.col.color.first, rows = 1, cols=4)
  # addStyle(wb, "Follow-up", style = style.col.color.first, rows = 1, cols=5)
  
  col.id <- which(colnames(cl) %in% c("variable", "issue", "old_value"))
  random.color <- ""
  if (nrow(cl)>1) {for (r in 2:nrow(cl)){
    if(as.character(cl[r, "uuid"])==as.character(cl[r-1, "uuid"]) & 
       as.character(cl[r, "check_id"])==as.character(cl[r-1, "check_id"])){
      if (random.color == "") random.color <- randomColor(1, luminosity = "light")
      addStyle(wb, "Follow-up", style = createStyle(fgFill=random.color, wrapText=F), 
               rows = r:(r+1), cols=col.id, gridExpand = T)
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

## 6. Kobo tool and label functions
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

data.validation.list.c <- function(choices, tool){
  choicelist <- get.select.db.c(choices, tool) %>% dplyr::select(name, choices)                  # extract list of valid answer for all survey
  choice_validation <- choicelist %>% mutate(col="", .before=1) %>% transpose %>% as.data.frame %>% setNames(.[1,]) %>% slice(-1) %>% mutate_all(~str_split(.,";\n"))
  nrow_validation <- lapply(choice_validation, function(x) length(x[[1]])) %>% unlist %>% max
  data.val <<- data.frame(matrix(NA, nrow = nrow_validation, ncol = 0))
  for (c in colnames(choice_validation)){
    data.val <- data.val %>% mutate(!!sym(c) := c(unlist(choice_validation[[c]]), rep(NA, nrow_validation-length(choice_validation[[c]][[1]]))))
  }
  return(data.val)
}

choice.long <- function(choices, tool){
  choices <- data.validation.list.c(choices, tool)
  choices_long <- choices %>% pivot_longer(cols=colnames(.)) %>% arrange(name) %>% filter(!is.na(value))
  return(choices_long)
}

get.select.db.c <- function(choices, tool){
  # list of choices for each list_name (from TOOL_CHOICES)
  list.choices <- choices %>% filter(!is.na(list_name)) %>% group_by(list_name) %>%
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

# Old function not used
arabic.tonumber.old <- function(s){
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

#5. Change date to text (i.e. March 2021)

number.to.arabic <- function(s){
  arabic <- c("\u0660","\u0661","\u0662","\u0663","\u0664","\u0665","\u0666","\u0667","\u0668","\u0669")
  english <- c("0","1","2","3","4","5","6","7","8","9")
  res <- s %>% str_replace_all(setNames(arabic, english))
  return(res)
}

months.to.arabic <- function(s){
  arabic <- c("يناير", "فبراير", "مارس", "أبريل", "مايو", "يونيو", "يوليو", "أغسطس", "سبتمبر", "أكتوبر", "نوفمبر", "ديسمبر")
  english <- c("January","February","March","April","May","June","July","August","September","October", "November", "December")
  res <- s %>% str_replace_all(setNames(arabic, english))
  return(res)
}

date.to.monthyear <- function(x){
  y <- as.Date(x, format = "%Y-%m-%d")
  res <- paste0(months(y), " ", format(y, "%Y"))
  return(res)
}

date.to.monthyear.arabic <- function(x){
  y <- as.Date(x, format = "%Y-%m-%d")
  z <- paste0(months(y), " ", format(y, "%Y"))
  res1 <- months.to.arabic(z)
  res <- number.to.arabic(res1)
  return(res)
}

date.to.monthyear.arabic.test <- function(x){
  z <- date.to.monthyear(x)
  res <- number.to.arabic(z) %>% months.to.arabic(.)
  return(res)
}


clean.date.vec <- function(x){
  res <- lapply(x, clean.date) %>% unlist
}

clean.date <- function(x){
  if (!grepl("\\/",x) & !grepl("-",x) & grepl("\\d", x)){
    res <- as.character(as.Date(as.numeric(x), origin="1900-01-01"))
  } else {res <- x}
  return(res)
}

