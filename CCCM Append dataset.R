### Script that takes V1 and V2 files and appends them to Masters
# REACH Yemen - christine.pfeffer@reach-initiative.org
# Update Raphael Bacot - raphael.bacot@reach-initiative.org
# 01/07/2021

origin <- "1900-01-01"
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
today <- Sys.Date()

require(tidyverse)
require(openxlsx)
require(readxl)
require(lubridate)

source("R/utils.R")

### filename paths
survey.v1.filename <- "data/kobo/CCCM_Site_Reporting_Kobo_tool_V1_23_05_2021_FINAL.xlsx"
survey.v2.filename <- "data/kobo/CCCM_Site_Reporting_Kobo_tool_V2_23_05_2021_FINAL.xlsx" 
# id.list.filename <- "data/archive/CCCM IDP Sites_making NEW site names and IDs_May 2021_1072021.xlsx"
sitename.masterlist.filename <- "./data/CCCM_Site Reporting List_March 2021_ALL internal_revised 9 Aug.xlsx"
# last.internal.v1.filename <- "./output/internal/CCCM_SiteReporting_V1 Internal_2021-07-01.xlsx"
# last.internal.v2.filename <- "./output/internal/CCCM_SiteReporting_V2 Internal_2021-07-01.xlsx" 
last.internal.v1.filename <- "./output/Internal/CCCM_SiteReporting_V1 Internal_2021-08-08.xlsx"
last.internal.v2.filename <- "./output/Internal/CCCM_SiteReporting_V2 Internal_2021-08-09.xlsx" 

### Load kobo choices file to vlook up old site codes
survey_v1 <- read.xlsx(survey.v1.filename, sheet = "survey")
survey_v2 <- read.xlsx(survey.v2.filename, sheet = "survey")
choices_v1 <- read.xlsx(survey.v1.filename, sheet = "choices")
choices_v2 <- read.xlsx(survey.v2.filename, sheet = "choices")

# binding all choices from the two tools together
choices <- bind_rows(choices_v1, choices_v2) %>% group_by(list_name, name, `label::english`, `label::arabic`) %>% summarise(n=n()) %>% select(-n) %>% ungroup
# choices <- read.csv("./data/kobo/choices.csv", check.names = T) %>% select(-X)
external_choicesv1 <- read.xlsx(survey.v1.filename, sheet = "external_choices") %>% filter(list_name == "sitename")
external_choicesv2 <- read.xlsx(survey.v2.filename, sheet = "external_choices") %>% filter(list_name == "sitename")

# Binding surveys together
survey <- survey_v1 %>% bind_rows(survey_v2) %>% filter(!duplicated(name))

# external_choices <- filter(external_choices, external_choices$list_name == "sitename")
# names(external_choices)[names(external_choices) == "name"] <- "a4_site_code"
# names(external_choices)[names(external_choices) == "label::english"] <- "a4_site_name"

## Load site ID master list
# id_list <- read.xlsx(id.list.filename)
# id_list <- read.xlsx("./data/CCCM IDP Sites_making NEW site names and IDs_May 2021_1072021.xlsx")
#id_list$name <- str_trim(id_list$a4_site_name)

#################################### INTERNAL ##############################################################################

### Produce Internal Updated dataset ###

# I. Import all necessary files with renaming of similar column accross V1 and V2 when necessary

#### Internal Master file
# master_all_int <- read_excel(sitename.masterlist.filename, guess_max = 900)
master_all_int <- read.xlsx(sitename.masterlist.filename)

### Load Last Cleaned files ###
## Internal
last_internal_v1 <- read.xlsx(last.internal.v1.filename)
last_internal_v2 <- read.xlsx(last.internal.v2.filename)

## If only TRUE it means that no new values have been cleaned - if FALSE, means that there are no surveys in both in current dataset(v1 or v2) and in masterlist
unique(last_internal_v1$uuid %in% master_all_int$uuid)
unique(last_internal_v2$uuid %in% master_all_int$uuid)

## Take latest dataset and remove the duplicated surveys using the master
new_v1 <- anti_join(last_internal_v1, master_all_int, "uuid") %>% 
  dplyr::rename(d1_most_common_reason_idps_left_place_of_origin.security_concerns_conflict_explosives_lack_of_security_forces_war = d1_most_common_reason_idps_left_place_of_origin.security_concerns_conflict_explosives_lack_of_security_forces)

new_v2 <- anti_join(last_internal_v2, master_all_int, "uuid") %>%
  dplyr::rename(d1_most_common_reason_idps_left_place_of_origin.security_concerns_conflict_explosives_lack_of_security_forces_war = d1_most_common_reason_idps_left_place_of_origin.war)

new_int <- plyr::rbind.fill(new_v1, new_v2) %>%
  setNames(tolower(colnames(.)))                                                # Set names to lower cases to merge with master dataset

## Append the unique new entries to the final Master ALL Internal
new_master_all_int <- plyr::rbind.fill(master_all_int, new_int) %>%
  mutate_at(vars(colnames(.)[grepl("\\.", colnames(.)) & (!grepl("(?<!\\.|groups_)(other|Other)", colnames(.), perl = T))]), as.numeric) 

# new_master_all_int %>% select(matches("\\.")) %>% str
# new_master_all_int %>% select(matches("other|Other")) %>% str
# Checking date formatting
all.dates <- new_master_all_int$q0_4_date %>% unique
## Check visually that all dates in all.dates are in the same format and that there is no NA
all.dates
all.dates.as.date <- as.Date(all.dates, format = "%Y-%m-%d", origin = origin)
if (sum(is.na(all.dates.as.date))>0) stop("Date has multiple format. Align them all on the same format by running the lines below.")

## If there is multiple dates format, uncomment the lines below
new_master_all_int <- new_master_all_int %>% mutate(q0_4_date = clean.date.vec(q0_4_date))

## II. Harmonisation of columns headers + recoding of binary/text column when necessary

# A. Question "threats to the site" - Harmonisation with past data 
# Recoding the text column when there is no text entry but binary columns are filled out. 
col <- colnames(new_master_all_int)[grepl("f1_threats_to_the_site.", colnames(new_master_all_int))]

for (c in col){
  new_master_all_int <- new_master_all_int %>%
    mutate(f1_threats_to_the_site = ifelse(!!sym(c) == 1 & is.na(f1_threats_to_the_site),
                                                                  paste(f1_threats_to_the_site, gsub("f1_threats_to_the_site.", "", c), sep = " "), f1_threats_to_the_site))
}

# Mutating binary columns for past responses that are only in text column
# This will recode the binary columns if they are as NA and the text column as a text entry matching the column name.
## the line below checks if this is necessary or not
test.recode.f1.threat <- new_master_all_int %>%
  select(matches("f1_")) %>% select(-matches("war|other|conflict_related_incidents")) %>%
  filter_all(any_vars(is.na(.))) %>% filter(!is.na(f1_threats_to_the_site))
if (nrow(test.recode.f1.threat)>0) stop("Some text responses are not reflected accordingly in the binary columns. Uncomment and run the lines below.")
## Uncomment the lines below if test.recode.f1.threat has more at least one row

# col.tool <- choices %>% filter(list_name %in% "qf1234") %>% pull(name)               # get all choices from both tools to mutate corresponding binary columns
# 
# for (c in col.tool){
#   col.name <- paste0("f1_threats_to_the_site.", c)
#   if (!col.name %in% colnames(new_master_all_int)){ ## If the column is not existing, recode using text entries only
#     new_master_all_int <- new_master_all_int %>%
#       mutate(!!sym(col.name) := ifelse(is.na(f1_threats_to_the_site), NA, ifelse(grepl(c, f1_threats_to_the_site), 1, 0)),
#              .after = "f1_threats_to_the_site")
#   } else {
#     new_master_all_int <- new_master_all_int %>%
#       mutate(!!sym(col.name) := ifelse(!is.na(!!sym(col.name)),!!sym(col.name),ifelse(grepl(c, f1_threats_to_the_site), 1, 0)),
#              .after = "f1_threats_to_the_site")
#   }
# }

# recode binary as NA when f1_threats_to_the_site is na
test.binary.recode.f1 <- new_master_all_int %>%
  select(matches("f1_")) %>% filter(is.na(f1_threats_to_the_site))
if (sum(!is.na(test.binary.recode.f1))>0) stop("Some binaries are not coded as NA but the text column is coded as NA. Uncomment and run the lines below.")
## If test.binary.recode.f1 has at least one row, uncomment the lines below:
# new_master_all_int <- new_master_all_int %>%
#   mutate_at(vars(matches("f1_threats_to_the_site.")), ~ifelse(is.na(f1_threats_to_the_site), NA, .))

# result <- new_master_all_int %>% select(matches("f1_"))

# cleaning some issues with NA and past tool's different choice name
new_master_all_int <- new_master_all_int %>%
  mutate(f1_threats_to_the_site = tolower(f1_threats_to_the_site),
         f1_threats_to_the_site = gsub("f1_threats_to_the_site.|\\,|f1_|\\b_\\b", "", gsub("\\bna\\b", "", f1_threats_to_the_site)),
         f1_threats_to_the_site = gsub("conflict-related incidents", "conflict_related_incidents", f1_threats_to_the_site),
         f1_threats_to_the_site = gsub("infectious_diseasess|infections disease|site.infectious_diseases", "infectious_diseases", f1_threats_to_the_site),
         f1_threats_to_the_site = gsub("infectious_diseases_", "infectious_diseases ", f1_threats_to_the_site),
         f1_threats_to_the_site = gsub("floods", "flooding ", f1_threats_to_the_site),
         f1_threats_to_the_site = gsub("friction_between_community_members", "friction_between_communities", f1_threats_to_the_site),
         f1_threats_to_the_site = gsub("fire_related_incidents_", "fire_related_incidents ", f1_threats_to_the_site),
         f1_threats_to_the_site = gsub("water contamination", "water_contamination", f1_threats_to_the_site),
         f1_threats_to_the_site = gsub("\\bther\\b", "other", f1_threats_to_the_site),
         f1_threats_to_the_site = gsub("^ | $", "", f1_threats_to_the_site),
         f1_threats_to_the_site = gsub("infectious_diseasess", "infectious_diseases", f1_threats_to_the_site)
         )

#########################################################TEST"####################################
# A. Question "c7_presence_of_particularly_vulnerable_groups" - Harmonisation with past data 
# Recoding the text column when there is no text entry but binary columns are filled out. 
col <- colnames(new_master_all_int)[grepl("c7_presence_of_particularly_vulnerable_groups.", colnames(new_master_all_int))]

for (c in col){
  new_master_all_int <- new_master_all_int %>%
    mutate(c7_presence_of_particularly_vulnerable_groups = ifelse(!!sym(c) == 1 & is.na(c7_presence_of_particularly_vulnerable_groups),
                                           paste(c7_presence_of_particularly_vulnerable_groups, gsub("c7_presence_of_particularly_vulnerable_groups.", "", c), sep = " "), c7_presence_of_particularly_vulnerable_groups))
}

## Checking if recoding of binary according to text column is needed
test.recode.binary.c7 <- new_master_all_int %>%
  select(matches("c7")) %>% select(-matches("marginalized_people|minorities")) %>%
  filter_all(any_vars(is.na(.))) %>% filter(!is.na(c7_presence_of_particularly_vulnerable_groups))
if (nrow(test.recode.binary.c7)>0) stop("Some text responses are not reflected accordingly in the binary columns. Uncomment and run the lines below.")
## if test.recode.binary.c7 has at least one row, uncomment the lines below to recode the binary columns

# Mutating binary columns for past responses that are only in text column
# col.tool <- choices %>% filter(list_name %in% "qc06R") %>% pull(name)               # get all choices from both tools to mutate corresponding binary columns
# 
# for (c in col.tool){
#   col.name <- paste0("c7_presence_of_particularly_vulnerable_groups.", c)
#   if (!col.name %in% colnames(new_master_all_int)){ ## If the column is not existing, recode using text entries only
#     new_master_all_int <- new_master_all_int %>%
#       mutate(!!sym(col.name) := ifelse(is.na(c7_presence_of_particularly_vulnerable_groups), NA, ifelse(grepl(c, c7_presence_of_particularly_vulnerable_groups), 1, 0)),
#              .after = "c7_presence_of_particularly_vulnerable_groups")
#   } else {
#     new_master_all_int <- new_master_all_int %>%
#       mutate(!!sym(col.name) := ifelse(!is.na(!!sym(col.name)),!!sym(col.name),ifelse(grepl(c, c7_presence_of_particularly_vulnerable_groups), 1, 0)),
#              .after = "c7_presence_of_particularly_vulnerable_groups")
#   }
# }

# checking if recoding of NA is needed for c7
test.binary.recode.c7 <- new_master_all_int %>%
  select(matches("c7_")) %>% filter(is.na(c7_presence_of_particularly_vulnerable_groups))
if (sum(!is.na(test.binary.recode.c7))!=0) stop("Some binaries are not coded as NA but the text column is coded as NA. Uncomment and run the line belows.")
## If test.binary.recode.f1 has at least one row, uncomment the lines below:

# # recode binary as NA when c7_presence_of_particularly_vulnerable_groups is na
# new_master_all_int <- new_master_all_int %>%
#   mutate_at(vars(matches("c7_presence_of_particularly_vulnerable_groups.")), ~ifelse(is.na(c7_presence_of_particularly_vulnerable_groups), NA, .))

# result <- new_master_all_int %>% select(matches("c7_"))

#######################################################TEST########################################
# for check, keep commented
# new_master_all_int$f1_threats_to_the_site %>% unique
# all.choices.data <- new_master_all_int$f1_threats_to_the_site %>% str_split(" ") %>% unlist %>% as.data.frame %>% setNames("response") %>% group_by(response) %>%
  # summarise(n())
# choices %>% filter(grepl("f1", list_name))

# B. Question Most common reason to leave place of origin - Harmonisation of  with past data
# d1_most_common_reason_idps_left_place_of_origin => security_concerns_conflict_explosives_lack_of_security_forces (V1) & war (V2) (new header: d1_most_common_reason_idps_left_place_of_origin.security_concerns_conflict_explosives_lack_of_security_forces_war)
# Note 1: In V2 there is no other field in the kobo tool? => non matching columns! Warning, might introduce a bias in analysis later
# Note 2: In masterlist, there is only the text column => recode the binary columns
 
# B.1. Recode the text response with the new header for the new category
new_master_all_int <- new_master_all_int %>%
  mutate(d1_most_common_reason_idps_left_place_of_origin = gsub("security_concerns_conflict_explosives_lack_of_security_forces|war", "security_concerns_conflict_explosives_lack_of_security_forces_war", d1_most_common_reason_idps_left_place_of_origin),
         f1_threats_to_the_site.conflict_related_incidents_war =  ifelse(!is.na(f1_threats_to_the_site.conflict_related_incidents), f1_threats_to_the_site.conflict_related_incidents, f1_threats_to_the_site.war) , .after="f1_threats_to_the_site") %>%
  select(-f1_threats_to_the_site.conflict_related_incidents, -f1_threats_to_the_site.war)

# B.2. Appropriately re-code the binary when there is a non na text response and the binary columns are NA.
col <- colnames(new_master_all_int)[grepl("d1_most_common_reason_idps_left_place_of_origin.", colnames(new_master_all_int))]
for (c in col){
  new_master_all_int <- new_master_all_int %>% 
    mutate(!!sym(c) := ifelse(!is.na(!!sym(c)), !!sym(c), 
                              ifelse(is.na(!!sym(c)) & (!is.na(d1_most_common_reason_idps_left_place_of_origin)) & grepl(gsub("d1_most_common_reason_idps_left_place_of_origin.", "", c), d1_most_common_reason_idps_left_place_of_origin), 1, 
                                     ifelse(is.na(!!sym(c)) & (!is.na(d1_most_common_reason_idps_left_place_of_origin)) & !grepl(gsub("d1_most_common_reason_idps_left_place_of_origin.", "", c), d1_most_common_reason_idps_left_place_of_origin), 0, NA)
                                     )), .after = "d1_most_common_reason_idps_left_place_of_origin") 
  }

# Recoding select one as select multiple, keeping NA when relevant
# Question Reason for leaving place of origin + primary cooking space question
new_master_all_int <- new_master_all_int %>%
  mutate_at(vars(matches("primary_cooking_space.")), function(x) ifelse(!is.na(x), x,
                                                                        ifelse(new_master_all_int$primary_cooking_space %>% tolower == "not_available", NA,
                                                                               ifelse(new_master_all_int$primary_cooking_space %>% tolower == gsub("primary_cooking_space.", "", deparse(substitute(x))), 1, 0)))) %>%
  mutate_at(vars(matches("d1_most_common_reason_idps_left_place_of_origin.")), function(x) ifelse(!is.na(x), x,
                                                                                                   ifelse(new_master_all_int$d1_most_common_reason_idps_left_place_of_origin %>% tolower == gsub("d1_most_common_reason_idps_left_place_of_origin.", "", deparse(substitute(x))), 1, 0)))

# C. Question vulnerable group - marginalized people (V1) with minorities (V2) + recoding when text column is NA with valid entries in binary columns
new_master_all_int <- new_master_all_int %>%
  mutate(c7_presence_of_particularly_vulnerable_groups = gsub("marginalized_people|minorities", "marginalized_people_minorities", c7_presence_of_particularly_vulnerable_groups),
         c7_presence_of_particularly_vulnerable_groups.marginalized_people_minorities = ifelse(!is.na(c7_presence_of_particularly_vulnerable_groups.marginalized_people), c7_presence_of_particularly_vulnerable_groups.marginalized_people, c7_presence_of_particularly_vulnerable_groups.minorities), .after = "c7_presence_of_particularly_vulnerable_groups") %>%
  select(-c7_presence_of_particularly_vulnerable_groups.marginalized_people, -c7_presence_of_particularly_vulnerable_groups.minorities)

# C.1. Replacing past all past string entries wrongly coded with the correct label from the kobo tool:
choices.vulnerable <- choices %>% filter(list_name=="qc06R") %>% select(-list_name, -`label::arabic`) # Extracting the label and names from question
pattern <- choices.vulnerable$`label::english`                                                        # Extract the labels in english to be replace
replacement <- choices.vulnerable$name                                                                # Extract the replacement correct name
new_master_all_int <- new_master_all_int %>%
  mutate(c7_presence_of_particularly_vulnerable_groups = str_replace_all(c7_presence_of_particularly_vulnerable_groups %>% tolower, setNames(replacement, tolower(pattern))),
         c7_presence_of_particularly_vulnerable_groups = gsub(",", "", c7_presence_of_particularly_vulnerable_groups),
         c7_presence_of_particularly_vulnerable_groups = gsub("lactating women|pregnant_and_lactating_women_elderly", "pregnant_and_lactating_women", c7_presence_of_particularly_vulnerable_groups),
         c7_presence_of_particularly_vulnerable_groups = gsub("person_with_disabilities|people with disabilities|persons with disabilities", "persons_with_disabilities", c7_presence_of_particularly_vulnerable_groups),
         c7_presence_of_particularly_vulnerable_groups = gsub("_condition\\b", "_conditions", c7_presence_of_particularly_vulnerable_groups),
         c7_presence_of_particularly_vulnerable_groups = gsub("\\.persons_with_chronic_diseases_serious_medical_conditions|\\bserious_medical_conditions\\b|serious medical condition|persons with chronic diseases serious medical conditions", "persons_with_chronic_diseases_serious_medical_conditions", c7_presence_of_particularly_vulnerable_groups),
         c7_presence_of_particularly_vulnerable_groups = gsub("unaccompanie_separated_children|unaccompaind_separated_children|unaccompanied separated children", "unaccompanied_separated_children", c7_presence_of_particularly_vulnerable_groups),
         c7_presence_of_particularly_vulnerable_groups = gsub("femal_headed_hh", "female_headed_hh", c7_presence_of_particularly_vulnerable_groups),
         c7_presence_of_particularly_vulnerable_groups = gsub("maginalized_people|marginalized_people|minorities", "marginalized_people_minorities", c7_presence_of_particularly_vulnerable_groups),
         c7_presence_of_particularly_vulnerable_groups = gsub("child headed hh", "child_headed_hh", c7_presence_of_particularly_vulnerable_groups),
         c7_presence_of_particularly_vulnerable_groups = gsub("female headed hh", "female_headed_hh", c7_presence_of_particularly_vulnerable_groups),
         c7_presence_of_particularly_vulnerable_groups = gsub(" people ", " ", c7_presence_of_particularly_vulnerable_groups)) 

# C.2. Recoding the text column when there is no text entry but binary columns are filled out. 
col <- colnames(new_master_all_int)[grepl("c7_presence_of_particularly_vulnerable_groups.", colnames(new_master_all_int))]
for (c in col){
  new_master_all_int <- new_master_all_int %>%
    mutate(c7_presence_of_particularly_vulnerable_groups = ifelse(!!sym(c) == 1 & (c7_presence_of_particularly_vulnerable_groups == "" | is.na(c7_presence_of_particularly_vulnerable_groups)),
                                                                  paste(c7_presence_of_particularly_vulnerable_groups, gsub("c7_presence_of_particularly_vulnerable_groups.", "", deparse(substitute(!!sym(c)))), sep = " "), c7_presence_of_particularly_vulnerable_groups))
}

# C.3. Cleaning the NA entries that were copied from the original text column
new_master_all_int <- new_master_all_int %>%
  mutate(c7_presence_of_particularly_vulnerable_groups = gsub("NA | NA | NA", "", c7_presence_of_particularly_vulnerable_groups))

# C.4. Check for multiple entries for each site, archive the oldest one and keep only latest one
new_master_all_int_2 <- new_master_all_int %>%
  group_by(a4_site_code) %>% mutate(n=n()) %>% mutate(max.date = max(q0_4_date)) 
check_dup <- new_master_all_int_2 %>%
  filter(n>1) %>% select(a4_site_code, everything()) %>% arrange(a4_site_code) 
  
## Keep only the latest entry for each site where there is duplicates
latest_entries <- check_dup %>% filter(q0_4_date==max.date)

# Log the past entries to keep track of it
past_entries <- check_dup %>% filter(q0_4_date!=max.date)
dir.create("output/masterlist/archive", showWarnings = F)
past_entries %>% write.xlsx("output/masterlist/archive/past_entries.xlsx")

# Newest entries 
# new_sites_surveys <- new_master_all_int_2 %>% filter(n==1, q0_4_date>as.POSIXct("2021-01-01 UTC"))

## bind together non duplicates with the clean duplicated entries
new_master_all_int <- new_master_all_int_2 %>% filter(n==1) %>% bind_rows(latest_entries)
new_master_all_int %>% write.xlsx(paste0("output/Internal/CCCM_SiteReporting_All Internal_", today, ".xlsx"))
# browseURL(paste0("output/Internal/CCCM_SiteReporting_All Internal", today, ".xlsx"))

## External masterlist
col.exclude <- c("a5_1_gps_longitude", "a5_2_gps_latitude")

new_master_all_ext <- new_master_all_int %>%
  select(-any_of(col.exclude))

new_master_all_ext %>% write.xlsx(paste0("output/external/new_master_all_ext_", today, ".xlsx"))

#### ARCHIVED CODE ## To be checked with christine 
## If anonymisation of the internal is sufficient just keep the lines above

### Internal Site ID code => will be moved in data cleaning script
#new_master_all_int$a4_site_name <- str_trim(new_master_all_int$a4_site_name, "both")
#new_master_all_int$a4_site_code <- external_choices$a4_site_code[match(new_master_all_int$a4_site_name, external_choices$a4_site_name)]
#new_master_all_int <- new_master_all_int[order(new_master_all_int$a4_site_code, new_master_all_int$a4_site_name, na.last = FALSE),]
#new_master_all_int <- new_master_all_int %>% mutate(a4_site_code = ifelse(is.na(a4_site_code) == TRUE, paste0(a2_district_code,"_",seq(16147, 5000)), paste0(a4_site_code)))
#new_master_all_int$a4_site_code

## Fix codes based on master list
## Trim leading and trailing spaces
#new_db$a4_site_name <- str_trim(new_db$a4_site_name)

## Replace the site ID with those in the master list (update)
## How to VlookUp in R: https://www.rforexcelusers.com/vlookup-in-r/

#(still needs to be adapted, does not add new Site IDs yet, for now manually inserted)

#new_master_all_int$master_list <- id_list$site_code[match(new_master_all_int$a4_site_name, id_list$site_name_en)]
#new_master_all_int <- new_master_all_int %>% mutate(id_check = ifelse(a4_site_code == master_list, a4_site_code, master_list)) %>%
  #mutate(a4_site_code_final = ifelse(is.na(id_check), a4_site_code, id_check))

#new_master_all_int$a4_site_code <- new_master_all_int$a4_site_code_final 

#new_master_all_int$master_list <- NULL
#new_master_all_int$id_check <- NULL
#new_master_all_int$a4_site_code_final <- NULL

# write.xlsx(new_master_all_int, paste0("./output/internal/CCCM_SiteReporting_All Internal (WithID)_",today,".xlsx"))
# 
# ################################## EXTERNAL #############################################################
# sitename.masterlist.ext.filename <- ""
# laste.external.v1.filename <- ""
# laste.external.v2.filename <- ""
# 
# ### Produce External Updated dataset ###
# master_all_ext <- read.xlsx("./data/CCCM_Site Reporting List_March 2021_ALL internal_incl. new site IDs.xlsx")
# 
# ### Load Last Cleaned files ###
# ## External
# last_external_v1 <- read.xlsx("./output/external/CCCM_SiteReporting_V1 External_2021-07-11.xlsx")
# last_external_v2 <- read.xlsx("./output/external/CCCM_SiteReporting_V2 External_2021-07-11.xlsx")
# 
# ## Create V1 and V2 variable
# last_external_v1$kobo_version <- "V1"
# last_external_v2$kobo_version <- "V2"
# 
# unique(last_external_v1$uuid %in% master_all_ext$uuid)
# unique(last_external_v2$uuid %in% master_all_ext$uuid)

# ## Take latest dataset and remove the duplicated entries using the master
# new_v1 <- anti_join(last_external_v1, master_all_ext, "uuid")
# new_v2 <- anti_join(last_external_v2, master_all_ext, "uuid")
# 
# new_ext <- plyr::rbind.fill(new_v1, new_v2)

## Take latest dataset and remove the duplicated "older" entries using the master
# new_v1 <- anti_join(last_external_v1, master_all_ext, "a4_site_code")
# new_v2 <- anti_join(last_external_v2, master_all_ext, "a4_site_code")
# 
# new_ext <- plyr::rbind.fill(new_v1, new_v2)
# 
# ## Append the unique new entries to the final Master ALL Internal
# new_master_all_ext <- plyr::rbind.fill(master_all_ext, new_ext)
# write.xlsx(new_master_all_ext, paste0("./output/external/CCCM_SiteReporting_All External (with some ID)_",today,".xlsx"))

### External Site ID code
#new_master_all_ext$a4_site_name <- str_trim(new_master_all_ext$a4_site_name, "both")
#new_master_all_ext$a4_site_code <- external_choices$a4_site_code[match(new_master_all_ext$a4_site_name, external_choices$a4_site_name)]
#new_master_all_ext <- new_master_all_ext[order(new_master_all_ext$a4_site_code, new_master_all_ext$a4_site_name, na.last = FALSE),]

#new_master_all_ext <- new_master_all_ext %>% mutate(a4_site_code = ifelse(is.na(a4_site_code) == TRUE, paste0(a2_district_code,"_",seq(1185,5000)), paste0(a4_site_code)))
#new_master_all_ext$a4_site_code

## Fix codes based on master list
## Trim leading and trailing spaces
#new_db$a4_site_name <- str_trim(new_db$a4_site_name)

## Replace the site ID with those in the master list (update)
## How to VlookUp in R: https://www.rforexcelusers.com/vlookup-in-r/
#new_master_all_int$master_list <- id_list$site_code[match(new_master_all_int$a4_site_name_NEW, id_list$site_name_en)]
#new_master_all_int <- new_master_all_int %>% mutate(id_check = ifelse(a4_site_code_NEW == master_list, a4_site_code_NEW, master_list)) %>%
  #mutate(a4_site_code_final = ifelse(is.na(id_check), a4_site_code_NEW, id_check))

#new_master_all_int$a4_site_code <- new_master_all_int$a4_site_code_final 

#new_master_all_int$master_list <- NULL
#new_master_all_int$id_check <- NULL
#new_master_all_int$a4_site_code_final <- NULL


#write.xlsx(new_master_all_ext, paste0("./output/external/CCCM_SiteReporting_All External (WithID)_",today,".xlsx"))

##################################### DASHBOARD (deleted) ######################################################################
