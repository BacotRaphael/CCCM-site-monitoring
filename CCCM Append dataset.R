### Script that takes V1 and V2 files and appends them to Masters
# REACH Yemen - christine.pfeffer@reach-initiative.org
# 05/17/2021

## Useful resource
# https://stackoverflow.com/questions/3171426/compare-two-data-frames-to-find-the-rows-in-data-frame-1-that-are-not-present-in

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
today <- Sys.Date()

require(tidyverse)
require(openxlsx)

source("./R/moveme.R")

### Load kobo choices file to vlook up old site codes
survey_v1 <- read.xlsx("data/CCCM_Site_Reporting_Kobo_tool_(V1)_12042021.xlsx", sheet = "survey")
survey_v2 <- read.xlsx("data/CCCM_Site_Reporting_Kobo_tool_(V2)_12042021.xlsx", sheet = "survey")
choices_v1 <- read.xlsx("data/CCCM_Site_Reporting_Kobo_tool_(V2)_12042021.xlsx", sheet = "choices")
choices_v2 <- read.xlsx("data/CCCM_Site_Reporting_Kobo_tool_(V2)_12042021.xlsx", sheet = "choices")
choices <- read.csv("./data/kobo/choices.csv", check.names = T) %>% select(-X)
external_choicesv1 <- read.xlsx("data/CCCM_Site_Reporting_Kobo_tool_(V1)_12042021.xlsx", sheet = "external_choices") %>% filter(list_name == "sitename")
external_choicesv2 <- read.xlsx("data/CCCM_Site_Reporting_Kobo_tool_(V2)_12042021.xlsx", sheet = "external_choices") %>% filter(list_name == "sitename")

# external_choices <- filter(external_choices, external_choices$list_name == "sitename")
# names(external_choices)[names(external_choices) == "name"] <- "a4_site_code"
# names(external_choices)[names(external_choices) == "label::english"] <- "a4_site_name"

## Load site ID master list
id_list <- read.xlsx("./data/CCCM IDP Sites_site names and IDs_May 2021.xlsx")
#id_list$name <- str_trim(id_list$a4_site_name)

#################################### INTERNAL ##############################################################################

### Produce Internal Updated dataset ###

# I. Import all necessary files with renaming of similar column accross V1 and V2 when necessary

#### Internal Master file
master_all_int <- read.xlsx("./data/CCCM_Site Reporting List_March 2021_ALL internal_incl. new site IDs.xlsx") %>%
  dplyr::rename(b4_site_cccm_agency_name = b2_site_smc_agency_name)

### Load Last Cleaned files ###
## Internal
last_internal_v1 <- read.xlsx("./output/internal/CCCM_SiteReporting_V1 Internal_2021-05-19.xlsx")
last_internal_v2 <- read.xlsx("./output/internal/CCCM_SiteReporting_V2 Internal_2021-05-19.xlsx")

## If only TRUE it means that no new values have been cleaned
unique(last_internal_v1$uuid %in% master_all_int$uuid)
unique(last_internal_v2$uuid %in% master_all_int$uuid)

## Take latest dataset and remove the duplicated entries using the master
new_v1 <- anti_join(last_internal_v1, master_all_int, "uuid") %>%
  dplyr::rename(
    # B1_CCCM_Pillars_existing_on_s.site_administration = B1_CCCM_Pillars_existing_on_s.site_administration__exu,       # check with christine that the idea is to recode the binary into a text question
    B1_CCCM_Pillars_existing_on_s.site_management_supervision = B1_CCCM_Pillars_existing_on_s.site_management__cccm_agency,
    d1_most_common_reason_idps_left_place_of_origin.security_concerns_conflict_explosives_lack_of_security_forces_war = d1_most_common_reason_idps_left_place_of_origin.security_concerns_conflict_explosives_lack_of_security_forces)

new_v2 <- anti_join(last_internal_v2, master_all_int, "uuid") %>%
  dplyr::rename(
    # B1_CCCM_Pillars_existing_on_s.site_administration = B1_CCCM_Pillars_existing_on_s.site_administration_SCMCHAIC,   # check with christine that the idea is to recode the binary into a text question
    B1_CCCM_Pillars_existing_on_s.site_management_supervision = B1_CCCM_Pillars_existing_on_s.site_supervision__cccm_agency,
    d1_most_common_reason_idps_left_place_of_origin.security_concerns_conflict_explosives_lack_of_security_forces_war = d1_most_common_reason_idps_left_place_of_origin.war) 

new_int <- plyr::rbind.fill(new_v1, new_v2) %>%
  setNames(tolower(colnames(.)))  %>%                                              # Set names to lower cases to merge with master dataset
  mutate(b1_CCCM_Pillars_existing_on_s.site_administration = ifelse(b1_cccm_pillars_existing_on_s.site_administration__exu == 1, "exu",
                                                                    ifelse(b1_cccm_pillars_existing_on_s.site_administration_scmchaic == 1, "scmchaic",
                                                                           NA)))

## Append the unique new entries to the final Master ALL Internal (new IDs still need to be added manually)
new_master_all_int <- plyr::rbind.fill(master_all_int, new_int)
# write.xlsx(new_master_all_int, paste0("./output/internal/CCCM_SiteReporting_All Internal (with some ID)_",today,".xlsx"))

## II. Harmonisation of columns headers + recoding of binary/text column when necessary

# A. Question "threats to the site" - Harmonisation with past data 
# Recoding the text column when there is no text entry but binary columns are filled out. 
col <- colnames(new_master_all_int)[grepl("f1_threats_to_the_site.", colnames(new_master_all_int))]
for (c in col){
  new_master_all_int <- new_master_all_int %>%
    mutate(f1_threats_to_the_site = ifelse(!!sym(c) == 1 & is.na(f1_threats_to_the_site),
                                                                  paste(f1_threats_to_the_site, gsub("f1_threats_to_the_site.", "", c), sep = " "), f1_threats_to_the_site))
}

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
# for check, erase later
# new_master_all_int$f1_threats_to_the_site %>% unique
# all.choices.data <- new_master_all_int$f1_threats_to_the_site %>% str_split(" ") %>% unlist %>% as.data.frame %>% setNames("response") %>% group_by(response) %>%
#   summarise(n())
# choices %>% filter(grepl("f1", list_name))

# B. Question Most common reason to leave place of origin - Harmonisation of  with past data
# d1_most_common_reason_idps_left_place_of_origin => security_concerns_conflict_explosives_lack_of_security_forces (V1) & war (V2) (new header: d1_most_common_reason_idps_left_place_of_origin.security_concerns_conflict_explosives_lack_of_security_forces_war)
# Note 1: In V2 there is no other field in the kobo tool? => non matching columns! Warning, might introduce a bias in analysis later
# Note 2: In masterlist, there is only the text column => recode the binary columns

# B.1. Recode the text response with the new header for the new category
new_master_all_int <- new_master_all_int %>%
  mutate(d1_most_common_reason_idps_left_place_of_origin = gsub("security_concerns_conflict_explosives_lack_of_security_forces|war", "security_concerns_conflict_explosives_lack_of_security_forces_war", d1_most_common_reason_idps_left_place_of_origin),
         f1_threats_to_the_site.conflict_related_incidents_war = f1_threats_to_the_site.conflict_related_incidents + f1_threats_to_the_site.war) %>%
  select(-f1_threats_to_the_site.conflict_related_incidents, -f1_threats_to_the_site.war)

# B.2. Appropriately recode the binary when there is a non na text response and the binary columns are NA.
col <- colnames(new_master_all_int)[grepl("d1_most_common_reason_idps_left_place_of_origin.", colnames(new_master_all_int))]
for (c in col){
  new_master_all_int <- new_master_all_int %>% 
    mutate(!!sym(c) := ifelse(!is.na(!!sym(c)), !!sym(c), 
                              ifelse(is.na(!!sym(c)) & (!is.na(d1_most_common_reason_idps_left_place_of_origin)) & grepl(gsub("d1_most_common_reason_idps_left_place_of_origin.", "", c), d1_most_common_reason_idps_left_place_of_origin), 1, 
                                     ifelse(is.na(!!sym(c)) & (!is.na(d1_most_common_reason_idps_left_place_of_origin)) & !grepl(gsub("d1_most_common_reason_idps_left_place_of_origin.", "", c), d1_most_common_reason_idps_left_place_of_origin), 0, NA)))) 
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
  mutate(c7_presence_of_particularly_vulnerable_groups.marginalized_people_minorities =
           c7_presence_of_particularly_vulnerable_groups.marginalized_people + c7_presence_of_particularly_vulnerable_groups.minorities, .after = "c7_presence_of_particularly_vulnerable_groups") %>%
  select(-c7_presence_of_particularly_vulnerable_groups.marginalized_people, -c7_presence_of_particularly_vulnerable_groups.minorities)

# C.1. Replacing past all past string entries wrongly coded with the correct label from the kobo tool:
choices.vulnerable <- choices %>% filter(list_name=="qc06R") %>% select(-list_name, -label..arabic) # Extracting the label and names from question
pattern <- choices.vulnerable$label..english                                    # Extract the labels in english to be replace
replacement <- choices.vulnerable$name                                          # Extract the replacement correct name
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

# Relocate columns => Check if necessary

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

#write.xlsx(new_master_all_int, paste0("./output/internal/CCCM_SiteReporting_All Internal (WithID)_",today,".xlsx"))

################################## EXTERNAL #############################################################

### Produce External Updated dataset ###
master_all_ext <- read.xlsx("./data/CCCM_Site Reporting List_2020_internal.xlsx")

### Load Last Cleaned files ###
## External
last_external_v1 <- read.xlsx("./output/external/CCCM_SiteReporting_V1 External_2021-05-18.xlsx")
last_external_v2 <- read.xlsx("./output/external/CCCM_SiteReporting_V2 External_2021-05-18.xlsx")

## Create V1 and V2 variable
last_external_v1$kobo_version <- "V1"
last_external_v2$kobo_version <- "V2"

unique(last_external_v1$uuid %in% master_all_ext$uuid)
unique(last_external_v2$uuid %in% master_all_ext$uuid)

## Take latest dataset and remove the duplicated entries using the master
new_v1 <- anti_join(last_external_v1, master_all_ext, "uuid")
new_v2 <- anti_join(last_external_v2, master_all_ext, "uuid")

new_ext <- plyr::rbind.fill(new_v1, new_v2)

## Append the unique new entries to the final Master ALL Internal
new_master_all_ext <- plyr::rbind.fill(master_all_ext, new_ext)
write.xlsx(new_master_all_ext, paste0("./output/external/CCCM_SiteReporting_All External (with some ID)_",today,".xlsx"))


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
