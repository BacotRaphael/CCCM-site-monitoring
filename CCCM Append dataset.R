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
choices <- read.csv("./data/kobo/choices.csv", check.names = T) %>% select(-X)
external_choices <- read.csv("./data/kobo/choices_external.csv", check.names = F)

external_choices <- filter(external_choices, external_choices$list_name == "sitename")
names(external_choices)[names(external_choices) == "name"] <- "a4_site_code"
names(external_choices)[names(external_choices) == "label::english"] <- "a4_site_name"

## Load site ID master list
id_list <- read.xlsx("./data/CCCM IDP Sites_site names and IDs_May 2021.xlsx")
#id_list$name <- str_trim(id_list$a4_site_name)

#################################### INTERNAL ##############################################################################

### Produce Internal Updated dataset ###
#### Internal Master file
master_all_int <- read.xlsx("./data/CCCM_Site Reporting List_March 2021_ALL internal_incl. new site IDs.xlsx")

### Load Last Cleaned files ###
## Internal
last_internal_v1 <- read.xlsx("./output/internal/CCCM_SiteReporting_V1 Internal_2021-05-19.xlsx")
last_internal_v2 <- read.xlsx("./output/internal/CCCM_SiteReporting_V2 Internal_2021-05-19.xlsx")

# last_internal_v1 <- last_internal_v1 %>%
#   setnames(old = c(""),
#            new = c(""))


## If only TRUE it means that no new values have been cleaned
unique(last_internal_v1$uuid %in% master_all_int$uuid)
unique(last_internal_v2$uuid %in% master_all_int$uuid)

## Take latest dataset and remove the duplicated entries using the master
new_v1 <- anti_join(last_internal_v1, master_all_int, "uuid") %>%
  dplyr::rename(B1_CCCM_Pillars_existing_on_s.site_administration = B1_CCCM_Pillars_existing_on_s.site_administration__exu,
                B1_CCCM_Pillars_existing.cccm_agency = B1_CCCM_Pillars_existing_on_s.site_management__cccm_agency,
                d1_most_common_reason_idps_left_place_of_origin.security_concerns_conflict_explosives_lack_of_security_forces_war = d1_most_common_reason_idps_left_place_of_origin.security_concerns_conflict_explosives_lack_of_security_forces)

new_v2 <- anti_join(last_internal_v2, master_all_int, "uuid") %>%
  dplyr::rename(B1_CCCM_Pillars_existing_on_s.site_administration = B1_CCCM_Pillars_existing_on_s.site_administration_SCMCHAIC,
                B1_CCCM_Pillars_existing.cccm_agency = B1_CCCM_Pillars_existing_on_s.site_supervision__cccm_agency,
                d1_most_common_reason_idps_left_place_of_origin.security_concerns_conflict_explosives_lack_of_security_forces_war = d1_most_common_reason_idps_left_place_of_origin.war)

# Column harmonisation check
order(colnames(new_v2)[grepl("f1", colnames(new_v2))]) %in% order(colnames(new_v1)[grepl("f1", colnames(new_v1))])

new_int <- plyr::rbind.fill(new_v1, new_v2) %>%
  setNames(tolower(colnames(.))) %>%                                                                  # Set names to lower cases to merge with master dataset
  setNames(gsub("b1_CCCM_Pillars_existing_on_s.", "b1_CCCM_Pillars_existing.", colnames(.)))          # put existing instead of existings for some for harmonising check with Christine if there is a reason or if it could creates issues

## Harmonisation of columns
# To delete => test for the d1 question on reason leaving place of origin
# colnames(new_int)[grepl("d1", colnames(new_int))] 
# colnames(master_all_int)[grepl("d1", colnames(master_all_int))]
# test <- master_all_int$d1_most_common_reason_idps_left_place_of_origin %>% unique
# lapply(test, function(x) grepl(x, colnames(new_int)[grepl("d1", colnames(new_int))])) # checking that paste entries match column names
# test <- master_all_int %>%
#   mutate(sum.na = rowSums(across(matches("d1_"), ~is.na(.)))) %>%
#   select(matches("d1_|sum.na"))

## Append the unique new entries to the final Master ALL Internal (new IDs still need to be added manually)
new_master_all_int <- plyr::rbind.fill(master_all_int, new_int)
# write.xlsx(new_master_all_int, paste0("./output/internal/CCCM_SiteReporting_All Internal (with some ID)_",today,".xlsx"))

# Recoding threats to the site columns and harmonising with past data 
# colnames(new_v1)[grepl("f1", colnames(new_v1))]
# colnames(new_v2)[grepl("f1", colnames(new_v2))]
# colnames(master_all_int)[grepl("f1", colnames(master_all_int))]
# colnames(new_master_all_int)[grepl("f1", colnames(new_master_all_int))]
# new_master_all_int <- new_master_all_int %>%
#   mutate(sum.na = rowSums(across(matches("f1_threats_to_the_site"), ~is.na(.)))) %>%
#   select(matches("f1_threats_to_the_site|sum.na"))

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

# new_master_all_int$f1_threats_to_the_site %>% unique
# all.choices.data <- new_master_all_int$f1_threats_to_the_site %>% str_split(" ") %>% unlist %>% as.data.frame %>% setNames("response") %>% group_by(response) %>%
#   summarise(n())
# choices %>% filter(grepl("f1", list_name))

# d1_most_common_reason_idps_left_place_of_origin - Recoding resons idps left place of origin 
# In V2 there is no other field in the kobo tool? => non matching columns
# In masterlist, there is only the text column => recode the binary columns
# d1_most_common_reason_idps_left_place_of_origin: security_concerns_conflict_explosives_lack_of_security_forces (V1) & war (V2) (new header: d1_most_common_reason_idps_left_place_of_origin.security_concerns_conflict_explosives_lack_of_security_forces_war)

# 1. Recode the text response with the new header for the new category
# test <- new_master_all_int %>%
#   mutate(sum.na = rowSums(across(matches("d1"), ~is.na(.)))) %>%
#   select(matches("d1|sum.na"))
new_master_all_int <- new_master_all_int %>%
  mutate(d1_most_common_reason_idps_left_place_of_origin = gsub("security_concerns_conflict_explosives_lack_of_security_forces|war", "security_concerns_conflict_explosives_lack_of_security_forces_war", d1_most_common_reason_idps_left_place_of_origin),
         f1_threats_to_the_site.conflict_related_incidents_war = f1_threats_to_the_site.conflict_related_incidents + f1_threats_to_the_site.war) %>%
  select(-f1_threats_to_the_site.conflict_related_incidents, -f1_threats_to_the_site.war)

# 2. Appropriately recode the binary when there is a non na text response and the binary columns are NA.
col <- colnames(test)[grepl("d1_most_common_reason_idps_left_place_of_origin.", colnames(test))]
for (c in col){
  test <- test %>% 
    mutate(!!sym(c) := ifelse(!is.na(!!sym(c)), !!sym(c), 
                              ifelse(is.na(!!sym(c)) & (!is.na(d1_most_common_reason_idps_left_place_of_origin)) & grepl(gsub("d1_most_common_reason_idps_left_place_of_origin.", "", c), d1_most_common_reason_idps_left_place_of_origin), 1, 
                                     ifelse(is.na(!!sym(c)) & (!is.na(d1_most_common_reason_idps_left_place_of_origin)) & !grepl(gsub("d1_most_common_reason_idps_left_place_of_origin.", "", c), d1_most_common_reason_idps_left_place_of_origin), 0, NA)))) 
  }
# check <- test %>% filter(sum.na==10) 

# Recoding select one text entries into the corresponding binary columns, keeping NA when relevant
# For primary cooking space question, and reason for leaving place of origin

# Check the number of NAs across columns 
# test <- new_master_all_int %>%
#   mutate(sum.na = rowSums(across(matches("d1_most_common_reason_idps_left_place_of_origin"), ~is.na(.))))
# test2 <- test %>%
#   select(matches("d1_most_common_reason_idps_left_place_of_origin|sum.na")) %>%
#   filter(!d1_most_common_reason_idps_left_place_of_origin %in% "not_available")
#   
#   filter(sum.na == 10)    # If you look at safe_cooking_practices column, we don't have entry that are not coded so far.
# test$d1_most_common_reason_idps_left_place_of_origin %>% unique

new_master_all_int <- new_master_all_int %>%
  mutate_at(vars(matches("primary_cooking_space.")), function(x) ifelse(!is.na(x), x,
                                                                        ifelse(new_master_all_int$primary_cooking_space %>% tolower == "not_available", NA,
                                                                               ifelse(new_master_all_int$primary_cooking_space %>% tolower == gsub("primary_cooking_space.", "", deparse(substitute(x))), 1, 0)))) %>%
  mutate_at(vars(matches("d1_most_common_reason_idps_left_place_of_origin.")), function(x) ifelse(!is.na(x), x,
                                                                                                   ifelse(new_master_all_int$d1_most_common_reason_idps_left_place_of_origin %>% tolower == gsub("d1_most_common_reason_idps_left_place_of_origin.", "", deparse(substitute(x))), 1, 0)))

# Merging marginalized people (V1) with minorities (V2) + recoding when text column is NA with valid entries in binary columns
new_master_all_int <- new_master_all_int %>%
  mutate(c7_presence_of_particularly_vulnerable_groups.marginalized_people_minorities =
           c7_presence_of_particularly_vulnerable_groups.marginalized_people + c7_presence_of_particularly_vulnerable_groups.minorities, .after = "c7_presence_of_particularly_vulnerable_groups") %>%
  select(-c7_presence_of_particularly_vulnerable_groups.marginalized_people, -c7_presence_of_particularly_vulnerable_groups.minorities)

# Replacing past all past string entries wrongly coded with the correct label from the kobo tool:
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
# d <- new_master_all_int$c7_presence_of_particularly_vulnerable_groups %>% str_split(" ") %>% unlist %>% as.data.frame %>% setNames("response") %>%
#   group_by(response) %>% summarise(n())
# e <- unique(new_master_all_int$c7_presence_of_particularly_vulnerable_groups)
# test <- new_master_all_int %>% filter(!grepl(replacement, c7_presence_of_particularly_vulnerable_groups)) %>%
#   select(matches("c7_presence_of_particularly_vulnerable_groups"))
# 
# test <- new_master_all_int %>%
#   mutate(sum.na = rowSums(across(matches("c7_presence_of_particularly_vulnerable_groups"), ~is.na(.)))) %>%
#   select(matches("c7_presence_of_particularly_vulnerable_groups|sum.na")) %>% filter(sum.na==1)

# Recoding the text column when there is no text entry but binary columns are filled out. 
col <- colnames(new_master_all_int)[grepl("c7_presence_of_particularly_vulnerable_groups.", colnames(new_master_all_int))]
for (c in col){
  new_master_all_int <- new_master_all_int %>%
    mutate(c7_presence_of_particularly_vulnerable_groups = ifelse(!!sym(c) == 1 & (c7_presence_of_particularly_vulnerable_groups == "" | is.na(c7_presence_of_particularly_vulnerable_groups)),
                                                                  paste(c7_presence_of_particularly_vulnerable_groups, gsub("c7_presence_of_particularly_vulnerable_groups.", "", deparse(substitute(!!sym(c)))), sep = " "), c7_presence_of_particularly_vulnerable_groups))
}

# Cleaning the NA entries that were copied from the original text column
new_master_all_int <- new_master_all_int %>%
  mutate(c7_presence_of_particularly_vulnerable_groups = gsub("NA | NA | NA", "", c7_presence_of_particularly_vulnerable_groups))
# final.check <- new_master_all_int %>% filter(sum.na == 1)

# Relocate columns => To be done


### Internal Site ID code
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
