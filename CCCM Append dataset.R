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
id_list <- read.xlsx("./data/kobo/CCCM IDP Sites_site names and IDs_May 2021.xlsx")
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
                B1_CCCM_Pillars_existing.cccm_agency = B1_CCCM_Pillars_existing_on_s.site_management__cccm_agency)

new_v2 <- anti_join(last_internal_v2, master_all_int, "uuid") %>%
  dplyr::rename(B1_CCCM_Pillars_existing_on_s.site_administration = B1_CCCM_Pillars_existing_on_s.site_administration_SCMCHAIC,
                B1_CCCM_Pillars_existing.cccm_agency = B1_CCCM_Pillars_existing_on_s.site_supervision__cccm_agency)

# Column harmonisation check
order(colnames(new_v1)[grepl("c7", colnames(new_v1))]) %in% order(colnames(new_v2)[grepl("c7", colnames(new_v2))])

new_int <- plyr::rbind.fill(new_v1, new_v2) %>%
  setNames(gsub("B1_CCCM_Pillars_existing_on_s.", "B1_CCCM_Pillars_existing.", colnames(.))) %>%  # put existing instead of existings for some for harmonising check with Christine if there is a reason or if it could creates issues
  setNames(tolower(colnames(.)))                                                # Set names to lower cases to merge with master dataset

colnames(new_int)[grepl("c7", colnames(new_int))] 
colnames(master_all_int)[grepl("c7", colnames(master_all_int))]
test <- master_all_int %>%
  mutate(sum.na = rowSums(across(matches("c7_presence_of_particularly_vulnerable_groups"), ~is.na(.)))) %>%
  select(matches("c7_presence_of_particularly_vulnerable_groups|sum.na"))
test$sum.na %>% unique
test2 <- test %>% filter(sum.na==1)
# names(new_int)[names(new_int) == "B1_CCCM_Pillars_existing_on_s.site_administration__exu"] <- "B1_CCCM_Pillars_existing.site_administration"
# names(new_int)[names(new_int) == "B1_CCCM_Pillars_existing_on_s.site_management__cccm_agency"] <- "B1_CCCM_Pillars_existings.cccm_agency"
# names(new_int)[names(new_int) == "B1_CCCM_Pillars_existing_on_s.site_administration_SCMCHAIC"] <- "B1_CCCM_Pillars_existing.site_administration"
# names(new_int)[names(new_int) == "B1_CCCM_Pillars_existing_on_s.site_supervision__cccm_agency"] <- "B1_CCCM_Pillars_existings.cccm_agency"
# names(new_int)[names(new_int) == "b2_site_smc_agency_name"] <- "b4_site_cccm_agency_name"
# names(new_int)[names(new_int) == "b7_community_committee_in_place"] <- "b5_community_committee_in_place"

# c7_presence_of_particularly_vulnerable_groups.marginalized_people_minorities = c7_presence_of_particularly_vulnerable_groups.marginalized_people
# c7_presence_of_particularly_vulnerable_groups.marginalized_people_minorities = c7_presence_of_particularly_vulnerable_groups.minorities

## Append the unique new entries to the final Master ALL Internal (new IDs still need to be added manually)
new_master_all_int <- plyr::rbind.fill(master_all_int, new_int)

# write.xlsx(new_master_all_int, paste0("./output/internal/CCCM_SiteReporting_All Internal (with some ID)_",today,".xlsx"))

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
test <- new_master_all_int %>%
  mutate(c7_presence_of_particularly_vulnerable_groups.marginalized_people_minorities =
           c7_presence_of_particularly_vulnerable_groups.marginalized_people + c7_presence_of_particularly_vulnerable_groups.minorities) %>%
  # select(-c7_presence_of_particularly_vulnerable_groups.marginalized_people, -c7_presence_of_particularly_vulnerable_groups.minorities) %>%
  select(matches("c7_presence_of_particularly_vulnerable_groups"))

# # Need to recode the choices in text column as some are written with no _ as separator.
# choices.vulnerable <- choices %>% filter(list_name=="qc06R")
# vector <- test$c7_presence_of_particularly_vulnerable_groups
# pattern <- choices.vulnerable$label..english
# replacement <- choices.vulnerable$name
# test4 <- str_replace_all(vector, tolower(pattern) , replacement)
# vector <- test4[!grepl("_", test4) & !is.na(test4)]
# 
# # testing
# df <- data.frame(name = c("bonjour_maman", "salut_papa"),
#            english = c("Bonjour Maman", "Salut Papa"))
# data <- data.frame(response = c(sample(df$english, 100, replace = T)))
# 
# matched <- str_replace_all(data$response, df$english, df$name)
# matched <- mapply(gsub, df$english, df$name, data$response, USE.NAMES = F)


# Need to recode the minorities / marginalized as only one

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
