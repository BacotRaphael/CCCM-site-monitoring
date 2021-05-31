# CCCM Site Monitoring Tool - Data Merge Script
# REACH Yemen - christine.pfeffer@reach-initiative.org
# Site-level datamerge EN/AR
# 05/31/2021 - updated by Raphael Bacot

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
today <- Sys.Date()

## Install packaged
# install.packages("tidyverse")
# install.packages("stringr")

require(tidyverse)
require(stringr)
require(openxlsx)

## Load sources
source("./R/cleanHead.R")

#### EN site-level datamerge

## Load choice sheet
choices <- read.csv("./data/kobo/choices.csv")

####################### V1 Dataset ######################


## Load cleaned V2 data
response <- read.xlsx("./output/Internal/CCCM_SiteReporting_V1 Internal_2021-05-19.xlsx")
#response <- cleanHead(response)
#response <- cleanHead(response)

## Filter out sites that are useless
#response <- response %>% filter(check == 1 & check.2 == 1)

## Step 1: Data Merge - dots
lookup <- data.frame(colour = c("inadequate", "non_existent", "adequate"), path = c("./merge/red.png", "./merge/grey.png", "./merge/green.png"))


dots_df <- response %>% select("rrm_distributions":"waste_disposal_services")
dots_df[is.na(dots_df)] <- "not_applicable"


dots_merge <- as.data.frame(lapply(dots_df, function(col) lookup$path[match(col, lookup$colour)]))
colnames(dots_merge) <- paste0("@", colnames(dots_merge))

#write.csv(dots_merge, "./output/test_image_datamerge.csv", row.names = F)

## Step 2: Data Merge - rest pf the stuff
data_rename <- response %>% select("b4_site_cccm_agency_name", "c2_landowner", "c1_type_of_site", "Primary_cooking_modality", "additional_fire_safety_measures",
                                  "c3_tenancy_agreement_for_at_least_6_months", "b7_community_committee_in_place", "d1_most_common_reason_idps_left_place_of_origin",
                                  "a8_population_groups_other_than_idps_in_site_select_all_applicable", "d3_most_common_intention_in_next_three_months", "c4_general_market_in_site_close_proximity",
                                  "d2_1_most_common_district_of_idp_origin", "d2_2_second_most_common_district_of_idp_origin", "d2_3_third_most_common_district_of_idp_origin", 
                                  "c6_electricity_solar_power_available_in_site", "c5_fuel_available_in_site_close_proximity", "c8_primary_water_source", "c9_primary_shelter_type",
                                  "c10_primary_latrine_type", "i1_top_priority_need", "i2_second_priority_need", "i3_third_priority_need", starts_with("psp_"), starts_with("if_ngo_"),
                                  "c7_presence_of_particularly_vulnerable_groups.unaccompanied_separated_children", "c7_presence_of_particularly_vulnerable_groups.child_headed_hh", "c7_presence_of_particularly_vulnerable_groups.elderly",
                                  "c7_presence_of_particularly_vulnerable_groups.female_headed_hh", "c7_presence_of_particularly_vulnerable_groups.marginalized_people", "c7_presence_of_particularly_vulnerable_groups.persons_with_disabilities",
                                  "c7_presence_of_particularly_vulnerable_groups.persons_with_chronic_diseases_serious_medical_conditions", "c7_presence_of_particularly_vulnerable_groups.pregnant_and_lactating_women",
                                  "f1_threats_to_the_site.flooding", "f1_threats_to_the_site.eviction", "f1_threats_to_the_site.infectious_diseases", "f1_threats_to_the_site.friction_between_communities",
                                  "f1_threats_to_the_site.water_contamination", "f1_threats_to_the_site.conflict_related_incidents", "f1_threats_to_the_site.fire_related_incidents")

data_rename <- data_rename %>% select(-c(ends_with("_other")))

data_rename2 <- data_rename
data_rename2[] <- choices$label..english[match(unlist(data_rename2), choices$name)]

### Rename the variables to keep the data merge template intact
data_norename <- response %>% select("q0_4_date","a1_governorate_name", "a2_district_name", "a3_sub_district_name", "a4_site_name", "a6_site_occupation_date_dd_mm_yy", "a7_site_population_individual", "a7_site_population_hh")

## Add site ID (still needs to be done - for now VLOOKUP in Excel)

## Combine cooking space answers (new, working)

firstup <- function(x){
  substr(x, 1, 1) <- toupper(substring(x, 1, 1))
  return(x)
}

data_rename3 <- response %>%
  mutate(Primary_cooking_space = gsub("_", " ", gsub(" " , ", ", firstup(Primary_cooking_space))))

## Combine safe cooking practices answers (new)

data_rename4 <- response %>%
  mutate(Safe_cooking_practices = gsub(" ", ",", gsub("_" , " ", firstup(Safe_cooking_practices))))


## Step 3: merge into one dataset
data_merge <- cbind(data_rename2, data_norename, data_rename4, dots_merge)

#data_norename$a8_population_groups_other_than_idps_in_site_select_all_applicable <- str_replace_all(data_norename$a8_population_groups_other_than_idps_in_site_select_all_applicable, pattern = " ", replacement = " - ")
#data_norename$a8_population_groups_other_than_idps_in_site_select_all_applicable <- str_replace_all(data_norename$a8_population_groups_other_than_idps_in_site_select_all_applicable, pattern = "_", replacement = " ")
#data_norename$a4_site_code <- str_replace_all(data_norename$a4_site_code, pattern = "_", replacement = " - ")
  

## Step 4: Add maps and save as csv
#data_merge$a4_site_name <- str_trim(data_merge$a4_site_name, "both")
data_merge$`@maps` <- paste0("./maps/YEM_CCCM__",response$a3_sub_district_code,".pdf")

##Output file

#write.csv(data_merge, paste0("./merge/cccm_site-level_merge_en_v1_",today,".csv"), row.names = F)                     
browseURL(paste0("./merge/cccm_site-level_merge_en_v1_",today,".csv"))


####################### V2 Dataset ######################


## Load cleaned V2 data (new)
response <- read.xlsx("./output/Internal/CCCM_SiteReporting_V2 Internal_2021-05-19.xlsx")
#response <- cleanHead(response)
#response <- cleanHead(response)

## Filter out sites that are useless
#response <- response %>% filter(check == 1 & check.2 == 1)

## Step 1: Data Merge - dots
lookup <- data.frame(colour = c("inadequate", "non_existent", "adequate"), path = c("./merge/red.png", "./merge/grey.png", "./merge/green.png"))


dots_df <- response %>% select("rrm_distributions":"waste_disposal_services")
dots_df[is.na(dots_df)] <- "not_applicable"


dots_merge <- as.data.frame(lapply(dots_df, function(col) lookup$path[match(col, lookup$colour)]))
colnames(dots_merge) <- paste0("@", colnames(dots_merge))

#write.csv(dots_merge, "./output/test_image_datamerge.csv", row.names = F)

## Step 2: Data Merge - rest pf the stuff 
data_rename <- response %>% select("b4_site_cccm_agency_name", "c2_landowner", "c1_type_of_site", "Primary_cooking_modality", "Primary_cooking_space", "Safe_cooking_practices", "additional_fire_safety_measures",
                                   "c3_tenancy_agreement_for_at_least_6_months", "b7_community_committee_in_place", "d1_most_common_reason_idps_left_place_of_origin",
                                   "a8_population_groups_other_than_idps_in_site_select_all_applicable", "d3_most_common_intention_in_next_three_months", "c4_general_market_in_site_close_proximity",
                                   "d2_1_most_common_district_of_idp_origin", "d2_2_second_most_common_district_of_idp_origin", "d2_3_third_most_common_district_of_idp_origin", 
                                   "c6_electricity_solar_power_available_in_site", "c5_fuel_available_in_site_close_proximity", "c8_primary_water_source", "c9_primary_shelter_type",
                                   "c10_primary_latrine_type", "i1_top_priority_need", "i2_second_priority_need", "i3_third_priority_need", starts_with("psp_"), starts_with("if_ngo_"),
                                   "c7_presence_of_particularly_vulnerable_groups.unaccompanied_separated_children", "c7_presence_of_particularly_vulnerable_groups.child_headed_hh", "c7_presence_of_particularly_vulnerable_groups.elderly",
                                   "c7_presence_of_particularly_vulnerable_groups.female_headed_hh", "c7_presence_of_particularly_vulnerable_groups.minorities", "c7_presence_of_particularly_vulnerable_groups.persons_with_disabilities",
                                   "c7_presence_of_particularly_vulnerable_groups.persons_with_chronic_diseases_serious_medical_conditions", "c7_presence_of_particularly_vulnerable_groups.pregnant_and_lactating_women",
                                   "f1_threats_to_the_site.flooding", "f1_threats_to_the_site.eviction", "f1_threats_to_the_site.infectious_diseases", "f1_threats_to_the_site.friction_between_communities",
                                   "f1_threats_to_the_site.water_contamination", "f1_threats_to_the_site.conflict_related_incidents", "f1_threats_to_the_site.fire_related_incidents")

data_rename <- data_rename %>% select(-c(ends_with("_other")))

data_rename2 <- data_rename
data_rename2[] <- choices$label..english[match(unlist(data_rename2), choices$name)]

### Rename the variables to keep the data merge template intact
data_norename <- response %>% select("q0_4_date","a1_governorate_name", "a2_district_name", "a3_sub_district_name", "a4_site_name", "a6_site_occupation_date_dd_mm_yy", "a7_site_population_individual", "a7_site_population_hh")


## Combine cooking space answers (new, working)

firstup <- function(x){
  substr(x, 1, 1) <- toupper(substring(x, 1, 1))
  return(x)
}

data_rename3 <- response %>%
  mutate(Primary_cooking_space = gsub("_", " ", gsub(" " , ", ", firstup(Primary_cooking_space))))

## Combine safe cooking practices answers (new)

data_rename4 <- response %>%
  mutate(Safe_cooking_practices = gsub(" ", ",", gsub("_" , " ", firstup(Safe_cooking_practices))))


## Step 3: merge into one dataset
data_merge <- cbind(data_rename2, data_norename, data_rename4, dots_merge)

#data_norename$a8_population_groups_other_than_idps_in_site_select_all_applicable <- str_replace_all(data_norename$a8_population_groups_other_than_idps_in_site_select_all_applicable, pattern = " ", replacement = " - ")
#data_norename$a8_population_groups_other_than_idps_in_site_select_all_applicable <- str_replace_all(data_norename$a8_population_groups_other_than_idps_in_site_select_all_applicable, pattern = "_", replacement = " ")
#data_norename$a4_site_code <- str_replace_all(data_norename$a4_site_code, pattern = "_", replacement = " - ")


## Step 4: Add maps and save as csv
#data_merge$a4_site_name <- str_trim(data_merge$a4_site_name, "both")
data_merge$`@maps` <- paste0("./maps/YEM_CCCM__",response$a3_sub_district_code,".pdf")


## Output file
write.csv(data_merge, paste0("./merge/cccm_site-level_merge_en_v2_",today,".csv"), row.names = F)                     
browseURL(paste0("./merge/cccm_site-level_merge_en_v2_",today,".csv"))



######################## AR site-level datamerge V1 (not finalized yet) #########################

## Load choice sheet
choices <- read.xlsx("./data/kobo/R_choices__AR.xlsx")

## Load cleaned data
response <- read.xlsx("./output/Internal/CCCM_SiteReporting_V1 Internal_2021-05-19.xlsx")
#response <- cleanHead(response)
#response <- cleanHead(response)

## Filter out sites that are useless
#response <- response %>% filter(check == 1 & check.2 == 1)

## Step 1: Data Merge - dots
lookup <- data.frame(colour = c("inadequate", "non_existent", "adequate"), path = c("./merge/red.png", "./merge/grey.png", "./merge/green.png"))


dots_df <- response %>% select("rrm_distributions":"waste_disposal_services")
#dots_df[is.na(dots_df)] <- "not_applicable"


dots_merge <- as.data.frame(lapply(dots_df, function(col) lookup$path[match(col, lookup$colour)]))
colnames(dots_merge) <- paste0("@", colnames(dots_merge))

#write.csv(dots_merge, "./output/test_image_datamerge.csv", row.names = F)

## Step 2: Data Merge - rest of the stuff
data_rename <- response %>% select("a4_site_name", "q0_4_date", "b4_site_cccm_agency_name", "c2_landowner", "c1_type_of_site", "Primary_cooking_modality", "Primary_cooking_space", "Safe_cooking_practices", "additional_fire_safety_measures",
                                   "c3_tenancy_agreement_for_at_least_6_months", "b7_community_committee_in_place", "d1_most_common_reason_idps_left_place_of_origin",
                                   "a8_population_groups_other_than_idps_in_site_select_all_applicable", "d3_most_common_intention_in_next_three_months", "c4_general_market_in_site_close_proximity",
                                   "d2_1_most_common_district_of_idp_origin", "d2_2_second_most_common_district_of_idp_origin", "d2_3_third_most_common_district_of_idp_origin", 
                                   "c6_electricity_solar_power_available_in_site", "c5_fuel_available_in_site_close_proximity", "c8_primary_water_source", "c9_primary_shelter_type",
                                   "c10_primary_latrine_type", "i1_top_priority_need", "i2_second_priority_need", "i3_third_priority_need", starts_with("psp_"), starts_with("if_ngo_"),
                                   "c7_presence_of_particularly_vulnerable_groups.unaccompanied_separated_children", "c7_presence_of_particularly_vulnerable_groups.child_headed_hh", "c7_presence_of_particularly_vulnerable_groups.elderly",
                                   "c7_presence_of_particularly_vulnerable_groups.female_headed_hh", "c7_presence_of_particularly_vulnerable_groups.marginalized_people", "c7_presence_of_particularly_vulnerable_groups.persons_with_disabilities",
                                   "c7_presence_of_particularly_vulnerable_groups.persons_with_chronic_diseases_serious_medical_conditions", "c7_presence_of_particularly_vulnerable_groups.pregnant_and_lactating_women",
                                   "f1_threats_to_the_site.flooding", "f1_threats_to_the_site.eviction", "f1_threats_to_the_site.infectious_diseases", "f1_threats_to_the_site.friction_between_communities",
                                   "f1_threats_to_the_site.water_contamination", "f1_threats_to_the_site.conflict_related_incidents", "f1_threats_to_the_site.fire_related_incidents", "a1_governorate_code", "a2_district_code", "a3_sub_district_code")



data_rename <- data_rename %>% select(-c(ends_with("_other")))

data_rename2 <- data_rename
data_rename2[] <- choices$`label::arabic`[match(unlist(data_rename2), choices$name)]


#data_rename3 <- response %>% select("a4_site_name")
#data_rename4 <- data_rename
#data_rename4[] <- external_choices$`label::arabic`[match(unlist(data_rename4), external_choices$label::english)]

### Rename the variables to keep the data merge template intact
data_norename <- response %>% select("a6_site_occupation_date_dd_mm_yy", "a7_site_population_individual", "a7_site_population_hh")

## Step 3: merge into one dataset
data_merge <- cbind(data_rename2,data_norename, dots_merge)


## Step 4: Add maps and save as csv
#data_merge$a4_site_name <- str_trim(data_merge$a4_site_name, "both")
data_merge$`@maps` <- paste0("./maps/YEM_CCCM__",response$a3_sub_district_code,".pdf")


write.xlsx(data_merge, paste0("./merge/cccm_site-level_merge_v1_",today,"_AR.xlsx"), row.names = F)                     
browseURL(paste0("./merge/cccm_site-level_merge_v1_",today,"_AR.xlsx"))




