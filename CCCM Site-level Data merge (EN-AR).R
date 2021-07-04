# CCCM Site Monitoring Tool - Data Merge Script
# REACH Yemen - christine.pfeffer@reach-initiative.org
# Site-level datamerge EN/AR
# 01/07/2021 - updated by Raphael Bacot - raphael.bacot@reach-initiative.org 

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

################################################################################
#### Parameters TO BE UPDATED EACH TIME
################################################################################

#### Indicate tool version ####
#tool <- "V1"
tool <- "V2"
print(paste0("The tool version selected is ", tool))

#### Declare file names 
# kobo.filename.v1 <- "data/kobo/CCCM_Site_Reporting_Kobo_tool_V1_23_05_2021_FINAL.xlsx"
# kobo.filename.v2 <- "data/kobo/CCCM_Site_Reporting_Kobo_tool_V2_23_05_2021_FINAL.xlsx"
# data.filename.v1 <- "./output/Internal/CCCM_SiteReporting_V1 Internal_2021-07-01.xlsx"
# data.filename.v2 <- "./output/Internal/CCCM_SiteReporting_V2 Internal_2021-07-01.xlsx"
kobo.filename.v1 <- "data/CCCM_Site_Reporting_Kobo_tool_V1_23_05_2021_FINAL.xlsx"
kobo.filename.v2 <- "data/CCCM_Site_Reporting_Kobo_tool_V2_23_05_2021_FINAL.xlsx"
data.filename.v1 <- "data/CCCM_SiteReporting_V1 Internal_2021-07-01.xlsx"
data.filename.v2 <- "data/CCCM_SiteReporting_V2 Internal_2021-07-01.xlsx"

## Load choice sheet
# choices <- read.csv("./data/kobo/choices.csv")
choices <- if (tool == "V1") {read.xlsx(kobo.filename.v1, sheet = "choices")} else if (tool == "V2") {
  read.xlsx(kobo.filename.v2, sheet = "choices")} else {print("invalid tool entered, should be either V1 or V2")}

external_choices <- if (tool == "V1") {read.xlsx(kobo.filename.v1, sheet = "external_choices")} else if (tool == "V2") {
  read.xlsx(kobo.filename.v2, sheet = "external_choices")} else {print("invalid tool entered, should be either V1 or V2")}

# Adding 1 and 0 to choice list so the script also matches Yes/No so match also binary columns 

binary <- "as.number"                                                           # to keep binary coluns as numbers 
# binary <- "as.text"                                                             # to transform binary as yes - no
if (binary == "as.number"){
  choices.binary <- data.frame(list_name = c("binary", "binary"), name = c("1", "0"), `label::english` = c("1", "0"), `label::arabic` = c("1", "0"), check.names = F)  
} else if (binary == "as.text"){
  choices.binary <- data.frame(list_name = c("binary", "binary"), name = c("1", "0"), `label::english` = c("Yes", "No"), `label::arabic` = c("نعم", "لاء"), check.names = F)
  } else {
    print("invalid tool entered, should be either V1 or V2")}
choices <- choices %>% bind_rows(choices.binary)

################################################################################
## Load cleaned data [V1 or V2 depending on tool]
################################################################################
response <- if (tool == "V1") {read.xlsx(data.filename.v1)} else if (tool == "V2"){
  read.xlsx(data.filename.v2)} else {print("invalid tool version entered, should be either V1 or V2")}
#response <- cleanHead(response)
#response <- cleanHead(response)

# Kobo Tool comparison
# Make sure there is no additionnal differences between the two tools
response1 <- read.xlsx(data.filename.v1)
response2 <- read.xlsx(data.filename.v2)
col1 <- colnames(response1)
col2 <- colnames(response2)
setdiff(col1, col2)                                                             # displays columns that are V1 specific
setdiff(col2, col1)                                                             # displays columns that are V2 specific

## Filter out sites that are useless                                            => Should it be erased?
#response <- response %>% filter(check == 1 & check.2 == 1)                     => Should it be erased?

## Load pcode
filename.pcode <- "R/pcodes/admin_list.xlsx"
pcodes <- read.xlsx(filename.pcode)

## Step 1: Data Merge - dots
lookup <- data.frame(colour = c("inadequate", "non_existent", "adequate"), path = c("./merge/red.png", "./merge/grey.png", "./merge/green.png"))

dots_df <- response %>% select("rrm_distributions":"waste_disposal_services")
dots_df[is.na(dots_df)] <- "not_applicable"

dots_merge <- as.data.frame(lapply(dots_df, function(col) lookup$path[match(col, lookup$colour)]))
colnames(dots_merge) <- paste0("@", colnames(dots_merge))
#write.csv(dots_merge, "./output/test_image_datamerge.csv", row.names = F)

## Step 2: Data Merge - rest pf the stuff
## Note: Here the code selects all columns from V1 and V2, and ignore the one that don't exist
## Question for Christine/Kata => Should we select "Primary_cooking_space", "Safe_cooking_practices" for V1 as they exist in the tool? or it was non selected on purpose? 
## For now kept it non selected as well as additional_fire_safety_measures as we do recode it in the datarename_select_multiple
data_rename <- response %>% select(any_of(c("q0_3_organization", "c2_landowner", "c1_type_of_site", "Primary_cooking_modality",
                                            "c3_tenancy_agreement_for_at_least_6_months", "b7_community_committee_in_place", "d1_most_common_reason_idps_left_place_of_origin",
                                            "d3_most_common_intention_in_next_three_months", "c4_general_market_in_site_close_proximity",
                                            "d2_1_most_common_district_of_idp_origin", "d2_2_second_most_common_district_of_idp_origin", "d2_3_third_most_common_district_of_idp_origin", 
                                            "c6_electricity_solar_power_available_in_site", "c5_fuel_available_in_site_close_proximity", "c8_primary_water_source", "c9_primary_shelter_type",
                                            "c10_primary_latrine_type", "i1_top_priority_need", "i2_second_priority_need", "i3_third_priority_need",
                                            "c7_presence_of_particularly_vulnerable_groups.unaccompanied_separated_children", "c7_presence_of_particularly_vulnerable_groups.child_headed_hh", "c7_presence_of_particularly_vulnerable_groups.elderly",
                                            "c7_presence_of_particularly_vulnerable_groups.female_headed_hh", "c7_presence_of_particularly_vulnerable_groups.marginalized_people", "c7_presence_of_particularly_vulnerable_groups.minorities", "c7_presence_of_particularly_vulnerable_groups.persons_with_disabilities",
                                            "c7_presence_of_particularly_vulnerable_groups.persons_with_chronic_diseases_serious_medical_conditions", "c7_presence_of_particularly_vulnerable_groups.pregnant_and_lactating_women",
                                            "f1_threats_to_the_site.flooding", "f1_threats_to_the_site.eviction", "f1_threats_to_the_site.infectious_diseases", "f1_threats_to_the_site.friction_between_communities",
                                            "f1_threats_to_the_site.water_contamination", "f1_threats_to_the_site.conflict_related_incidents", "f1_threats_to_the_site.fire_related_incidents")), starts_with("psp_"), starts_with("if_ngo_"))

data_rename <- data_rename %>% select(-c(ends_with("_other")))

## Match choice variable name with choice label from the kobo tool for all columns except multiple choices text column
## For english version
data_rename_en <- data_rename
data_rename_en[] <- choices$`label::english`[match(unlist(data_rename_en), choices$name)]

## For Arabic version
data_rename_ar <- data_rename 
data_rename_ar[] <- choices$`label::arabic`[match(unlist(data_rename_ar), choices$name)]

### Rename the variables to keep the data merge template intact
data_norename <- response %>% select("q0_4_date","a1_governorate_name", "a2_district_name", "a3_sub_district_name", "a4_site_name", "a4_site_code","a6_site_occupation_date_dd_mm_yy", "a7_site_population_individual", "a7_site_population_hh", starts_with("B1"))
data_norename_ar <- response %>%  select("q0_4_date", "a3_sub_district_code", "a4_site_name_ar", "a4_site_code","a6_site_occupation_date_dd_mm_yy", "a7_site_population_individual", "a7_site_population_hh", starts_with("B1"))

## Match choice variable name with choice label from the kobo tool for the multiple choices text column
## Reformat and add text questions Primary cooking space - safe cooking practices answers 
firstup <- function(x){substr(x, 1, 1) <- toupper(substring(x, 1, 1))
return(x)}
choices.q <- choices %>% filter(grepl("q", list_name))                          # filter out gov/dis/ngo choices

# English reformat
data_rename_select_multiple <- response %>%         
  mutate_at(vars(c("Primary_cooking_space","Safe_cooking_practices","a8_population_groups_other_than_idps_in_site_select_all_applicable", "additional_fire_safety_measures")),
            ~ gsub(" ,","," ,gsub("^, ", "", str_replace_all(., setNames(paste0(", ",choices.q$`label::english`), choices.q$name)))) %>% tolower %>% firstup(.)) %>%
  select(Primary_cooking_space, Safe_cooking_practices,a8_population_groups_other_than_idps_in_site_select_all_applicable, additional_fire_safety_measures)

# Arabic reformat 
data_rename_select_multiple_ar <- response %>%
  mutate_at(vars(c("Primary_cooking_space","Safe_cooking_practices","a8_population_groups_other_than_idps_in_site_select_all_applicable", "additional_fire_safety_measures")),
            ~gsub(" ،", "،", gsub("^،", "", str_replace_all(., setNames(paste0("، ",choices.q$`label::arabic`), choices.q$name))))) %>%
  select(Primary_cooking_space, Safe_cooking_practices, a8_population_groups_other_than_idps_in_site_select_all_applicable, additional_fire_safety_measures)

## Step 3: merge into one dataset
data_merge <- cbind(data_rename_en, data_rename_select_multiple, data_norename, dots_merge)       ## Remark: sketchy to use cbind, would be more robust to use leftjoin with uuid?
data_merge_ar <- cbind(data_rename_ar, data_rename_select_multiple_ar, data_norename_ar, dots_merge)

## Check for duplicated columns
a<-colnames(data_merge)[duplicated(colnames(data_merge))]
b<-colnames(data_merge_ar)[duplicated(colnames(data_merge_ar))]

# colnames(data_merge)[duplicated(colnames(data_merge))] => intermediate check to see if there are duplicates // should be erased hopefully :)

# Recode the administration column as "present" or "not present"
# data_merge <- data_merge %>%
#   mutate_at(vars(matches("B1_CCCM_Pillars_existing_on_s.site_administration_")), ~ifelse(is.na(.), NA,
#                                                                                          ifelse(. == 1, "Present",
#                                                                                                 ifelse(. == 0, "Not Present", .))))
# data_merge_ar <- data_merge_ar %>%
#   mutate_at(vars(matches("B1_CCCM_Pillars_existing_on_s.site_administration_")), ~ifelse(is.na(.), NA,
#                                                                                          ifelse(. == 1, "موجودة",
#                                                                                                 ifelse(. == 0, "غير موجودة", .)))) %>%
#     mutate_at(vars(matches("psp_|if_ngo")), ~ifelse(is.na(.), "لا ينطبق", .)) %>%             # code psp_ if_ngo columns as "not applicable" => لا ينطبق
#     mutate_at(vars(-matches("psp_|if_ngo")), ~ifelse(is.na(.), "غير متوفر", .))                  # code the other columns as "not available" => غير متوفر

# Re-code not applicable for NA in Arabic, except for psp_ if_ngo columns for which NAs are recoded as "not available"  
data_merge_ar <- data_merge_ar %>%
  mutate_at(vars(matches("psp_|if_ngo")), ~ifelse(is.na(.), "لا ينطبق", .)) %>%             # code psp_ if_ngo columns as "not applicable" => لا ينطبق
  mutate_at(vars(-matches("psp_|if_ngo|^@|^a3_|^a4_")), ~ifelse(is.na(.), "غير متوفر", .)) %>%          # code the other columns as "not available" => غير متوفر
  # left_join(choices[choices$list_name=="ngo",] %>% dplyr::select(name, `label::arabic`), by = c("q0_3_organization"="name")) %>%
  # dplyr::select(-q0_3_organization) %>% dplyr::rename(q0_3_organization=`label::arabic`) %>% relocate(q0_3_organization, .before = 1) %>%
  left_join(pcodes %>% select(admin3Pcode, admin1Name_ar, admin2Name_ar, admin3Name_ar), by=c("a3_sub_district_code"="admin3Pcode")) %>%
  dplyr::select(admin1Name_ar, admin2Name_ar, admin3Name_ar, a4_site_name_ar, everything(), -a3_sub_district_code) %>%
  dplyr::rename(a1_governorate_name=admin1Name_ar, a2_district_name=admin2Name_ar, a3_sub_district_name=admin3Name_ar)

## Step 4: Add maps and save as csv
#data_merge$a4_site_name <- str_trim(data_merge$a4_site_name, "both")
data_merge$`@maps` <- paste0("./maps/YEM_CCCM__",response$a3_sub_district_code,".pdf")
data_merge_ar$`@maps` <- paste0("./maps/YEM_CCCM__",response$a3_sub_district_code,".pdf")

## Write output file
if (tool == "V1"){
  
  write.xlsx(data_merge, paste0("./merge/cccm_site-level_merge_en_v1_",today,".xlsx"))
  browseURL(paste0("./merge/cccm_site-level_merge_en_v1_",today,".xlsx"))
  write.xlsx(data_merge_ar, paste0("./merge/cccm_site-level_merge_en_v1_",today,"_ar.xlsx"))
  browseURL(paste0("./merge/cccm_site-level_merge_en_v1_",today,"_ar.xlsx"))
  
  } else if (tool == "V2") {
  
  write.xlsx(data_merge, paste0("./merge/cccm_site-level_merge_en_v2_",today,".xlsx"))
  browseURL(paste0("./merge/cccm_site-level_merge_en_v2_",today,".xlsx"))
  write.xlsx(data_merge_ar, paste0("./merge/cccm_site-level_merge_en_v2_",today,"_ar.xlsx"))
  browseURL(paste0("./merge/cccm_site-level_merge_en_v2_",today,"_ar.xlsx"))

  } else {
  
    print("invalid tool version entered, should be either V1 or V2")}




## To get step by step understanding of how the recoding of multiple choice text columns is done 
# Original solution (replacing the _ with space and putting first letter to upper) => not as good for some choices, so preferred the solution with kobo label choice list
# data_rename_3 <- response %>%
#   mutate(Primary_cooking_space = gsub("_", " ", gsub(" ", ", ", firstup(Primary_cooking_space))),
#          Safe_cooking_practices = gsub("_", " ", gsub(" ", ", ", firstup(Safe_cooking_practices))))
# Neater way to do it all at once, calling all MCQ text columns at once:
# data_rename_3 <- response %>%
#   mutate_at(vars(c("Primary_cooking_space", "Safe_cooking_practices", "c7_presence_of_particularly_vulnerable_groups")),
#             ~gsub("_", " ", gsub(" ", ", ", firstup(.)))) %>%
#   select(Primary_cooking_space, Safe_cooking_practices, c7_presence_of_particularly_vulnerable_groups, additional_fire_safety_measures)

# data_rename_3_ar <- response %>%
#   mutate(Primary_cooking_space = str_replace_all(Primary_cooking_space, setNames(choices.q$`label::arabic`, choices.q$name)),
#          Safe_cooking_practices = str_replace_all(Safe_cooking_practices, setNames(choices.q$`label::arabic`, choices.q$name)),
#          c7_presence_of_particularly_vulnerable_groups = str_replace_all(c7_presence_of_particularly_vulnerable_groups, setNames(paste0("ÃÂ ",choices.q$`label::arabic`), choices.q$name)))
# data_rename_3_ar <- response %>%
#   mutate(Primary_cooking_space = str_replace_all(Primary_cooking_space, setNames(choices.q$`label::arabic`, choices.q$name)),
#          Safe_cooking_practices = str_replace_all(Safe_cooking_practices, setNames(choices.q$`label::arabic`, choices.q$name)),
#          c7_presence_of_particularly_vulnerable_groups = gsub(" ÃÂ", "ÃÂ", gsub("^ÃÂ", "", str_replace_all(c7_presence_of_particularly_vulnerable_groups, setNames(paste0("ÃÂ ",choices.q$`label::arabic`), choices.q$name)))))


# Archive: old script used to col the following columns for the data_rename data.frame:

# colv1 <- c("b4_site_cccm_agency_name", "c2_landowner", "c1_type_of_site", "Primary_cooking_modality", "additional_fire_safety_measures",
#            "c3_tenancy_agreement_for_at_least_6_months", "b7_community_committee_in_place", "d1_most_common_reason_idps_left_place_of_origin",
#            "a8_population_groups_other_than_idps_in_site_select_all_applicable", "d3_most_common_intention_in_next_three_months", "c4_general_market_in_site_close_proximity",
#            "d2_1_most_common_district_of_idp_origin", "d2_2_second_most_common_district_of_idp_origin", "d2_3_third_most_common_district_of_idp_origin", 
#            "c6_electricity_solar_power_available_in_site", "c5_fuel_available_in_site_close_proximity", "c8_primary_water_source", "c9_primary_shelter_type",
#            "c10_primary_latrine_type", "i1_top_priority_need", "i2_second_priority_need", "i3_third_priority_need", 
#            "c7_presence_of_particularly_vulnerable_groups.unaccompanied_separated_children", "c7_presence_of_particularly_vulnerable_groups.child_headed_hh", "c7_presence_of_particularly_vulnerable_groups.elderly",
#            "c7_presence_of_particularly_vulnerable_groups.female_headed_hh", "c7_presence_of_particularly_vulnerable_groups.marginalized_people", "c7_presence_of_particularly_vulnerable_groups.persons_with_disabilities",
#            "c7_presence_of_particularly_vulnerable_groups.persons_with_chronic_diseases_serious_medical_conditions", "c7_presence_of_particularly_vulnerable_groups.pregnant_and_lactating_women",
#            "f1_threats_to_the_site.flooding", "f1_threats_to_the_site.eviction", "f1_threats_to_the_site.infectious_diseases", "f1_threats_to_the_site.friction_between_communities",
#            "f1_threats_to_the_site.water_contamination", "f1_threats_to_the_site.conflict_related_incidents", "f1_threats_to_the_site.fire_related_incidents")
# 
# colv2 <- c("b4_site_cccm_agency_name", "c2_landowner", "c1_type_of_site", "Primary_cooking_modality", "Primary_cooking_space", "Safe_cooking_practices", "additional_fire_safety_measures",
#            "c3_tenancy_agreement_for_at_least_6_months", "b7_community_committee_in_place", "d1_most_common_reason_idps_left_place_of_origin",
#            "a8_population_groups_other_than_idps_in_site_select_all_applicable", "d3_most_common_intention_in_next_three_months", "c4_general_market_in_site_close_proximity",
#            "d2_1_most_common_district_of_idp_origin", "d2_2_second_most_common_district_of_idp_origin", "d2_3_third_most_common_district_of_idp_origin", 
#            "c6_electricity_solar_power_available_in_site", "c5_fuel_available_in_site_close_proximity", "c8_primary_water_source", "c9_primary_shelter_type",
#            "c10_primary_latrine_type", "i1_top_priority_need", "i2_second_priority_need", "i3_third_priority_need",
#            "c7_presence_of_particularly_vulnerable_groups.unaccompanied_separated_children", "c7_presence_of_particularly_vulnerable_groups.child_headed_hh", "c7_presence_of_particularly_vulnerable_groups.elderly",
#            "c7_presence_of_particularly_vulnerable_groups.female_headed_hh", "c7_presence_of_particularly_vulnerable_groups.minorities", "c7_presence_of_particularly_vulnerable_groups.persons_with_disabilities",
#            "c7_presence_of_particularly_vulnerable_groups.persons_with_chronic_diseases_serious_medical_conditions", "c7_presence_of_particularly_vulnerable_groups.pregnant_and_lactating_women",
#            "f1_threats_to_the_site.flooding", "f1_threats_to_the_site.eviction", "f1_threats_to_the_site.infectious_diseases", "f1_threats_to_the_site.friction_between_communities",
#            "f1_threats_to_the_site.water_contamination", "f1_threats_to_the_site.conflict_related_incidents", "f1_threats_to_the_site.fire_related_incidents")

