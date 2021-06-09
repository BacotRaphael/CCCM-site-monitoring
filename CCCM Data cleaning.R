# CCCM Site Monitoring Tool - Data Cleaning script
# REACH Yemen - raphael.bacot@reach-initiative.org
# V5
# 04/06/2021

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
today <- Sys.Date()

## Install/Load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, openxlsx, reshape2, sf, leaflet, readxl)
p_load_gh("mabafaba/cleaninginspectoR","agualtieri/cleaninginspectoR","agualtieri/dataqualitycontrol", "impact-initiatives-research/clog")

## Source
source("./R/cleanHead.R")
source("./R/add_locations.R")
source("./R/moveme.R")
source("./R/utils.R")
## Paramaters to be updated each month

## Uncomment the relevant tool version and comment the other one
tool.version <- "V1"                                                            # V1 or V2 update according to the kobo tool used
# tool.version <- "V2"

## Update the directory for all files each month with the latest version. Beware of getting V1 and V2 right!
rawdata.filename.v1 <- "data/Copy of CCCM_Site_Reporting_Form_V1_Week 8_raw org data.xlsx"    # Update the name of the rawdata filename
rawdata.filename.v2 <- "data/Copy of CCCM_Site_Reporting_V2__Week 8_raw org data.xlsx"
tool.filename.v1 <- "data/CCCM_Site_Reporting_Kobo_tool_(V1)_12042021.xlsx"
tool.filename.v2 <- "data/CCCM_Site_Reporting_Kobo_tool_(V2)_12042021.xlsx"
sitemasterlist.filename <- "CCCM-site-masterlist-cleaning/data/CCCM_IDP Hosting Site List_March 2021.xlsx" # To be updated depending on where you put it
filename.pcodes <- "R/pcodes/yem_admin_ochayemen_20191002.xlsx"

## Tool columns comparison
# responsev1 <- read.xlsx(rawdata.filename.v1)
# responsev2 <- read.xlsx(rawdata.filename.v2)
# colv1 <- colnames(responsev1)
# colv2 <- colnames(responsev2)
# col.all <- unique(colv1, colv2)

## Load survey questions from kobo tool
tool <- if (tool.version == "V1") {read.xlsx(tool.filename.v1, sheet = "survey")} else if(tool.version == "V2") {read.xlsx(tool.filename.v2, sheet = "survey")} else {print("invalid tool entered, should be either V1 or V2")}
## Load survey choices Using kobo to rename the site name and than merge the other column
choices <- if (tool.version == "V1") {read.xlsx(tool.filename.v1, sheet = "choices")} else if(tool.version == "V2") {read.xlsx(tool.filename.v2, sheet = "choices")} else {print("invalid tool entered, should be either V1 or V2")}
external_choices <- if (tool.version == "V1") {read.xlsx(tool.filename.v1, sheet = "external_choices")} else if (tool.version == "V2") {read.xlsx(tool.filename.v2, sheet = "external_choices")} else {print("invalid tool entered, should be either V1 or V2")}
external_choices_site <- external_choices %>%
  filter(list_name == "sitename") %>% dplyr::rename(a4_site_name = name)
choices_all <- bind_rows(choices, external_choices) %>%                         # For cleaning log functions
  rbind(c("sitename", "other", "Other", "أخرى", NA, NA))
survey_choices <- get.select.db()

## Load site_name masterlist to match admin names with site_name
setwd("..")                                                                     # To be updated depending on where you put it
masterlist <- read.xlsx(sitemasterlist.filename) %>%                            
  setNames(gsub("\\.", "_", colnames(.))) %>%
  mutate_at(vars(matches("Newly_|_Households|Population|#_of_total_Households")), ~as.numeric(arabic.tonumber(gsub(",", "", .))))
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Load data to be cleaned
response <- if (tool.version == "V1") {read.xlsx(rawdata.filename.v1)} else if (tool.version == "V2") {read.xlsx(rawdata.filename.v2)} else {print("invalid tool entered, should be either V1 or V2")}
response <- response %>% mutate(a4_site_name = a4_other_site, id = 1:n(), .before = "deviceid") %>%
  dplyr::rename(index = '_index', uuid = '_uuid')

## Upload updated cleaning log file
files.cleaning.log <- paste0("output/cleaning log/partners updated/", list.files("output/cleaning log/partners updated"))
cleaning.log <- data.frame()
for (file in files.cleaning.log){
  cleaning.log <- bind_rows(cleaning.log, read.xlsx(file) %>% mutate_all(as.character))
}
if ((cleaning.log %>% group_by(uuid, variable) %>% filter(n()>1) %>% nrow) > 0) {print("There are duplicates entries (or multiple entries for the same variable) in the cleaning log. Make sure it's ok and that there are no conflicting new_values assigned.")}
duplicate.cl <- cleaning.log %>% group_by(uuid, variable) %>% filter(n()>1) %>% ungroup

### Sitenames
## Check that sitename/code entered are not duplicated & within list
data.validation.list()                                                          # Extract all choices from Kobo tool in neat format (with headers name)
duplicate.site.cl <- cleaning.log %>%                                           # Display duplicate site ids
  filter(variable == "a4_site_name", !is.na(new_value)) %>%
  group_by(new_value) %>% filter(n()>1) %>% arrange(new_value)

site.cl.not.in.list <- cleaning.log %>%                                         # Display site id entered that are not in kobo list
  filter(variable == "a4_site_name",
         !is.na(new_value),
         !(new_value %in% data.val$a4_site_name[!is.na(data.val$a4_site_name)]))

## Filter sites not in kobo list or that are NA to generate a new site code
new.sites <- cleaning.log %>%                                         # Display site id entered that are not in kobo list
  filter(variable == "a4_site_name",
         is.na(new_value),
         !(new_value %in% data.val$a4_site_name[!is.na(data.val$a4_site_name)])) %>%
  mutate(new_site = TRUE, new_site_id = NA, new_site_name_en = NA, new_site_name_ar = NA, .after = "new_value") %>%
  left_join(response %>% select(uuid, a2_district, a3_sub_district), by = "uuid") %>%
  relocate(a2_district, a3_sub_district, .after = "new_site_name_ar")

## Flag sites that are existing and that don't need a new name (changing column new_site to FALSE)
existing.sites.uuid <- c("90c2ab8e-b3c5-49f3-86be-55a8aa89c144", "f4f5bee4-afcb-44f6-89f3-d22b6ff17962", "42f0eea0-8d4e-4200-9383-7cc976d0a8e6")
new.sites <- new.sites %>%
  mutate(new_site = ifelse(uuid %in% existing.sites.uuid,                       # put new_site = FALSE for the uuid selected above
                           FALSE, new_site))

## Creating new sitename code
max.id.split <- masterlist$Site_ID %>% sub(".*?_", "",.) %>%                    # Extract the highest number in site id in masterlist 
  as.numeric %>% max(na.rm = T)
latest_id <- masterlist$Site_ID[grepl(paste0("_", max.id.split), masterlist$Site_ID)]      # Extract the latest full ID
seq <- (max.id.split+1):(max.id.split+500)                                      # Create list of numbers starting from the latest one

new.sites <- new.sites %>%
  filter(new_site == TRUE) %>%
  mutate(new_site_id = paste0(a2_district, "_", seq[1:nrow(.)]),                # create new site ID in column new_site_id of the cleaning log                
         new_value = new_site_id)
new.sites.uuid <- new.sites$uuid

## Assign the new official name in English + Arabic
new.sites %>% write.xlsx(paste0("output/new_sites_",today, ".xlsx"))
# browseURL(paste0("output/new_sites_",today, ".xlsx"))                         # Manually enter the name of site in English and Arabic, and save the file with "_updated" at the end of the name
# Read the updated sitename file
new.sites.updated <- read.xlsx(paste0("output/new_sites_",today, "_updated.xlsx")) 
if (new.sites.updated %>% filter(is.na(new_site_name_en) | is.na(new_site_name_ar)) %>% nrow > 0){print("You didn't assign name in english and or Arabic for all new sites. Please redo the previous step.")}
if (nrow(new.sites.updated)>0){print(paste0("There are ", nrow(new.sites.updated), " new sites that will be added to the site masterlist."))}

## Add to cleaning log to keep track of changes done to site name in english + arabic in the dataset
cleaning.log <- cleaning.log %>%                                                # Update in the cleaning log
  filter(!(uuid %in% new.sites.uuid & variable == "a4_site_name")) %>%          # filter out old entries in cleaning log for the new sites
  bind_rows(new.sites)                                                          # Append new value for the site ID for new sites in cleaning log

### Apply cleaning log changes to response file
clean_data <- response
for (n in seq_along(1:nrow(cleaning.log))){
  col <- cleaning.log %>% slice(n) %>% pull(variable)                                                 # Get the name of the variable to be updated in response dataset
  new_value <- cleaning.log %>% slice(n) %>% pull(new_value)                                          # Get the new value for this cleaning log entry
  if (new_value %in% c("confirmed", "Confirmed")) {new_value <- cleaning.log[n, "old_value"]} else {  # Tell to keep old value if new value is "confirmed"
    new_value <- cleaning.log[n, "new_value"]}
  if (col %in% colnames(clean_data)) {
    clean_data[clean_data$uuid==cleaning.log[n,"uuid"], col] <- new_value                             # Assign new value to the variable in the dataframe response
  }
}

## Update the masterlist with new sites entries + ID and available informations
new.sites.master <- new.sites.updated %>% 
  left_join(response %>% select(uuid, a5_2_gps_latitude, a5_1_gps_longitude, a7_site_population_hh, a7_site_population_individual), by="uuid") %>%
  mutate(Partner_Name = agency, Sub_Dist_ID=a3_sub_district, Site_ID = new_site_id, Site_Name=new_site_name_en, 
         Site_Name_In_Arabic=new_site_name_ar, Latitude=a5_2_gps_latitude, Longitude=a5_1_gps_longitude, "#_of_total_Households"=as.numeric(a7_site_population_hh),
         Total_Site_Population=as.numeric(a7_site_population_individual)) %>% select(intersect(colnames(.), colnames(masterlist))) 

masterlist_updated <- masterlist %>% bind_rows(new.sites.master)                # Adding the new sites in the existing site masterlist
# dir.create("output/masterlist", showWarnings = F)
masterlist_updated %>% write.xlsx(paste0("output/masterlist/masterlist_", today,".xlsx"))

# ### Using kobo to rename the site name and than merge the other column
# clean_data$a4_site_name2 <- external_choices$`label::english`[match(clean_data$a4_site_name, external_choices$`label::arabic`)]
# clean_data <- clean_data %>% mutate(a4_site_name3 = ifelse(!is.na(a4_site_name2), as.character(a4_site_name2), a4_other_site))
# external_choices <- filter(external_choices, external_choices$list_name == "sitename")
# names(external_choices)[names(external_choices) == "name"] <- "a4_site_name"
# 
# clean_data$a4_site_name2 <- external_choices$label..english[match(clean_data$a4_site_name, external_choices$a4_site_name)]
# 
# clean_data <- clean_data %>% mutate(a4_site_name3 = ifelse(!is.na(a4_site_name2), as.character(a4_site_name2), a4_other_site))

### Rename all variables for the dashboard and save file
# clean_data <- add.location(clean_data) 
admin3 <- read.xlsx(filename.pcodes, sheet = "admin3") %>%
  dplyr::rename(a3_sub_district_name=admin3RefName_en, a3_sub_district=admin3Pcode,
                a2_district_name=admin2Name_en, a2_district=admin2Pcode,
                a1_governorate_name=admin1Name_en, a1_governorate=admin1Pcode)

clean_data_loc <- clean_data %>% 
  select(-any_of(c("a1_governorate", "a2_district"))) %>%
  left_join(admin3, by="a3_sub_district") %>%
  select(uuid, start, end, today, matches("a1_|a2_"), matches("a3_"), everything())

# Delete unneccessary site columns
clean_data <- clean_data_loc %>% select(-c("deviceid", "subscriberid", "imei","calca11", contains("phone")))
pii <- clean_data_loc %>% select(uuid, c("deviceid", "subscriberid", "imei","calca11", contains("phone")))

# Rename site columns to match the DB file
clean_data <- clean_data %>%
  setnames(old = c("a4_site_name", "a1_governorate", "a2_district", "a3_sub_district"), 
           new = c("a4_site_code", "a1_governorate_code", "a2_district_code", "a3_sub_district_code"),
           skip_absent = T)

# # Rename site columns to match the DB file
# names(clean_data)[names(clean_data) == "a4_site_name3"] <- "a4_site_name"
# names(clean_data)[names(clean_data) == "new_site_id"] <- "a4_site_code"
# names(clean_data)[names(clean_data) == "a1_governorate"] <- "a1_governorate_code"
# names(clean_data)[names(clean_data) == "a2_district"] <- "a2_district_code"
# names(clean_data)[names(clean_data) == "a3_sub_district"] <- "a3_sub_district_code"

## Prepare dashboard-ready file => SHOULD BE ONLY IN DATA MERGE?
# dashboard <- clean_data %>% select(-any_of(c("country_name", "country_id"))) %>%
#   mutate(a9_Site_Classification=" ", a4_site_code = " ") %>%
#   setnames(old = c("a1_governorate_name", "X_id", "uuid", "X_submission_time"),
#            new = c("a1_governorate", "_id", "_uuid", "_submission_time"),
#            skip_absent = T)
# 
# db_rename <- select(dashboard, -c("q0_4_date", "a5_1_gps_longitude", "a5_2_gps_latitude", "a6_site_occupation_date_dd_mm_yy", 
#                                   "a7_site_population_hh", "a7_site_population_individual", "a1_governorate", "a1_governorate_code",
#                                   "a2_district_name", "a2_district_code", "a3_sub_district_name", "a3_sub_district_code","a4_site_code", "a4_site_name")) %>%
#   as.data.frame
# 
# db_rename[] <- choices$label..english[match(unlist(db_rename), choices$name)] 
# 
# db_norename <- select(dashboard, c("q0_4_date", "a5_1_gps_longitude", "a5_2_gps_latitude", "a6_site_occupation_date_dd_mm_yy", 
#                                    "a7_site_population_hh", "a7_site_population_individual", "a1_governorate", "a1_governorate_code",
#                                    "a2_district_name", "a2_district_code", "a3_sub_district_name", "a3_sub_district_code",
#                                    "a4_site_name", "a4_site_code"))
# 
# final_dashboard <- cbind(db_rename, db_norename)
# 
# final_dashboard$a8_population_groups_other_than_idps_in_site_select_all_applicable <- str_replace_all(final_dashboard$a8_population_groups_other_than_idps_in_site_select_all_applicable, pattern = " ", replacement = " - ")
# final_dashboard$a8_population_groups_other_than_idps_in_site_select_all_applicable <- str_replace_all(final_dashboard$a8_population_groups_other_than_idps_in_site_select_all_applicable, pattern = "_", replacement = " ")
# final_dashboard$a4_site_code <- str_replace_all(final_dashboard$a4_site_code, pattern = "_", replacement = " - ")
# 
# final_dashboard <- final_dashboard[moveme(names(final_dashboard), "a1_governorate after q1_3_key_informat_mobile_number; a1_governorate_code after a1_governorate;
#                                           a2_district_name after a1_governorate_code; a2_district_code after a2_district_name; a3_sub_district_name after a2_district_code;
#                                           a3_sub_district_code after a3_sub_district_name; a4_site_name after a3_sub_district_code; a4_site_code after a4_site_name")]
# 
# final_dashboard <- final_dashboard[moveme(names(final_dashboard), "q0_4_date after q0_3_organization_other; a5_1_gps_longitude after a5_gps_coordinates; 
#                                           a5_2_gps_latitude after a5_1_gps_longitude; a6_site_occupation_date_dd_mm_yy after a5_2_gps_latitude; 
#                                           a7_site_population_hh after a6_site_occupation_date_dd_mm_yy; a7_site_population_individual after a7_site_population_hh;
#                                           a9_Site_Classification after a9_formal_informal")]
# 
# write.xlsx(final_dashboard, paste0("./output/dashboard/CCCM_Site Reporting_V2_",today,".xlsx"))

#write.xlsx(final_dataset, paste0("./output/CCCM_SiteReporting_Week 1 Cleaned_",today,".xlsx"))

### Save Internal and External version of the files
#### After data is cleaning update the dataset by adding admin units names

#### INTERNAL
final_dataset_internal <- clean_data %>% select(-c("start", "end", "q0_1_enumerator_name", "q0_2_gender", "q1_1_key_informant_name", "q1_2_key_informat_gender", "a5_gps_coordinates",
                                                   "q0_3_organization_other", "a4_other_site", "q0_4_date", "b2_exu_fp_name", "b8_community_committee_fp_name", "b9_community_committee_fp_cell", 
                                                   contains("number")))
write.xlsx(final_dataset_internal, paste0("./output/internal/CCCM_SiteReporting_V2 Internal_",today,".xlsx"))

#### EXTERNAL
final_dataset_external <- clean_data %>% select(-c("start", "end", "q0_1_enumerator_name", "q0_2_gender", "q1_1_key_informant_name", "q1_2_key_informat_gender", "a5_gps_coordinates", 
                                                   "q0_3_organization_other", "a4_other_site", "a5_1_gps_longitude", "a5_2_gps_latitude", "b2_exu_fp_name", "b2_exu_fp_name", "b8_community_committee_fp_name", 
                                                   "b9_community_committee_fp_cell", "q0_3_organization", "q0_4_date", "b7_community_committee_in_place"), contains("number"))
write.xlsx(final_dataset_external, paste0("./output/external/CCCM_SiteReporting_V2 External_",today,".xlsx"))    

