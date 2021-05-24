# CCCM Site Monitoring Tool - Data Cleaning script
# REACH Yemen - alberto.gualtieri@reach-initiative.org
# V4
# 31/10/2019

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

## Upload choices from Kobo tool
tool <- read.xlsx("data/CCCM_Site_Reporting_Kobo_tool_(V1)_12042021.xlsx", sheet = "survey")
choices <- read.xlsx("data/CCCM_Site_Reporting_Kobo_tool_(V1)_12042021.xlsx", sheet = "choices")
external_choices <- read.xlsx("data/CCCM_Site_Reporting_Kobo_tool_(V1)_12042021.xlsx", sheet = "external_choices") %>%
  filter(list_name == "sitename") %>% dplyr::rename(a4_site_name = name)

## Upload data to be cleaned and fix some header names
response <- read.xlsx("./data/CCCM_Site_Reporting_Form_V1_example raw data.xlsx")%>%
  mutate(a4_site_name = a4_other_site,
         id = 1:n(), .before = "deviceid") %>%
  dplyr::rename(index = '_index', uuid = '_uuid')

## Upload updated cleaning log file
cleaning_log <- read.xlsx("./output/CCCM_SiteID_cleaning log_2021-05-19.xlsx")
# cleaning_log <- read.xlsx("./output/CCCM_SiteID_cleaning log_2021-05-06.xlsx")

# ## Apply cleaning log [edited loop to change latitude & longitude + new format for conflicting variable in priority needs checks]
# 
# 
# for (n in seq_along(1:nrow(cleaning_log))){
#   col <- cleaning_log %>% slice(n) %>% pull(variable)                                         # Get the name of the variable to be updated in response dataset
#   new_value <- cleaning_log %>% slice(n) %>% pull(new_value)                                  # Get the new value for this cleaning log entry
#   if (new_value %in% c("confirmed", "Confirmed")) {new_value <- cleaning_log[n, "old_value"]} # Tell to keep old value if new value is "confirmed"
#   
#   response[n, col] <- cleaning_log %>% slice(n) %>% pull(new_value)                           # Update the value in response dataset
#   if (!is.na(cleaning_log[n, "conflicting_variable"])) {                                      # In case there is a conflicting variable, update this variable too
#     col2 <- cleaning_log %>% slice(n) %>% pull(conflicting_variable)                          # Get the name of the conflicting variable to be updated
#   }
# }

## Apply cleaning log
my_log <- cleaninglog(ids = cleaning_log$uuid,
                      variables = cleaning_log$variable,
                      new_values = cleaning_log$new_value,
                      name = cleaning_log$fix,
                      change = cleaning_log$change,
                      data_id_column_name = "uuid")

clean_data <- clog_clean(response, my_log)

# ## In case a new ID has to be assigned 
# paste DistrictPcode and increasing number
# YE1505_1566 => get the latest created id code and increase by one the number after 
# => move that part in the data cleaning script

# max.id.split.df <- response$a4_site_name %>% sub(".*?_", "",.) %>% as.numeric %>% max(na.rm = T)
# max.id.split.master <- masterlist$Site_ID %>% sub(".*?_", "",.) %>% as.numeric %>% max(na.rm = T)
# max.id.split <- max(max.id.split.master, max.id.split.df)
# latest_id <- masterlist$Site_ID[grepl(paste0("_", max.id.split), masterlist$Site_ID)]
# response <- response %>% mutate(new_site_id = ifelse(a4_site_name != "other", a4_site_name, paste0(a2_district,"_",numeric_seq$numeric_seq)))

#numeric_seq <- seq(from = 1185, to = 5000)
#numeric_seq <- data.frame(numeric_seq)

### Using kobo to rename the site name and than merge the other column

clean_data$a4_site_name2 <- external_choices$`label::english`[match(clean_data$a4_site_name, external_choices$`label::arabic`)]
clean_data <- clean_data %>% mutate(a4_site_name3 = ifelse(!is.na(a4_site_name2), as.character(a4_site_name2), a4_other_site))

# external_choices <- filter(external_choices, external_choices$list_name == "sitename")
# names(external_choices)[names(external_choices) == "name"] <- "a4_site_name"
# 
# clean_data$a4_site_name2 <- external_choices$label..english[match(clean_data$a4_site_name, external_choices$a4_site_name)]
# 
# clean_data <- clean_data %>% mutate(a4_site_name3 = ifelse(!is.na(a4_site_name2), as.character(a4_site_name2), a4_other_site))

### Rename all variables for the dashboard and save file
# clean_data <- add.location(clean_data) => issue if response doesn't have any district column // not convinced by separate join method in the function as if there is mismatch, it will yield inconsistent admin levels
# clean_data <- clean_data %>% left_join(admin3, by="a3_sub_district") => not possible if not available in survey, needs to use GPS coordinates

filename.pcodes <- "R/pcodes/yem_admin_ochayemen_20191002.xlsx"
admin3 <- read.xlsx(filename.pcodes, sheet = "admin3") %>%
  dplyr::rename(a3_sub_district_name=admin3RefName_en, a3_sub_district=admin3Pcode,
                a2_district_name=admin2Name_en, a2_district=admin2Pcode,
                a1_governorate_name=admin1Name_en, a1_governorate=admin1Pcode)

# localise cleaned gps locations and import admin names accordingly
source("R/utils.R")

# Sanitizing GPS locations
clean_data <- clean.gps(clean_data, "a5_1_gps_longitude", "a5_2_gps_latitude") 

# importing boundaries
# adm2 <- st_read(dsn = "R/shapes/yem_adm_govyem_cso_ochayemen_20191002_GDB.gdb", layer="yem_admbnda_adm2_govyem_cso")
# adm3_loc <- st_read(dsn = "R/shapes/yem_adm_govyem_cso_ochayemen_20191002_GDB.gdb", layer="yem_admbndp_adm3_govyem_cso")
adm3 <- st_read(dsn = "R/shapes/yem_adm_govyem_cso_ochayemen_20191002_GDB.gdb", layer="yem_admbnda_adm3_govyem_cso") %>%
  dplyr::rename(a3_sub_district_name=admin3RefName_en, a3_sub_district=admin3Pcode, a2_district_name=admin2Name_en, 
                a2_district=admin2Pcode, a1_governorate_name=admin1Name_en, a1_governorate=admin1Pcode)

#joining gps coordinates with admin3 names
clean_data_mapped <- clean_data %>% mutate(SHAPE = mapply(c, as.numeric(Longitude_clean), as.numeric(Latitude_clean), SIMPLIFY = F) %>% map(st_point)) %>%
  st_sf(crs = 4326, sf_column_name = "SHAPE") %>% select(-a1_governorate) %>%
  st_join(adm3) %>% st_drop_geometry %>% mutate(country_name = "Yemen", country_id = "YE")
clean_data_mapped <- clean_data_mapped %>% 
  select(country_name, a1_governorate, country_name, country_id, a1_governorate_name, a1_governorate,
         a2_district_name, a2_district, a3_sub_district_name, a3_sub_district, a3_sub_district,a4_site_name3, everything())

# Delete unneccessary site columns
clean_data <- clean_data_mapped %>% select(-c("a4_site_name2", "a4_site_name", "deviceid", "subscriberid", "imei","calca11", contains("phone")))
pii <- clean_data_mapped %>% select(uuid, c("a4_site_name2", "a4_site_name", "deviceid", "subscriberid", "imei","calca11", contains("phone")))

# clean_data <- anonymise_dataset(clean_data, c("comments_001", "a4_site_name2", "a4_site_name", "deviceid", "subscriberid", "imei", "phonenumber", "_validation_status",
#                                               "d2_2_second_most_common_district_of_idp_origin_001", "d2_2_most_common_governorate_of_idp_origin", "d2_governorate_of_idp_origin",
#                                               "comments", "__version__", "calca11", "country_name", "country_id"))

# Rename site columns to match the DB file
clean_data <- clean_data %>%
  setnames(old = c("a4_site_name3", "new_site_id", "a1_governorate", "a2_district", "a3_sub_district"), 
           new = c("a4_site_name", "a4_site_code", "a1_governorate_code", "a2_district_code", "a3_sub_district_code"),
           skip_absent = T)

# # Rename site columns to match the DB file
# names(clean_data)[names(clean_data) == "a4_site_name3"] <- "a4_site_name"
# names(clean_data)[names(clean_data) == "new_site_id"] <- "a4_site_code"
# names(clean_data)[names(clean_data) == "a1_governorate"] <- "a1_governorate_code"
# names(clean_data)[names(clean_data) == "a2_district"] <- "a2_district_code"
# names(clean_data)[names(clean_data) == "a3_sub_district"] <- "a3_sub_district_code"

## Prepare dashboard-ready file
dashboard <- clean_data %>% select(-c("country_name", "country_id")) %>%
  mutate(a9_Site_Classification=" ", a4_site_code = " ") %>%
  setnames(old = c("a1_governorate_name", "X_id", "uuid", "X_submission_time"),
           new = c("a1_governorate", "_id", "_uuid", "_submission_time"),
           skip_absent = T)

# dashboard$a9_Site_Classification <- " "
# dashboard$a4_site_code <- " "
# dashboard$country_name <- NULL
# dashboard$country_id <- NULL

# names(dashboard)[names(dashboard) == "a1_governorate_name"] <- "a1_governorate"
# names(dashboard)[names(dashboard) == "X_id"] <- "_id"
# names(dashboard)[names(dashboard) == "uuid"] <- "_uuid"
# names(dashboard)[names(dashboard) == "index"] <- "_index"
# names(dashboard)[names(dashboard) == "X_submission_time"] <- "_submission_time"

db_rename <- select(dashboard, -c("q0_4_date", "a5_1_gps_longitude", "a5_2_gps_latitude", "a6_site_occupation_date_dd_mm_yy", 
                                  "a7_site_population_hh", "a7_site_population_individual", "a1_governorate", "a1_governorate_code",
                                  "a2_district_name", "a2_district_code", "a3_sub_district_name", "a3_sub_district_code","a4_site_code", "a4_site_name")) %>%
  as.data.frame
db_rename[] <- choices$label..english[match(unlist(db_rename), choices$name)] # Makes sure what this does with Dami. It seems to filter the whole data frame to keep only responses that are part of choices$names
# For now as it kills all site columns that don't match with choices.

db_norename <- select(dashboard, c("q0_4_date", "a5_1_gps_longitude", "a5_2_gps_latitude", "a6_site_occupation_date_dd_mm_yy", 
                                   "a7_site_population_hh", "a7_site_population_individual", "a1_governorate", "a1_governorate_code",
                                   "a2_district_name", "a2_district_code", "a3_sub_district_name", "a3_sub_district_code",
                                   "a4_site_name", "a4_site_code"))

final_dashboard <- cbind(db_rename, db_norename)

final_dashboard$a8_population_groups_other_than_idps_in_site_select_all_applicable <- str_replace_all(final_dashboard$a8_population_groups_other_than_idps_in_site_select_all_applicable, pattern = " ", replacement = " - ")
final_dashboard$a8_population_groups_other_than_idps_in_site_select_all_applicable <- str_replace_all(final_dashboard$a8_population_groups_other_than_idps_in_site_select_all_applicable, pattern = "_", replacement = " ")
final_dashboard$a4_site_code <- str_replace_all(final_dashboard$a4_site_code, pattern = "_", replacement = " - ")

final_dashboard <- final_dashboard[moveme(names(final_dashboard), "a1_governorate after q1_3_key_informat_mobile_number; a1_governorate_code after a1_governorate;
                                          a2_district_name after a1_governorate_code; a2_district_code after a2_district_name; a3_sub_district_name after a2_district_code;
                                          a3_sub_district_code after a3_sub_district_name; a4_site_name after a3_sub_district_code; a4_site_code after a4_site_name")]

final_dashboard <- final_dashboard[moveme(names(final_dashboard), "q0_4_date after q0_3_organization_other; a5_1_gps_longitude after a5_gps_coordinates; 
                                          a5_2_gps_latitude after a5_1_gps_longitude; a6_site_occupation_date_dd_mm_yy after a5_2_gps_latitude; 
                                          a7_site_population_hh after a6_site_occupation_date_dd_mm_yy; a7_site_population_individual after a7_site_population_hh;
                                          a9_Site_Classification after a9_formal_informal")]
 
write.xlsx(final_dashboard, paste0("./output/dashboard/CCCM_Site Reporting_V2_",today,".xlsx"))

#write.xlsx(final_dataset, paste0("./output/CCCM_SiteReporting_Week 1 Cleaned_",today,".xlsx"))

### Save Internal and External version of the files
#### After data is cleaning update the dataset by adding admin units names

#### INTERNAL
final_dataset_internal <- clean_data %>% select(-c("start", "end", "q0_1_enumerator_name", "q0_2_gender", "q1_1_key_informant_name", "q1_2_key_informat_gender", "a5_gps_coordinates",
                                                   "q0_3_organization_other", "a4_other_site", "q0_4_date", "b2_exu_fp_name", "b8_community_committee_fp_name", "b9_community_committee_fp_cell", 
                                                   contains("number")))
# final_dataset_internal <- anonymise_dataset(clean_data, c("start", "end", "q0_1_enumerator_name", "q0_2_gender", "q1_1_key_informant_name",
#                                                     "q1_2_key_informat_gender", "q1_3_key_informat_mobile_number", "a5_gps_coordinates", "_id", "_submission_time",
#                                                     "q0_3_organization_other", "a4_site_name2", "comments", "comments_001", "a4_other_site", "q0_4_date", "b2_exu_fp_name",
#                                                     "b3_exu_fp_mobile_number", "b5_smc_agency_fp_name", "b6_smc_agency_fp_mobile_number", "b8_community_committee_fp_name", "b9_community_committee_fp_cell"))

write.xlsx(final_dataset_internal, paste0("./output/internal/CCCM_SiteReporting_V2 Internal_",today,".xlsx"))

#### EXTERNAL
final_dataset_external <- clean_data %>% select(-c("start", "end", "q0_1_enumerator_name", "q0_2_gender", "q1_1_key_informant_name", "q1_2_key_informat_gender", "a5_gps_coordinates", 
                                                   "q0_3_organization_other", "a4_other_site", "a5_1_gps_longitude", "a5_2_gps_latitude", "b2_exu_fp_name", "b2_exu_fp_name", "b8_community_committee_fp_name", 
                                                   "b9_community_committee_fp_cell", "q0_3_organization", "q0_4_date", "b7_community_committee_in_place"), contains("number"))

# final_dataset_external <- anonymise_dataset(clean_data, c("start", "end", "q0_1_enumerator_name", "q0_2_gender", "q1_1_key_informant_name",
#                                                              "q1_2_key_informat_gender", "q1_3_key_informat_mobile_number", "a5_gps_coordinates", "__version__", "_id", "_submission_time", "_validation_status",
#                                                              "q0_3_organization_other", "a4_site_name2", "comments", "comments_001", "a4_other_site", "a5_1_gps_longitude", "a5_2_gps_latitude", "b2_exu_fp_name",
#                                                              "b2_exu_fp_name", "b3_exu_fp_mobile_number", "b5_smc_agency_fp_name", "b6_smc_agency_fp_mobile_number", "b8_community_committee_fp_name", "b9_community_committee_fp_cell",
#                                                              "q0_3_organization", "q0_4_date", "b3_smc_agency_fp_name", "b4_smc_agency_fp_mobile_number", "b6_community_committee_fp_name", "b7_community_committee_fp_cell in external",
#                                                              "b4_site_smc_agency_name", "b7_community_committee_in_place"))

write.xlsx(final_dataset_external, paste0("./output/external/CCCM_SiteReporting_V2 External_",today,".xlsx"))    

