# CCCM Site Monitoring Tool - Data Cleaning script
# REACH Yemen - raphael.bacot@reach-initiative.org
# V7
# 27/06/2021

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
today <- Sys.Date()

## Install/Load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, openxlsx, reshape2, sf, leaflet, readxl)
# p_load_gh("mabafaba/cleaninginspectoR","agualtieri/cleaninginspectoR","agualtieri/dataqualitycontrol", "impact-initiatives-research/clog")

## Source
source("./R/cleanHead.R")
source("./R/add_locations.R")
source("./R/moveme.R")
source("./R/utils.R")

## Paramaters to be updated each month
## Tool version - uncomment the relevant tool version and comment the other one
tool.version <- "V1"                                                            # V1 or V2 update according to the kobo tool used
#tool.version <- "V2"

## Update the directory for all files each month with the latest version. Beware of getting V1 and V2 right!
rawdata.filename.v1 <- "data/CCCM_Site_Reporting_Raw Data_Master_tbc_V1_Round3-5_incl FMF Round2_2021.xlsx"    # Update the name of the rawdata filename
rawdata.filename.v2 <- "data/CCCM_Site_Reporting_Raw Data_Master_tbc_V2_Round 3-5.xlsx"
tool.filename.v1 <- "data/kobo/CCCM_Site_Reporting_Kobo_tool_V1_23_05_2021_FINAL_all ngo names.xlsx"
tool.filename.v2 <- "data/kobo/CCCM_Site_Reporting_Kobo_tool_V2_23_05_2021_FINAL_all ngo names.xlsx"
# sitemasterlist.filename <- "data/CCCM_IDP Hosting Site List_coordinates verification exercise_March2021.xlsx"
sitemasterlist.filename <- "data/CCCM IDP Sites_making NEW site names and IDs_Aug 2021_22092021.xlsx"
filename.pcodes <- "R/pcodes/yem_admin_ochayemen_20191002.xlsx"

## Tool columns comparison
# responsev1 <- read.xlsx(rawdata.filename.v1)
# responsev2 <- read.xlsx(rawdata.filename.v2)
# colv1 <- colnames(responsev1)
# colv2 <- colnames(responsev2)
# col.all <- unique(colv1, colv2)
# common.col <- intersect(colv1, colv2)
# col.v1.only <- setdiff(colv1, colv2)
# col.v2.only <- setdiff(colv2, colv1)

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
masterlist <- read.xlsx(sitemasterlist.filename) %>%
  setNames(gsub("\\.", "_", colnames(.))) %>%
  setnames(old = c("SITE_ID", "Site_Name", "Site_Name_In_Arabic"),              # Put here whatever names CCCM cluster put in the latest masterlist
           new = c("Site_ID", "Site_Name", "Site_Name_In_Arabic"),              # Streamline colnames to keep the same script.
           skip_absent = TRUE) %>%
  mutate_at(vars(-matches("_Households|_Population|_IDPs")), as.character) %>%  # mutate all names and IDs as character to enable matching
  mutate_at(vars(matches("Newly_|_Households|Population|#_of_total_Households")), ~as.numeric(unlist(lapply(gsub(",", "", .), arabic.tonumber))))

## Load data to be cleaned
response <- if (tool.version == "V1") {read.xlsx(rawdata.filename.v1)} else if (tool.version == "V2") {read.xlsx(rawdata.filename.v2)} else {print("invalid tool entered, should be either V1 or V2")}
response <- response %>% 
  # mutate(a4_site_name = a4_other_site, id = 1:n(), .before = "deviceid") %>%
  dplyr::rename(index = '_index', uuid = '_uuid')

## code to check for duplicated colnames
# colnames(response)[4]
# colnames(response)[2:8] ## displays colnames of response from 2 to 8 elementh
# colnames(response)[grepl("site", colnames(response))] ## displays all column that have the pattern "site_name"
# colnames(response)[duplicated(colnames(response))] ## to access duplicted columns

## Upload updated cleaning log files from partners
files.cleaning.log <- paste0("output/cleaning log/parnter cleaning logs updated/", list.files("output/cleaning log/parnter cleaning logs updated"))
cleaning.log <- data.frame()
for (file in files.cleaning.log){
  cleaning.log <- bind_rows(cleaning.log, read.xlsx(file) %>% mutate_all(as.character))
}
# ### How to debug this particular case
# ## 1 run file to see on which element of the loop it broke
# file
# ## 2 load the cleaning log on its on
# test <- read.xlsx(file)
# ## 3 check for duplicated column names
# colnames(test)[duplicated(colnames(test))]

## Upload internal cleaning log [sitename/organisation name + easy gps changes]
file.cleaning.log.internal <- "output/cleaning log/internal cleaning logs/"
files.internal.cleaning.log <- paste0(file.cleaning.log.internal, list.files(file.cleaning.log.internal))
cleaning.log.int <- data.frame()
for (file in files.internal.cleaning.log){
  cleaning.log.int <- bind_rows(cleaning.log.int, read.xlsx(file) %>% mutate_all(as.character))
}

## Binding internal and external cleaning logs together
cleaning.log <- cleaning.log %>% bind_rows(cleaning.log.int)

## checking for duplicated rows (in case you have twice the same cleaning log in the folder)
if ((cleaning.log %>% group_by(uuid, variable, check_id) %>% filter(n()>1) %>% nrow) > 0) {print("There are duplicates entries (or multiple entries for the same variable) in the cleaning log. Make sure it's ok and that there are no conflicting new_values assigned.")}
duplicate.cl <- cleaning.log %>% group_by(uuid, variable, check_id) %>% filter(n()>1) %>% ungroup %>% arrange(uuid, variable)

### Apply cleaning log changes to raw response file
clean_data <- response 
for (r in seq_along(1:nrow(cleaning.log))){
  col <- as.character(cleaning.log[r, "variable"])                                                                  # Get the name of the variable to be updated in response dataset
  new_value <- as.character(cleaning.log[r, "new_value"])                                                           # Get the new value for this cleaning log entry
  if ((col %in% colnames(clean_data)) & (cleaning.log[r, "change"] %in% c("TRUE"))) {
    clean_data[clean_data$uuid==as.character(cleaning.log[r,"uuid"]), col] <- new_value                             # Assign new value to the variable in the dataframe response
  }
}

### Recode other responses as NA when relevant [sitename + organization other cleaning log entries have been kept out to keep cleaning log concise]
for (r in seq_along(1:nrow(cleaning.log))){
  col <- cleaning.log[r, "variable"]
  new_value <- cleaning.log[r, "new_value"]                                     
  if (col %in% c("a4_site_name")){var.other <- "a4_other_site"} else {var.other <- paste0(col, "_other")}
  if ((var.other %in% colnames(clean_data)) & (cleaning.log[r, "change"] %in% c("TRUE")) & !is.na(new_value)) {
    clean_data[clean_data$uuid==cleaning.log[r,"uuid"], var.other] <- NA        # Set other entry as NA when other has been recoded (i.e. new_value is not NA and change have been applied)
  }
}

## Check if new site ids need to be created 
new.sites <- cleaning.log %>% filter(variable == "a4_site_name") %>% filter(new_site %in% c("TRUE", "T", TRUE, T))
if ((a<-nrow(new.sites))>0){print(paste0(a, " New site ID must be generated."))}

# ## Creation of new site IDs // COMMENT THE WHOLE SECTION IF NO NEED TO GENERATE SITE IDs.
# site.masterlist <- masterlist$Site_ID %>% unique                                  # Make sure all sites are on one sheet or consolidate on one sheet in masterlist
# 
# new.sites <- cleaning.log %>%
#   filter(variable == "a4_site_name") %>%
#   filter(new_site %in% c("TRUE", "T", TRUE, T)) %>%                             # Display site id entered that are not in masterlist or kobo tool
#   filter(!(new_value %in% unique(c(site.masterlist)))) %>% 
#   left_join(clean_data %>% select(uuid, a2_district, a3_sub_district), by = "uuid") %>%
#   relocate(a4_other_site, a4_site_name_new_en, a4_site_name_new_ar, a2_district, a3_sub_district, new_site, .before = "old_value")
# 
# ## Check that cleaning log for new sites has been filled correctly
# check_new_names_filled <- new.sites %>% summarise_at(vars(matches("a4_site_name_new")), ~sum(is.na(.)))
# 
# if (sum(check_new_names_filled > 0)>0){
#   print("the new names in english and arabic are not specified for all new camps! Please update the file below.")
#   new.sites %>% mutate(issue="Check the new names in english and arabic and fill them all. Update new_site with FALSE if some sites don't need new IDs.") %>% write.xlsx(paste0("output/cleaning log/site name/new_sites_corrected_", today, ".xlsx"))
#   browseURL(paste0("output/cleaning log/site name/new_sites_corrected_", today, ".xlsx"))
# }
# ## If relevant, check the names and update them accordingly in the excel file and then update the filename below with _updated at the end.
# file.name.new.sites <- "output/cleaning log/site name/new_sites_corrected_2021-06-30_updated.xlsx"
# if (sum(check_new_names_filled > 0)>0){new.sites <- read.xlsx(file.name.new.sites)}
# 
# # Inspect the new attributed names in English and arabic
# check_names <- new.sites %>% select(matches("a4_site_name_new")) %>% view
# 
# ## Create new sitename code for remaining sites
# max.id.split <- masterlist$Site_ID %>% sub(".*?_", "",.) %>% as.numeric %>% max(na.rm = T) # Extract the highest number in site id in masterlist
# latest_id <- masterlist$Site_ID[grepl(paste0("_", max.id.split), masterlist$Site_ID)]      # Extract the latest full ID
# seq <- (max.id.split+1):(max.id.split+500)                                                 # Create list of numbers starting from the latest one
# 
# new.sites <- new.sites %>% 
#   mutate(a4_site_name_new = paste0(a2_district, "_", seq[1:nrow(.)])) %>%       # create new site ID in column new_site_id of the cleaning log
#   relocate(a4_site_name_new,	a4_site_name_new_en,	a4_site_name_new_ar, .after = "issue")
# new.sites.uuid <- new.sites$uuid
# 
# ## Update the masterlist with new sites entries + ID and available informations
# new.sites <- new.sites %>%
#   left_join(response %>% select(uuid, a7_site_population_hh, a7_site_population_individual, a5_2_gps_latitude, a5_1_gps_longitude), by="uuid") %>%
#   mutate(Partner_Name = agency, Sub_Dist_ID=a3_sub_district, Site_ID = a4_site_name_new, Site_Name=a4_site_name_new_en,
#          Site_Name_In_Arabic=a4_site_name_new_ar, Latitude=a5_2_gps_latitude, Longitude=a5_1_gps_longitude, "#_of_total_Households"=as.numeric(a7_site_population_hh),
#          Total_Site_Population=as.numeric(a7_site_population_individual)) %>% select(intersect(colnames(.), colnames(masterlist)))
# 
# ## Adding the new sites in the existing site masterlist
# masterlist_updated <- masterlist %>% bind_rows(new.sites)                
# dir.create("output/masterlist", showWarnings = F)
# masterlist_updated %>% select(-any_of("Site_Name_In_Arabic_tidy")) %>% write.xlsx(paste0("output/masterlist/masterlist_", today,".xlsx"))

## Add Pcodes
admin3 <- read.xlsx(filename.pcodes, sheet = "admin3") %>%
  dplyr::rename(a3_sub_district_name=admin3RefName_en, a3_sub_district=admin3Pcode,
                a2_district_name=admin2Name_en, a2_district=admin2Pcode,
                a1_governorate_name=admin1Name_en, a1_governorate=admin1Pcode)
clean_data_loc <- clean_data %>% 
  select(-any_of(c("a1_governorate", "a2_district"))) %>%
  left_join(admin3, by=c("a3_sub_district"="a3_sub_district")) %>%
  select(uuid, start, end, today, matches("a1_|a2_"), matches("a3_"), everything()) 

## Delete unneccessary site columns 
pii <- clean_data_loc %>% select(uuid, c("deviceid", "subscriberid", "imei","calca11", contains("phone")))
clean_data <- clean_data %>% 
  select(-c("deviceid", "subscriberid", "imei","calca11", contains("phone"))) %>%   
  setnames(old = c("a4_site_name", "a1_governorate", "a2_district", "a3_sub_district"), 
           new = c("a4_site_code", "a1_governorate_code", "a2_district_code", "a3_sub_district_code"),
           skip_absent = T)                                                     # Rename site columns to match the DB file

### Save Internal and External version of the files
## INTERNAL
final_dataset_internal <- clean_data %>% select(-any_of(c("start", "end", "q0_1_enumerator_name", "q0_2_gender", "q1_1_key_informant_name", "q1_2_key_informat_gender", "a5_gps_coordinates",
                                                          "q0_3_organization_other", "a4_other_site", "q0_4_date", "b2_exu_fp_name", "b8_community_committee_fp_name", "b9_community_committee_fp_cell", 
                                                          contains("number"))))
write.xlsx(final_dataset_internal, paste0("./output/internal/CCCM_SiteReporting_", tool.version," Internal_",today,".xlsx"))

## EXTERNAL
final_dataset_external <- clean_data %>% select(-any_of(c("start", "end", "q0_1_enumerator_name", "q0_2_gender", "q1_1_key_informant_name", "q1_2_key_informat_gender", "a5_gps_coordinates", 
                                                          "q0_3_organization_other", "a4_other_site", "a5_1_gps_longitude", "a5_2_gps_latitude", "b2_exu_fp_name", "b2_exu_fp_name", "b8_community_committee_fp_name", 
                                                          "b9_community_committee_fp_cell", "q0_3_organization", "q0_4_date", "b7_community_committee_in_place", contains("number"))))
write.xlsx(final_dataset_external, paste0("./output/external/CCCM_SiteReporting_", tool.version," External_",today,".xlsx"))    
