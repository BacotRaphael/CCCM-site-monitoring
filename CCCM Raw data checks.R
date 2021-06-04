# CCCM Site Monitoring Tool - Data Cleaning script
# REACH Yemen - raphael.bacot@reach-initiative.org
# V10
# 03/06/2021

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
today <- Sys.Date()

## Install/Load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, openxlsx, reshape2,sf, leaflet, readxl, withr, mapview, randomcoloR)
p_load_gh("mabafaba/cleaninginspectoR","impact-initiatives-research/clog")

## Source
source("./R/cleanHead.R")
source("./R/check_time_RBA.R")
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

## Load site_name masterlist to match admin names with site_name
setwd("..")                                                                     # To be updated depending on where you put it
masterlist <- read.xlsx(sitemasterlist.filename) %>%                            
  setNames(gsub("\\.", "_", colnames(.)))
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Tool columns comparison
# responsev1 <- read.xlsx(rawdata.filename.v1)
# responsev2 <- read.xlsx(rawdata.filename.v2)
# colv1 <- colnames(responsev1)
# colv2 <- colnames(responsev2)
# col.all <- unique(colv1, colv2)

## Load data to be cleaned
response <- if (tool.version == "V1") {read.xlsx(rawdata.filename.v1)} else if (tool.version == "V2") {read.xlsx(rawdata.filename.v2)} else {print("invalid tool entered, should be either V1 or V2")}
response <- response %>%
  mutate(id = 1:n(), .before = "deviceid") %>%
  dplyr::rename(index = '_index', uuid = '_uuid') %>%
  setNames(tolower(colnames(.)))                                                # reduce to all lowercase

## Remove group name
#response <- cleanHead(response) ## This will gives duplicate colnames, so rather update the tool 
#response <- cleanHead(response)

## Anonymize dataset - Update sensible columns vector by adding all new headers containing sensible information that is not needed for data followup.
# It will ignore non matching columns, just put all columns from both tools
sensible.columns <- c("deviceid", "subscriberid", "imei", "phonenumber", "q0_1_enumerator_name", "q0_2_gender", "q1_1_key_informant_name",
                      "q1_2_key_informat_gender", "q1_3_key_informat_mobile_number", "a5_gps_coordinates", "calca11", "__version__", "_id", "_submission_time", "_validation_status")
PII <- response %>%                                                             # personnally identifiable information data.frame 
  select(uuid, any_of(sensible.columns))                                        # any_of() ensures taking only the columns that exist in resposne
# PII %>% write.csv("/output/pii.csv")
response <- response %>%                                                        
  select(-any_of(sensible.columns))

## Load survey questions from kobo tool
tool <- if (tool.version == "V1") {read.xlsx(tool.filename.v1, sheet = "survey")} else if(tool.version == "V2") {read.xlsx(tool.filename.v2, sheet = "survey")} else {print("invalid tool entered, should be either V1 or V2")}
## Load survey choices Using kobo to rename the site name and than merge the other column
choices <- if (tool.version == "V1") {read.xlsx(tool.filename.v1, sheet = "choices")} else if(tool.version == "V2") {read.xlsx(tool.filename.v2, sheet = "choices")} else {print("invalid tool entered, should be either V1 or V2")}
external_choices <- if (tool.version == "V1") {read.xlsx(tool.filename.v1, sheet = "external_choices")} else if (tool.version == "V2") {read.xlsx(tool.filename.v2, sheet = "external_choices")} else {print("invalid tool entered, should be either V1 or V2")}
external_choices_site <- external_choices %>%
  filter(list_name == "sitename") %>% dplyr::rename(a4_site_name = name)
choices_all <- bind_rows(choices, external_choices) %>%                         # For cleaning log functions
  rbind(c("sitename", "other", "Other", "أخرى", NA, NA))

### Initialize cleaning log
coldf <- c("uuid", "q0_3_organization", "a4_site_name")
col.cl <- c("uuid", "agency", "area", "variable", "issue", "check_id", "old_value", "new_value", "fix", "checked_by")
cleaning.log <- initialise.cleaning.log()

## Check 1: check that there are no site surveyed twice
n_occur <- response %>%                                                         # Check duplicates sitenames
  group_by(a4_site_name) %>%
  filter(a4_site_name!="other") %>%                                             # Filtering out "other" sitenames entry => should be cleaned beforehand
  summarise(Freq = n())
duplicate.site.names <- n_occur %>% filter(Freq>1) %>% select(a4_site_name)
duplicate.site.names
check_duplicate_sites <- response %>%
  mutate(flag = ifelse(a4_site_name %in% duplicate.site.names, T, F),
         agency=q0_3_organization, area=a4_site_name)
## Add to the cleaning log 
add.to.cleaning.log(checks = check_duplicate_sites, check_id = "1", question.names = c("a4_site_name"), issue = "Duplicated site name")
if (nrow(check_duplicate_sites %>% filter(flag))==0) {print ("No sites with identical names have been detected. The dataset seems clean.")}
if (nrow(check_duplicate_sites %>% filter(a4_site_name=="other"))>0) {print(paste0("There are ", sum(response$a4_site_name == "other"), " sitenames recorded as other. Check later!"))}

#write.csv(duplicate_sites, paste0("./output/duplicated_sites_",today,".csv"), row.names = F)
#browseURL(paste0("./output/duplicated_sites_",today,".csv"))  

# Check 2: Try to match site name entered in arabic as other entry with masterlist using different approaches

# A. Cleaning specific patterns from arabic sitename in the masterlist: 
# pattern_english <- c("site", "school", "camp", "mosque", "center", "hospital", "souq", "valley", "building", "city", "street", "neighbourhood")
pattern_arabic <- c("ال","آل","وادي","موقع","مدرسة","مخيم","مركز","مستشفى","سوق","مبنى","حي ","مدينة","مسجد ")
replacement <- rep("", length(pattern_arabic))
# Trying to harmonise different arabic writings + remove village
masterlist <- masterlist %>%
  mutate(Site_Name_In_Arabic_tidy = str_replace_all(Site_Name_In_Arabic, setNames(replacement, pattern_arabic)))

# B. Comparing left_join with perfect match with left_join with arabic names tidy ("cleaned from patterns") as well as partial_match
response2 <- response %>% 
  select(matches("site_name|other_site"), everything()) %>%
  left_join(masterlist %>%                                                      # Perfect match with arabic sitename in masterlist
              select(Site_ID, Site_Name, Site_Name_In_Arabic),
            by = c("a4_other_site" = "Site_Name_In_Arabic")) %>%
  left_join(masterlist %>%                                                      # Perfect match with arabic names tidy (cleaned from patterns)
              select(Site_ID, Site_Name, Site_Name_In_Arabic, Site_Name_In_Arabic_tidy) %>%
              setnames(paste0(colnames(.),"_tidy")) %>% dplyr::rename(Site_Name_In_Arabic_before_tidy = Site_Name_In_Arabic_tidy), 
            by = c("a4_other_site" = "Site_Name_In_Arabic_tidy_tidy")) %>%
  partial_join(x = ., y = masterlist %>%                                        # Splitting sitename in arabic by space as separator, and returning all sitenames that any match part of the sitename in arabic
                 select(Site_ID, Site_Name, Site_Name_In_Arabic) %>%            # WARNING THIS STEP MIGHT GENERATE DUPLICATE MATCHES, FILTER THEM OUT MANUALLY 
                 setnames(paste0(colnames(.),"_partial")),
               pattern_x = "a4_other_site", by_y = "Site_Name_In_Arabic_partial") %>%  
  relocate(Site_ID, Site_Name, Site_ID_tidy, Site_Name_tidy, Site_ID_partial, Site_Name_partial, .before = "a4_other_site")

response2 <- response2 %>%                                                      # Apply changes for perfect matches
  mutate(a4_site_name = ifelse((is.na(a4_site_name) | a4_site_name=="other") & !is.na(Site_ID), Site_ID, a4_site_name))
  
duplicate_match <- response2 %>%
  group_by(uuid, a4_other_site) %>% 
  filter(n()>1 & !is.na(Site_Name_In_Arabic_partial)) %>% ungroup               # display the duplicate sitename found with the partial match approach

site.id.to.keep <- c("YE1517_0262", "YE2613_1417")                              # After looking in the duplicate match data frame, write in this vector the id to be kept [there should be as many id as unique uuid]
site.id.to.throw <- setdiff(duplicate_match$Site_ID_partial, site.id.to.keep)   # Duplicate site ids that will be filtered out
# Check that you didn't do rubbish above
if (sum(site.id.to.keep %in% duplicate_match$Site_ID_partial) < length(site.id.to.keep)) {
  print(paste0("Select site id that are in the list of partial matches. (", duplicate_match$Site_ID_partial, ")"))
  } else if (length(site.id.to.keep) != (duplicate_match$uuid %>% unique %>% length)) {
    print(paste0("You should select as many site id as surveys entries that have multiple match, there are ", length(unique(duplicate_match$uuid)), " sites id to be kept."))}

response2 <- response2 %>% 
  filter(!Site_ID_partial %in% site.id.to.throw)                                # Filter out duplicate sitename matches

check_site_name <- response2 %>%                                                # Flag, filter and categorize sitename issues
  mutate(flag = ifelse(!(a4_site_name %in% external_choices_site$a4_site_name) | a4_site_name == "other" | is.na(a4_site_name), T, F),
         sum.na.match = rowSums(across(matches("Site_Name|Site_ID"), ~is.na(.))),
         issue = ifelse(flag == T & a4_site_name != "other", "Issue with entered sitename. The entered P-code site is not present in the kobo list",
                                       ifelse(flag == T & a4_site_name == "other" & sum.na.match == 6, "Issue with entered sitename. No potential match were found for the other sitename entered in Arabic.",
                                                ifelse(flag == T & a4_site_name == "other" & sum.na.match < 5, "The entered site name is not present in the kobo list. Potential matches to be checked in the columns Site_Name/ID", "Issue with entered sitename."))),
         agency=q0_3_organization, area=a4_site_name)

## C. Add flagged sitenames to the cleaning log
add.to.cleaning.log(checks = check_site_name, check_id = "2",
                    question.names = "a4_site_name", 
                    issue = "issue",
                    add.col = c("a4_other_site", "Site_ID", "Site_Name", "Site_ID_tidy", "Site_Name_tidy", "Site_ID_partial", "Site_Name_partial"))

## Check 3: Match organisation other names with kobo list
## A. Do a partial match for Partner name in arabic from the external choice list
choices.ngo <- choices %>% filter(list_name == "ngo") %>% 
  select(-governorate, -list_name) %>% setNames(c("ngo_code", "ngo_name_en", "ngo_name_ar"))
response3 <- response2 %>%
  left_join(choices.ngo, 
            by = c("q0_3_organization" = "ngo_code")) %>% 
  left_join(choices.ngo %>% setNames(paste0(colnames(.), "_match_other")) , 
            by = c("q0_3_organization_other" = "ngo_name_ar_match_other")) %>%
  partial_join(x = ., y = choices.ngo %>% setNames(paste0(colnames(.), "_match_other_partial")),
               pattern_x = "q0_3_organization_other", by_y = "ngo_name_ar_match_other_partial") %>%
  select(matches("organization"), matches("^ngo_"), matches("Site_|a4_"), everything())

response <- response3 %>%                                                       # Apply changes for perfect matches
  mutate(q0_3_organization = ifelse(q0_3_organization == "other" & !is.na(ngo_code_match_other), ngo_code_match_other, q0_3_organization),
         ngo_name_en = ifelse(ngo_name_en == "Other" & !is.na(ngo_name_en_match_other), ngo_name_en_match_other, ngo_name_en))

check_ngo <- response3 %>%                                                      # Flag, filter and categorize organisation name issues
  mutate(flag = ifelse(q0_3_organization == "other" & is.na(ngo_code_match_other), T, F),
         issue = ifelse(flag & is.na(ngo_code_match_other) & is.na(ngo_code_match_other_partial),
                        "Issue with ngo code. No potential match were found for the other organisation name entered.",
                        ifelse(flag & !(is.na(ngo_code_match_other) & is.na(ngo_code_match_other_partial)), 
                               "Issue with ngo code. Potential matches to be checked in the columns ngo_code_match_other & ngo_code_match_other_partial", NA)),
         agency=q0_3_organization, area=a4_site_name)

## B. Add flagged organization names to the cleaning log
add.to.cleaning.log(checks = check_ngo, check_id = "3",
                    question.names = "q0_3_organization", 
                    issue = "issue",
                    add.col = c("ngo_code_match_other", "ngo_name_en_match_other", "ngo_code_match_other_partial", "ngo_name_en_match_other_partial"))
rm(response2,response3)

### Check 4 & 5: GPS Coordinates check

## 0 Importing layers
adm1 <- st_read(dsn = "data/shapes/yem_adm_govyem_cso_ochayemen_20191002_GDB.gdb", layer="yem_admbnda_adm1_govyem_cso")
adm2 <- st_read(dsn = "data/shapes/yem_adm_govyem_cso_ochayemen_20191002_GDB.gdb", layer="yem_admbnda_adm2_govyem_cso")
adm3 <- st_read(dsn = "data/shapes/yem_adm_govyem_cso_ochayemen_20191002_GDB.gdb", layer="yem_admbnda_adm3_govyem_cso") # Importing the sbd boundaries
adm3_loc <- st_read(dsn = "data/shapes/yem_adm_govyem_cso_ochayemen_20191002_GDB.gdb", layer="yem_admbndp_adm3_govyem_cso")
# admin4 <- read.xlsx("data/shapes/yem_administrative_levels_122017.xlsx", sheet = "main_locality_standard_v0") %>%
#   dplyr::rename(admin1Pcode = admin1pcode, admin2Pcode = admin2pcode, admin3Pcode = admin3pcode, admin4Pcode = admin4pcode)
# admin4 <- admin4 %>% mutate(geometry = mapply(c, longitude, latitude, SIMPLIFY = F) %>% map(st_point)) %>% filter(!(longitude == 0 | latitude == 0))

# 1 Joining site location to boundaries
df.loc <- response %>% mutate(longitude = a5_1_gps_longitude %>% as.numeric,
                              latitude = a5_2_gps_latitude %>% as.numeric ,
                              geometry = mapply(c, longitude, latitude, SIMPLIFY = F) %>% map(st_point)) 

# 2 Plotting map of non cleaned location, setting as NA any non valid DD GPS coordinate
m <- leaflet() %>% addTiles() %>%
  addMarkers(data = df.loc, lng = ~longitude, lat = ~latitude,
             label = paste0("Site name: ", df.loc$a4_site_name, ", lon: ", df.loc$longitude, ", lat: ", df.loc$latitude)) 
m

## Check 4: Sanitizing gps coordinates entries
## 4.1 Convert to DD when possible and flag gps coordinates syntax issues
response <- clean.gps(response, "a5_1_gps_longitude", "a5_2_gps_latitude")

## 4.2 Flag GPS coordinates that lies in a different sub-district than the one entered - [st_intersection]
response <- response %>%                                                        # Match admin3 pcode with corresponding english name 
  left_join(adm3 %>% select(admin3Pcode, admin3Name_en) %>% dplyr::rename(admin3Name_en_df=admin3Name_en), by = c("a3_sub_district" = "admin3Pcode"))

empty.gps.sites <- response %>%
  filter(is.na(Longitude_clean) | is.na(Latitude_clean) | Longitude_clean == 0 | Latitude_clean == 0)

### Check in which admin boundaries the GPS location is supposed to fall
response.sf <- response %>%
  dplyr::mutate(admin2Pcode_df = a2_district,
                admin3Pcode_df = a3_sub_district) %>%                           # Rename (sub)district column to know it shows district information as in survey (df)               
  filter(!is.na(Longitude_clean) & !is.na(Latitude_clean) &
           Longitude_clean != 0, Latitude_clean != 0) %>%
  mutate(SHAPE = mapply(c, as.numeric(Longitude_clean), as.numeric(Latitude_clean), SIMPLIFY = F) %>%
           map(st_point)) %>%                                                   # Add a geometry column to the response dataframe
  st_sf(crs = 4326, sf_column_name = "SHAPE")
response.df <- response.sf %>% st_drop_geometry                                 # Drop the geometry column to be able to do intersect/non-intersection between the two

## Get match original data with the sbd boundaries corresponding to the entered GPS location
valid.gps.sf <- st_intersection(response.sf, adm3) %>% st_as_sf()               # will subset to only keep gps entries falling in adm3 existing boundaries [nrows will be lower or equal to original file nrows]
valid.gps.df <- valid.gps.sf %>% st_drop_geometry %>%                           # Drop the geometry column to be able to do intersect/non-intersection between the two
  select(intersect(colnames(valid.gps.sf), colnames(response.df)))              # select the original df's columns to be able to use setdiff()

## => list the GPS location that falls outside of Yemen boundaries // Here we used the cleaned gps coordinates so it should be zero.
##  => Displays the gps entries outside of all adm3 boundaries [shows rows that disappeared when doing st_instersect i.e.]
non.valid.gps.entries <- 
  setdiff(response.df %>% select(intersect(colnames(response.df), colnames(valid.gps.df))),
          valid.gps.df) 
response.df <- response.df %>%
  mutate(issue.gps = ifelse(uuid %in% non.valid.gps.entries$uuid,
                            "The gps location falls outside of Yemen boundaries",
                            issue.gps))

## => list all entries that have wrong admin names [column a2_district in original data not corresponding to the boundaries in which GPS location falls]
## Can add this after joining the admin level from site_name list
valid.gps.sf <- valid.gps.sf %>%
  mutate(issue.gps = ifelse(admin3Pcode_df != admin3Pcode,
                            "The sub-district name entered is not corresponding to the gps location entered",
                            issue.gps))
response.df <- response.df %>%
  left_join(valid.gps.sf %>% st_drop_geometry %>%
              select(uuid, admin2Pcode, admin2Name_en, admin3Pcode, admin3Name_en) %>% 
              rename_at(vars(-matches("uuid")), ~paste0(., ".gps.matched")), by = "uuid")
response.df <- response.df %>%
  mutate(issue.admin3 = ifelse(admin3Pcode.gps.matched != admin3Pcode_df,
                               "The sub-district name entered is not corresponding to the gps location entered",
                               ""))
response.with.gps <- valid.gps.sf %>% 
  bind_rows(non.valid.gps.entries %>%
              left_join(response.sf %>% select(a5_1_gps_longitude, a5_2_gps_latitude, SHAPE),
                        by = c("a5_1_gps_longitude", "a5_2_gps_latitude"))) 

## Test map to display gps points that have issues  
df.adm2 <- adm2 %>% st_join(response.with.gps %>% st_as_sf() %>%
                              select(issue.gps)) %>%
  mutate(has.gps.issue = ifelse(!is.na(issue.gps), 1, 0))
# df.adm3 <- adm3 %>% st_join(response.with.gps %>% st_as_sf %>% select(issue.gps)) %>%
# mutate(has.gps.issue = ifelse(!is.na(issue.gps), 1, 0))
df.adm_loc <- response.with.gps %>% filter(!is.na(issue.gps)) %>% st_as_sf()
pal.adm2 <- function(x) return(ifelse(x!=0, "indianred", "ghostwhite"))

map <- leaflet() %>%
  addPolygons(data=df.adm2, color = "#B5B5B5", weight = 2, opacity = 0.5,
              highlightOptions = highlightOptions(color = "white", weight = 2), fillOpacity = 0.5,
              fillColor = ~pal.adm2(df.adm2$has.gps.issue),
              label = df.adm2$admin2Name_en) %>%
  addMarkers(data = df.adm_loc, 
             label = paste0("Site name: ", df.adm_loc$a4_site_name," - The admin name entered in dataset is ", df.adm_loc$admin3Name_en_df, ",\r\nactual district according to GPS location is: ", df.adm_loc$admin3Name_en)) %>%
  addTiles()

map                                                                             # Display map
withr::with_dir("./output/",map %>% mapshot(url="sitemap_cleaned_gps.html"))    # Export map as html file

check_gps <- response.df %>%                                                    # Flagging all gps issues and reworking format to include in cleaning log
  mutate(flag = ifelse(!(is.na(issue.gps) | issue.gps == ""), T, F),
         agency=q0_3_organization, area=a4_site_name) %>%
  dplyr::rename(issue = issue.gps)
if (nrow(check_gps %>% filter(flag)) == 0) print("No issues with GPS coordinates have been detected. The dataset seems clean.") else {
  print("GPS coordinates issues have been detected. To be checked")}

## Add check 4 to the cleaning log 
add.to.cleaning.log(checks = check_gps, check_id = "4",
                    question.names = c("a5_1_gps_longitude", "a5_2_gps_latitude"), 
                    issue = "issue",
                    add.col = c("Longitude_clean", "Latitude_clean"))

check_admin_gps <- response.df %>%                                              # Flagging all admin3/Gps that don't match + reworking format to include in cleaning log
  mutate(flag = ifelse(!(is.na(issue.admin3) | issue.admin3 == ""), T, F),
         agency=q0_3_organization, area=a4_site_name) %>%
  dplyr::rename(issue = issue.admin3)
if (nrow(check_admin_gps %>% filter(flag)) == 0) print("GPS coordinates falls within the Sub-district entered in dataset, no issues detected.") else {
  print("Some GPS coordinates don't fall in the entered sub-district. To be checked")}

## Add check 5 to the cleaning log 
add.to.cleaning.log(checks = check_admin_gps, check_id = "5",
                    question.names = c("a3_sub_district", "a5_1_gps_longitude", "a5_2_gps_latitude"), 
                    issue = "issue",
                    add.col = c("admin3Pcode.gps.matched", "admin3Name_en_df", "admin3Name_en.gps.matched", "Longitude_clean", "Latitude_clean"))

#write.csv(wrong_lat_long, paste("./output/wrong_lat_long_",today,".csv"), row.names = F)
#browseURL(paste("./output/wrong_lat_long_",today,".csv"))

## Check: run cleaninginspector => doesn't bring any additionnal [don't use the other, no numerical columns to be checked and duplicates done above]
# response_issue <- inspect_all(response, "uuid") %>% 
#   filter(!grepl("'other' response. may need recoding|Potentially sensitive information.", issue_type))
# issue_log <- initialise.cleaning.log()
# if(nrow(response_issue)>=1) {
#   issue_table <- response_issue %>% mutate(uuid=response[.$index,"uuid",drop=TRUE],
#                                            q0_3_organization=response[.$index,"q0_3_organization", drop=TRUE],
#                                            a4_site_name3 = response[.$index, "a4_site_name3", drop = TRUE])
#   issue_log <- cleaning.log.new.entries(issue_table, var="variable", issue_type="issue_type")
# } else {print("No outliers issues have been detected. The dataset seems clean.")}
#write.csv(response_issue, paste0("./output/dataset_issues_",today,".csv"), row.names = F)
#browseURL(paste0("./output/dataset_issues_",today,".csv"))

## Check 6: Check adequacy
### Check that a service defined as a priority need is not classified as adequate
adequacy_log <- priority.need.checks(response)
if(nrow(adequacy_log)==0) {print("No issues across avilability and service provisions have been detected. The dataset seems clean.")} else {
  print("Issues across avilability and service provisions have been detected. To be checked")}

# Add the adequacy issues to general cleaning log
cleaning.log <- bind_rows(cleaning.log, adequacy_log)                                                    

#write.csv(adequacy_log, paste0("./output/adequacy_issues_",today,".csv"), row.names = F)
#browseURL(paste0("./output/adequacy_issues_",today,".csv"))

### Check 7: Phone number 
### Flag all phone numbers in the data that don't start with the correct phone code for Yemen
phone.col <- c("phonenumber", "q1_3_key_informat_mobile_number", "b3_exu_fp_mobile_number", "b3_SCMCHAIC_fp_number",  "b6_cccm_agency_fp_mobile_number", "b4_smc_agency_fp_mobile_number", "b6_smc_agency_fp_mobile_number", "b5_smc_agency_fp_mobile_number")
# phone.col <- colnames(response)[grepl("phone|number|mobile", colnames(response))]
# v <- c(colnames(responsev1)[grepl("mobile|number", colnames(responsev1))], colnames(responsev2)[grepl("mobile|number", colnames(responsev2))]) %>% unique
# v %in% phone.col                                                                # check that we have all columns with phone numbers from both tools
phonenumber <- response %>% 
  select("uuid","q0_3_organization","a4_site_name", any_of(phone.col)) %>%
  setnames(old = "phonenumber", new = "phone.number", skip_absent = T) %>%
  setnames(gsub("_number", ".number", colnames(.)))

check_phonenumber <- phonenumber %>%
  mutate(across(contains("number"),                                             
                ~ifelse(grepl("^[70|71|73|77|79]",.) | is.na(.), 0, 1),         # Create a binary column with 1 for all non NA phone numbers that don't start with 70 or 71 or etc...
                .names = "{col}_wrong_number"))

phone_log <- check_phonenumber %>%                                             # Reformat for cleaning log
  mutate_at(vars(matches("number")), as.character) %>% 
  pivot_longer(cols = colnames(.)[grepl("number", colnames(.))],
               names_to = c("variable", ".value"), names_pattern = "(.*)\\.(.*)") %>%
  mutate(variable = gsub("phone_number" ,"phonenumber" , paste0(variable,"_number")),
         issue = "The phone number provided may contain errors", check_id="7",
         old_value = number, new_value = "", 
         fix="Checked with partner", checked_by="ON") %>% 
  filter(number_wrong_number==1) %>% select(-number_wrong_number)

if (nrow(phone_log)==0) {print("No issues with phone numbers. The dataset seems clean.")} else {
  print("Issue with some phone numbers detected. To be checked.")}

# Add phone issues to the general cleaning log
cleaning.log <- bind_rows(cleaning.log, phone_log)                               

### Check 8: Ensure that formal sites have more than 20 HH
check_formal <- response %>%
  mutate(flag = ifelse((a9_formal_informal == "formal" & as.numeric(a7_site_population_hh) < 20), T, F),
         issue = ifelse(flag, "The site has been reported as formal but reported number of households is below 20. To be checked", NA)) %>%
  dplyr::rename(agency=q0_3_organization, area=a4_site_name)

if (nrow(check_formal %>% filter(flag))==0){print("Formal site population size check not needed. The dataset seems clean.")} else {
  print("Formal site population size check needed. Some site with less than 20 HH have been reported as formal. To be checked")}

# Add to the cleaning log
add.to.cleaning.log(check_formal,  check_id = "8", question.names = c("a9_formal_informal", "a7_site_population_hh"), issue = "issue")

#write.csv(formal, paste0("./output/formal_site_population_issue_",today,".csv"), row.names = F)
#browseURL(paste0("./output/formal_site_population_issue_",today,".csv"))

### Check 9: collective centre is not a makeshift or emergency or transitional or open-air type of shelter
check_collective <- response %>%
  mutate(flag = ifelse((c1_type_of_site == "collective_centre" & (c9_primary_shelter_type == "emergency_shelter" | 
                                                                    c9_primary_shelter_type == "makeshift_shelter" |
                                                                    c9_primary_shelter_type == "transitional_shelter" |
                                                                    c9_primary_shelter_type == "open_air_no_shelter")), T, F),
         issue = ifelse(flag, "Primary shelter type marked as 'makeshfit', 'emergency', 'transitional', or 'open-air' although type of site was identified as 'Collective centre'", NA)) %>%
  dplyr::rename(agency=q0_3_organization, area=a4_site_name)

if (nrow(check_collective %>% filter(flag))==0){print("Collective centre type checks not needed. The dataset seems clean.")} else {
  print("Some collective centres have been reported as makeshift or emergency or transitional or open-air type of shelter. To be checked")}

# Add to the cleaning log
add.to.cleaning.log(check_collective, check_id = "9", question.names = c("c1_type_of_site", "c9_primary_shelter_type"), issue = "issue")
#write.csv(collective, paste0("./output/collective_centre_issue_",today,".csv"), row.names = F)
#browseURL(paste0("./output/collective_centre_issue_",today,".csv"))

### Check 10: Waste disposal cannot be "non-existent" if they selected flush latrines
check_waste_disposal <- response %>% 
  mutate(flag = ifelse((waste_disposal_services == "non_existant" & c10_primary_latrine_type == "flush_latrine_to_tank_sewage_system_pit"), T, F),
         issue = ifelse(flag, "Waste disposal marked as 'non-existant' although  'flush latrines' was selected as primary latrine type", "")) %>%
  dplyr::rename(agency=q0_3_organization, area=a4_site_name)

if (nrow(check_waste_disposal %>% filter(flag))==0){print("Waste disposal system checks not needed. The dataset looks clean.")} else {
  print("Some sites have reported having lush latrines while reporting waste disposal as non-existent. To be checked")}

# Add to the cleaning log
add.to.cleaning.log(check_waste_disposal, check_id = "10", question.names = c("waste_disposal_services", "c10_primary_latrine_type"), issue = "issue")
#write.csv(waste_disposal, paste0("./output/waste_disposal_issue_",today,".csv"), row.names = F)
#browseURL(paste0("./output/waste_disposal_issue_",today,".csv"))

### Check 11: that adequate WASH services do not include: flush latrine to the open / open defecation / pit uncovered / illegal water connection / unprotected well / surface water
check_adequate_wash <- response %>%
  mutate(flag = ifelse((wash_services == "adequate" & (c10_primary_latrine_type == "flush_latrine_to_the_open" |
                                                         c10_primary_latrine_type == "open_defecation" |
                                                         c10_primary_latrine_type == "pit_latrine_open" |
                                                         c8_primary_water_source == "illegal_connection_to_piped_network" |
                                                         c8_primary_water_source == "unprotected_well" |
                                                         c8_primary_water_source == "surface_water")), T, F),
         issue = ifelse(flag, "WASH services identified as adequate although water source or primary latrine type was marked as unsafe or unprotected", "")) %>%
  dplyr::rename(agency=q0_3_organization, area=a4_site_name)

if (nrow(check_adequate_wash %>% filter(flag))==0){print("No issues with adequacy of wash services has been detected. The dataset seems clean.")} else {
  print("Some sites identified WASH services as adequate although water source or primary latrine type was marked as unsafe or unprotected. To be checked.")}

# Add to the cleaning log
add.to.cleaning.log(check_adequate_wash, check_id = "11", question.names = c("wash_services", "c10_primary_latrine_type"), issue = "issue")
#write.csv(adequate_wash, paste0("./output/adeqate_facility_issue_",today,".csv"), row.names = F)
#browseURL(paste0("./output/adeqate_facility_issue_",today,".csv"))

### Check 12: Eviction risk should not be reported if there is a tenanacy agreement
check_eviction <- response %>%
  mutate(flag = ifelse((c3_tenancy_agreement_for_at_least_6_months == "yes" & f1_threats_to_the_site.eviction == 1), T, F),
         issue = ifelse(flag, "Eviction was identied as a risk although the site holds a tennacy agreement", "")) %>%
  dplyr::rename(agency=q0_3_organization, area=a4_site_name)

if (nrow(check_eviction %>% filter(flag))==0){print("No issues with eviction and type of tennacy has been detected. The dataset seems clean.")} else {
  print("For some sites, eviction was identied as a risk although the site holds a tennacy agreement. To be checked.")}

# Add to the cleaning log
add.to.cleaning.log(check_eviction, check_id = "12", question.names = c("c3_tenancy_agreement_for_at_least_6_months", "f1_threats_to_the_site.eviction"), issue = "issue")
#write.csv(eviction, paste0("./output/eviction_issue_",today,".csv"), row.names = F)
#browseURL(paste0("./output/eviction_issue_",today,".csv"))

### Check 13: Survey length
### Check lenght of the survey, 15 = minimum, 60 = maximum (can be changed)
check_survey_length <- check.time(response, duration_threshold_lower = 15, duration_threshold_upper = 60) %>%
  mutate(old_value = as.character(old_value), check_id = "13") 
if (nrow(check_survey_length)==0){print(paste0("No survey has been flagged as too short/long with the given parameters entered."))} else {
  print("Some survey lengths are outside of the minimum and maximum duration parameters entered. To be checked.")}

## Add to the cleaning log
cleaning.log <- bind_rows(cleaning.log, check_survey_length) 

cleaning.log <- cleaning.log %>%
  mutate(change = NA) %>%                                                       # adding "change" column to be filled later (will determine whether changes must be done or not)
  arrange(agency, check_id, uuid)                                               

### Saves and put in format cleaning log 
dir.create("output/cleaning log", showWarnings = F)
save.follow.up.requests(cleaning.log, filename.out=paste0("output/cleaning log/cleaning_log_all_", today,".xlsx"))
# browseURL(paste0("output/cleaning log/cleaning_log_all_", today,".xlsx"))

### Split all cleaning log by partner
dir.create("output/cleaning log/partners", showWarnings = F, recursive = T)
for (org in unique(cleaning.log$agency)) {
  cl.org <- cleaning.log %>% filter(agency == org) %>%
    save.follow.up.requests(., filename.out=paste0("output/cleaning log/partners/cleaning_log_", org, "_", today, ".xlsx"))
}

# how to do it in a more beautiful way? ask Dami...
# cleaning.log %>%
#   group_by(agency) %>%
#   group_map(~save.follow.up.requests(., filename.out = paste0("output/cleaning_log_", ,".xlsx")))
# test <- split(cleaning.log, as.factor(cleaning.log$agency))

#### After data is cleaning updated dataset by adding admin units names
## Load cleaned data
#response_clean <- read.csv("./data/CCCM_SiteID_24102019_cleaned.csv", stringsAsFactors = F)

################################################################################
## Annex: For Adding a new logical check:
################################################################################

# check_name <- response %>%                                                      # Whatever name you want to give to your logical check
#   mutate(flag = ifelse((variable.1 == "yes" & variable.2 == 1), T, F),          # Create column flag (TRUE/FALSE) that flag surveys matching the logical condition(s)
#          issue = ifelse(flag, "issue message", NA)) %>%                         # Issue message as it will be displayed in the cleaning log [can also be manually edited in the parameter's function issue = ""]
#   dplyr::rename(agency=q0_3_organization, area=a4_site_name)                    # To streamline headers with standard cleaning log's columns headers
# 
# if (nrow(check_name %>% filter(flag))==0){print("Message if no error. The dataset seems clean.")} else {
#   print("Message highlighting that there is at least one error. To be checked.")}
# 
# # Add to the cleaning log
# # You can have as many conflicting/interlinked variable as you'd like, just write all headers in the question.names parameter as vector.
# add.to.cleaning.log(check_name,  
#                     check_id = "XX",                                            # For coloring cells in cleaning log when multiple rows are linked with same issue. Write the number/code for the additionnal check_id => need to be non empty and not the same than previous one. pick the next number ideally.
#                     question.names = c("var1", "var2", "var3"),                 # Exact column headers corresponding to all variables that potentially needs updating - at least one column must be specified 
#                     issue = "issue",                                            # if "issue", it will look for a column named "issue" in check_name. You can manually edit the issue also by writing direct text between ""
#                     add.col = c(""))                                            # If for some reason, the logical check require a specific additionnal column to be more self explanatory. Avoid in general, it will make the cleaning log messy
#  
################################################################################
# Archived code: 
################################################################################

## Anonymize dataset
#response <- anonymise_dataset(response, c("deviceid", "subscriberid", "imei", "phonenumber", "q0_1_enumerator_name", "q0_2_gender", "q1_1_key_informant_name",
#"q1_2_key_informat_gender", "q1_3_key_informat_mobile_number", "a5_gps_coordinates", "calca11", "__version__", "_id", "_submission_time", "_validation_status"))

# Sitename matching
# response2$site_name_ar_pmatch <- external_choices_site$`label::english`[pmatch(response$a4_other_site, external_choices_site$`label::arabic`)]
# response$a4_site_name2 <- external_choices_site$`label::english`[match(response$a4_other_site, external_choices_site$`label::arabic`)]
# response <- response %>% mutate(a4_site_name3 = ifelse(!is.na(a4_site_name2), as.character(a4_site_name2), a4_other_site))

## OLD GPS CHECK 
# ### GPS coordinates - Mapping and check
# # load layers from gdb
# adm1 <- st_read(dsn = "data/shapes/yem_adm_govyem_cso_ochayemen_20191002_GDB.gdb", layer="yem_admbnda_adm1_govyem_cso")
# adm2 <- st_read(dsn = "data/shapes/yem_adm_govyem_cso_ochayemen_20191002_GDB.gdb", layer="yem_admbnda_adm2_govyem_cso")
# adm3 <- st_read(dsn = "data/shapes/yem_adm_govyem_cso_ochayemen_20191002_GDB.gdb", layer="yem_admbnda_adm3_govyem_cso") # Importing the sbd boundaries
# adm3_loc <- st_read(dsn = "data/shapes/yem_adm_govyem_cso_ochayemen_20191002_GDB.gdb", layer="yem_admbndp_adm3_govyem_cso")
# admin4 <- read.xlsx("data/shapes/yem_administrative_levels_122017.xlsx", sheet = "main_locality_standard_v0") %>%
#   dplyr::rename(admin1Pcode = admin1pcode, admin2Pcode = admin2pcode, admin3Pcode = admin3pcode, admin4Pcode = admin4pcode)
# admin4 <- admin4 %>% mutate(geometry = mapply(c, longitude, latitude, SIMPLIFY = F) %>% map(st_point)) %>% filter(!(longitude == 0 | latitude == 0))
# 
# # Joining site location to boundaries
# df.loc <- response %>% mutate(a5_1_gps_longitude = a5_1_gps_longitude %>% as.numeric,
#                               a5_2_gps_latitude = a5_2_gps_latitude %>% as.numeric ,
#                               geometry = mapply(c, a5_1_gps_longitude, a5_2_gps_latitude, SIMPLIFY = F) %>% map(st_point)) 
# 
# # Plotting map of non cleaned location, setting as NA any non valid DD GPS coordinate
# m <- leaflet() %>% addTiles() %>%
#   addMarkers(data = df.loc, lng = ~a5_1_gps_longitude, lat = ~a5_2_gps_latitude,
#              label = paste0("Site name: ", df.loc$a4_site_name, ", lon: ", df.loc$a5_1_gps_longitude, ", lat: ", df.loc$a5_2_gps_latitude)) 
# m
# 
# ### Sanitizing longitude and latitude entered coordinates
# response <- clean.gps(response, "a5_1_gps_longitude", "a5_2_gps_latitude")
# 
# ### Check in which admin boundaries the GPS location is supposed to fall
# # Add a geometry column to the response dataframe
# response.sf <- response %>%
#   # dplyr::rename(admin2Pcode_df = Dist_ID) %>%                                 # commented as response has no systematic admin1/2 columns => For now partial matching of arabic site names with masterlist gives limited results. 
#   filter(!is.na(Longitude_clean) & !is.na(Latitude_clean) & Longitude_clean != 0, Latitude_clean != 0) %>%
#   mutate(SHAPE = mapply(c, as.numeric(Longitude_clean), as.numeric(Latitude_clean), SIMPLIFY = F) %>% map(st_point)) %>%
#   st_sf(crs = 4326, sf_column_name = "SHAPE")
# response.df <- response.sf %>% st_drop_geometry                                 # Drop the geometry column to be able to do intersect/non-intersection between the two
# 
# ## Get match original data with the sbd boundaries corresponding to the entered GPS location
# valid.gps.sf <- st_intersection(response.sf, adm2) %>% st_as_sf()               # will subset to only keep gps entries falling in adm3 existing boundaries [nrows will be lower or equal to original file nrows]
# valid.gps.df <- valid.gps.sf %>% st_drop_geometry %>%                           # Drop the geometry column to be able to do intersect/non-intersection between the two
#   select(intersect(colnames(valid.gps.sf), colnames(response.df)))              # select the original df's columns to be able to use setdiff()
# 
# ## Step 1 => list the GPS location that falls outside of Yemen boundaries // Here we used the cleaned gps coordinates so it should be zero.
# ##  => Displays the gps entries outside of all adm3 boundaries [shows rows that disappeared when doing st_instersect i.e.]
# non.valid.gps.entries <- setdiff(response.df %>% select(intersect(colnames(response.df), colnames(valid.gps.df))), valid.gps.df) 
# 
# ## Step 1.2 => Look at the response file that only has the valid GPS entries and if the districts/governorate make sense
# response.valid.gps <- valid.gps.sf %>% select(matches("Pcode|Name_en|site_name3"), matches("longitude|latitude"), matches("issue"), everything()) %>% st_drop_geometry
# 
# ## Step 1.3 Map the sanitized GPS locations and sitenames
# ## Step 3 => Test map to display gps points that have issues
# df.adm2 <- st_join(adm2, valid.gps.sf %>% mutate(covered = 1) )
# pal.adm2 <- function(x) return(ifelse(!is.na(x), "indianred", "ghostwhite"))
# df.adm3 <- valid.gps.sf
# 
# map <- leaflet() %>%
#   addTiles()%>%
#   addMarkers(data = df.adm3, lng = ~Longitude_clean, lat = ~Latitude_clean,
#              label = paste0("The sitename name in tool is ", df.adm3$a4_other_site, ",\r\nmatched name in masterlist is: ", df.adm3$Site_Name_In_Arabic)) %>%
#   addPolygons(data = df.adm2, color = "#B5B5B5", weight = 2, opacity = 0.5,
#               highlightOptions = highlightOptions(color = "white", weight = 2), fillOpacity = 0.5,
#               fillColor = ~pal.adm2(df.adm2$has.gps.issue),
#               label = paste0(df.adm2$admin2Name_en, " district"))
# 
# map

### ISSUE: If response has no admin 2 level, the below lines becomes useless as they aim at highlighing admin2 level columns not corresponding to entered GPS.

## Step 2 => list all entries that have wrong admin names [column admin3RefName_en in original data not corresponding to the boundaries in which GPS location falls]
## Can add this after joining the admin level from site_name list
# issue.list <- valid.gps.sf %>%
#   filter(admin2Pcode_df != admin2Pcode) %>%                                   # here no admin2 as response file had no admin2 column
#   mutate(issue = "The administrative name entered is not corresponding to the gps location entered") %>%
#   plyr::rbind.fill(non.valid.gps.entries %>% left_join(response.sf %>% select(Longitude_clean, Latitude_clean, SHAPE), by = c("Longitude_clean", "Latitude_clean"))) %>%
#   mutate(issue = ifelse(is.na(issue), "The gps location falls outside of Yemen boundaries", issue)) 
# issue.list.df <- issue.list %>%
#   select(c("Site_ID", "Site.Name", "Partner.Name", "Governorate.Name", "admin2Name_en", "District.Name"), Longitude_clean, Latitude_clean, issue, Longitude, Latitude, issue.gps)

## Step 3 => Test map to display gps points that have issues  
# df.adm2 <- adm2 %>% st_join(issue.list %>% st_as_sf() %>% select(issue)) %>%
#   mutate(has.gps.issue = ifelse(!is.na(issue),1,0))
# df.adm3 <- adm3 %>% st_join(issue.list %>% st_as_sf %>% select(issue)) %>%
# mutate(has.gps.issue = ifelse(!is.na(issue),1,0))
# df.adm_loc <- issue.list %>% st_as_sf()
# pal.adm2 <- function(x) return(ifelse(x!=0, "indianred", "ghostwhite"))

# map <- leaflet() %>%
#   addPolygons(data=df.adm2, color = "#B5B5B5", weight = 2, opacity = 0.5, 
#               highlightOptions = highlightOptions(color = "white", weight = 2), fillOpacity = 0.5,
#               fillColor = ~pal.adm2(df.adm2$has.gps.issue),
#               label = df.adm2$admin2Name_en) %>% 
#   addCircles(data = df.adm_loc, stroke = F, fillOpacity = 0.5, radius = 1000,
#              label = paste0("The admin name entered in dataset is ", df.adm_loc$District.Name, ",\r\nactual district according to GPS location is: ", df.adm_loc$admin2Name_en)) %>%
#   addTiles() %>%
#   addMeasure(primaryLengthUnit = "kilometers")
# map                                                                             # display map

# gps_issue_log <- response %>% 
#   select(c("uuid", "q0_3_organization", "a4_site_name3", "a5_1_gps_longitude", "a5_2_gps_latitude", "issue.gps", "Longitude_clean", "Latitude_clean")) %>%
#   filter(!is.na(issue.gps)) %>%
#   setnames(old = c("Longitude_clean", "Latitude_clean", "issue.gps", "q0_3_organization", "a4_site_name3"),
#            new = c("Longitude new value","Latitude new value", "issue", "agency", "area")) %>%
#   mutate(fix="Checked with partner", checked_by = "ON", variable = paste0("a5_1_gps_longitude, a5_2_gps_latitude"))

# if (nrow(gps_issue_log) == 0) print("No issues with GPS coodinates have been detected. The dataset seems clean.")