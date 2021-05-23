# CCCM Site Monitoring Tool - Data Cleaning script
# REACH Yemen - alberto.gualtieri@reach-initiative.org
# V8
# 09/11/2019

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
today <- Sys.Date()

## Install/Load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, openxlsx, reshape2,sf, leaflet, readxl)
p_load_gh("mabafaba/cleaninginspectoR","impact-initiatives-research/clog")

## Source
source("./R/cleanHead.R")
source("./R/check_time_RBA.R")
source("./R/utils.R")

## Load site_name masterlist to match admin names with site_name
setwd("..")
masterlist <- read.xlsx("CCCM-site-masterlist-cleaning/data/CCCM_IDP Hosting Site List_March 2021.xlsx") %>%
  setNames(gsub("\\.", "_", colnames(.)))
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Load data to be cleaned
response <- read.xlsx("data/CCCM_Site_Reporting_Form_V1_example raw data.xlsx") %>%
  mutate(a4_site_name = a4_other_site, id = 1:n(), .before = "deviceid") %>%
  dplyr::rename(index = '_index', uuid = '_uuid') 

## Remove group name and reduce to all lowercase
names(response) <- tolower(names(response))
#response <- cleanHead(response) ## This will gives duplicate colnames, so rather update the tool 
#response <- cleanHead(response)

## Anonymize dataset
#response <- anonymise_dataset(response, c("deviceid", "subscriberid", "imei", "phonenumber", "q0_1_enumerator_name", "q0_2_gender", "q1_1_key_informant_name",
                                          #"q1_2_key_informat_gender", "q1_3_key_informat_mobile_number", "a5_gps_coordinates", "calca11", "__version__", "_id", "_submission_time", "_validation_status"))

## Load survey choices Using kobo to rename the site name and than merge the other column
choices <- read.xlsx("data/CCCM_Site_Reporting_Kobo_tool_(V1)_12042021.xlsx", sheet = "choices")
external_choices <- read.xlsx("data/CCCM_Site_Reporting_Kobo_tool_(V1)_12042021.xlsx", sheet = "external_choices") %>%
  filter(list_name == "sitename") %>% dplyr::rename(a4_site_name = name)

# Try to match arabic with english names => see with partial match function sent to Kata => to be tested
# response$a4_site_name2 <- external_choices$`label::english`[match(response$a4_site_name, external_choices$`label::arabic`)]
# response <- mutate(response, a4_site_name3 = ifelse(!is.na(a4_site_name2), as.character(a4_site_name2), a4_other_site))

## IF YOU DON'T MANAGE TO FIND OUT THE RIGHT MATCH, Comment the below section and run the two lines above
# Partial matching of arabic name in data with the site masterlist
response <- partial_join(x = response, y = masterlist, pattern_x = "a4_other_site", by_y = "Site_Name_In_Arabic") %>%
  mutate(a4_site_name3 = ifelse(!is.na(Site_Name_In_Arabic), Site_Name_In_Arabic, a4_other_site))
# IMPORTANT!!! Look at the dataframe and make sure that the duplicates partial match have been removed.
# to remove a duplicate partial match 
# [Let's say that the there are three Site_Name_In_Arabic matchs "Aden", Adenalos "Aden village" 
# and Aden is the real match you want to keep, then run:
# response <- response %>% filter(!Site_Name_In_Arabic %in% c("Aden village", "Adenalos"))
# It will filter out all rows for which Site_Name_In_Arabic is either Aden village or Adenalos
response <- response %>% filter(!Site_Name_In_Arabic %in% c(""))                # Add the match to exclude in the vector c("")

## In case a new ID has to be assigned
### Create var from 1185 onwards and assing new site id
#numeric_seq <- seq(from = 1185, to = 5000)
#numeric_seq <- data.frame(numeric_seq)
#response <- mutate(response, new_site_id = ifelse(a4_site_name != "other", a4_site_name, paste0(a2_district,"_",numeric_seq$numeric_seq)))

### GPS coordinates - Mapping and check

# load layers from gdb
adm1 <- st_read(dsn = "data/shapes/yem_adm_govyem_cso_ochayemen_20191002_GDB.gdb", layer="yem_admbnda_adm1_govyem_cso")
adm2 <- st_read(dsn = "data/shapes/yem_adm_govyem_cso_ochayemen_20191002_GDB.gdb", layer="yem_admbnda_adm2_govyem_cso")
adm3 <- st_read(dsn = "data/shapes/yem_adm_govyem_cso_ochayemen_20191002_GDB.gdb", layer="yem_admbnda_adm3_govyem_cso") # Importing the sbd boundaries
adm3_loc <- st_read(dsn = "data/shapes/yem_adm_govyem_cso_ochayemen_20191002_GDB.gdb", layer="yem_admbndp_adm3_govyem_cso")
admin4 <- read.xlsx("data/shapes/yem_administrative_levels_122017.xlsx", sheet = "main_locality_standard_v0") %>%
  dplyr::rename(admin1Pcode = admin1pcode, admin2Pcode = admin2pcode, admin3Pcode = admin3pcode, admin4Pcode = admin4pcode)
admin4 <- admin4 %>% mutate(geometry = mapply(c, longitude, latitude, SIMPLIFY = F) %>% map(st_point)) %>% filter(!(longitude == 0 | latitude == 0))

# Joining site location to boundaries
df.loc <- response %>% mutate(a5_1_gps_longitude = a5_1_gps_longitude %>% as.numeric,
                              a5_2_gps_latitude = a5_2_gps_latitude %>% as.numeric ,
                              geometry = mapply(c, a5_1_gps_longitude, a5_2_gps_latitude, SIMPLIFY = F) %>% map(st_point)) 

# Plotting map of non cleaned location, setting as NA any non valid DD GPS coordinate
m <- leaflet() %>% addTiles() %>%
  addMarkers(data = df.loc, lng = ~a5_1_gps_longitude, lat = ~a5_2_gps_latitude,
             label = paste0("Site name: ", df.loc$a4_site_name, ", lon: ", df.loc$a5_1_gps_longitude, ", lat: ", df.loc$a5_2_gps_latitude)) 
m

### Sanitizing longitude and latitude entered coordinates
response <- clean.gps(response, "a5_1_gps_longitude", "a5_2_gps_latitude")

### Check in which admin boundaries the GPS location is supposed to fall
# Add a geometry column to the response dataframe
response.sf <- response %>%
  # dplyr::rename(admin2Pcode_df = Dist_ID) %>%                                 # commented as response has no systematic admin1/2 columns => For now partial matching of arabic site names with masterlist gives limited results. 
  filter(!is.na(Longitude_clean) & !is.na(Latitude_clean) & Longitude_clean != 0, Latitude_clean != 0) %>%
  mutate(SHAPE = mapply(c, as.numeric(Longitude_clean), as.numeric(Latitude_clean), SIMPLIFY = F) %>% map(st_point)) %>%
  st_sf(crs = 4326, sf_column_name = "SHAPE")
response.df <- response.sf %>% st_drop_geometry                                 # Drop the geometry column to be able to do intersect/non-intersection between the two

## Get match original data with the sbd boundaries corresponding to the entered GPS location
valid.gps.sf <- st_intersection(response.sf, adm2) %>% st_as_sf()               # will subset to only keep gps entries falling in adm3 existing boundaries [nrows will be lower or equal to original file nrows]
valid.gps.df <- valid.gps.sf %>% st_drop_geometry %>%                           # Drop the geometry column to be able to do intersect/non-intersection between the two
  select(intersect(colnames(valid.gps.sf), colnames(response.df)))              # select the original df's columns to be able to use setdiff()

## Step 1 => list the GPS location that falls outside of Yemen boundaries // Here we used the cleaned gps coordinates so it should be zero.
##  => Displays the gps entries outside of all adm3 boundaries [shows rows that disappeared when doing st_instersect i.e.]
non.valid.gps.entries <- setdiff(response.df %>% select(intersect(colnames(response.df), colnames(valid.gps.df))), valid.gps.df) 

## Step 1.2 => Look at the response file that only has the valid GPS entries and if the districts/governorate make sense
response.valid.gps <- valid.gps.sf %>% select(matches("Pcode|Name_en|site_name3"), matches("longitude|latitude"), matches("issue"), everything()) %>% st_drop_geometry

## Step 1.3 Map the sanitized GPS locations and sitenames
## Step 3 => Test map to display gps points that have issues
df.adm2 <- st_join(adm2, valid.gps.sf %>% mutate(covered = 1) )
pal.adm2 <- function(x) return(ifelse(!is.na(x), "indianred", "ghostwhite"))
df.adm3 <- valid.gps.sf

map <- leaflet() %>%
  addTiles()%>%
  addMarkers(data = df.adm3, lng = ~Longitude_clean, lat = ~Latitude_clean,
             label = paste0("The sitename name in tool is ", df.adm3$a4_other_site, ",\r\nmatched name in masterlist is: ", df.adm3$Site_Name_In_Arabic)) %>%
  addPolygons(data = df.adm2, color = "#B5B5B5", weight = 2, opacity = 0.5,
               highlightOptions = highlightOptions(color = "white", weight = 2), fillOpacity = 0.5,
               fillColor = ~pal.adm2(df.adm2$has.gps.issue),
               label = paste0(df.adm2$admin2Name_en, " district"))

 map

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

#write.csv(wrong_lat_long, paste("./output/wrong_lat_long_",today,".csv"), row.names = F)
#browseURL(paste("./output/wrong_lat_long_",today,".csv"))

## Check 0: check that there are no site surveyed twice
n_occur <- response %>% group_by(a4_site_name3) %>% summarise(Freq = n())
n_occur %>% filter(Freq>1)
duplicate_sites <- response[response$a4_site_name3 %in% n_occur$a4_site_name3[n_occur$Freq > 1],]
duplicates_log <- initialise.cleaning.log()
if (nrow(duplicate_sites>0)){
  duplicates_log <- cleaning.log.new.entries(df = duplicate_sites, var = "a4_site_name3", issue_type = "Duplicate site name:  check area code")
} else print ("No sites with identical names have been detected. The dataset seems clean.")
#write.csv(duplicate_sites, paste0("./output/duplicated_sites_",today,".csv"), row.names = F)
#browseURL(paste0("./output/duplicated_sites_",today,".csv"))

## Check 1: run cleaninginspector
response_issue <- inspect_all(response, "uuid") %>% 
  filter(!grepl("'other' response. may need recoding|Potentially sensitive information.", issue_type))
issue_log <- initialise.cleaning.log()

if(nrow(response_issue)>=1) {
  issue_table <- response_issue %>% mutate(uuid=response[.$index,"uuid",drop=TRUE],
                                           q0_3_organization=response[.$index,"q0_3_organization", drop=TRUE],
                                           a4_site_name3 = response[.$index, "a4_site_name3", drop = TRUE])
  issue_log <- cleaning.log.new.entries(issue_table, var="variable", issue_type="issue_type")
  } else {print("No outliers issues have been detected. The dataset seems clean.")}
#write.csv(response_issue, paste0("./output/dataset_issues_",today,".csv"), row.names = F)
#browseURL(paste0("./output/dataset_issues_",today,".csv"))

## Check 2: run extra cleaning analysis
### Check that the site name is in the correct location - TBD

## Check 3: Check adequacy
### Check that a service defined as a priority need is not classified as adequate
# response %>% select(matches("_services|priority"))                               ## TO BE DELETED : Adding errors to check 
# response[9, "i1_top_priority_need"] <- "medical_assistance"                      ## TO BE DELETED : Adding errors to check

check_adequacy <- select(response, "uuid", "q0_3_organization", "a4_site_name3", c("rrm_distributions":"waste_disposal_services"), "i1_top_priority_need", "i2_second_priority_need", "i3_third_priority_need")
adequacy_log <- priority.need.checks(response)
if(nrow(adequacy_log)==0) {print("No issues across avilability and service provisions have been detected. The dataset seems clean.")}
#write.csv(check_adequacy, paste0("./output/adequacy_issues_",today,".csv"), row.names = F)
#browseURL(paste0("./output/adequacy_issues_",today,".csv"))


### Check phone number - still needs to be fixed
phonenumber <- response %>% select("uuid","q0_3_organization","a4_site_name3", "q1_3_key_informat_mobile_number", "b6_cccm_agency_fp_mobile_number", "b3_exu_fp_mobile_number", "phonenumber") %>% dplyr::rename(phone.number=phonenumber) %>%
  setnames(gsub("_number", ".number", colnames(.)))

phonenumber_df <- phonenumber %>%
  mutate(across(contains("number"), ~ifelse(grepl("^[70|71|73|77|79]",.) | is.na(.), 0, 1), .names = "{col}_wrong_number"))

phone_melt <- phonenumber_df %>% mutate_at(vars(matches("number")), as.character) %>% 
  pivot_longer(cols = colnames(.)[grepl("number", colnames(.))],
               names_to = c("variable", ".value"), names_pattern = "(.*)\\.(.*)") %>%
  mutate(variable = gsub("phone_number" ,"phonenumber" , paste0(variable,"_number"))) %>% 
  filter(number_wrong_number==1)

 if(nrow(phone_melt)>=1) {

phone_melt$new_value <- " "
phone_melt$fix <- "Checked with partner"
phone_melt$checked_by <- "ON"
phone_melt$issue_type <- "The phone number provided may contain errors"
phone_melt$old_value <- phone_melt$number

 phone_log <- data.frame(uuid = phone_melt$uuid, 
                         agency = phone_melt$q0_3_organization, 
                         area = phone_melt$a4_site_name3, 
                         variable = phone_melt$variable, 
                         issue = phone_melt$issue_type, 
                         old_value = phone_melt$old_value, 
                         new_value = phone_melt$new_value, 
                         fix = phone_melt$fix, 
                         checked_by = phone_melt$checked_by)

  } else { 
  
  phone_log <- data.frame(uuid = as.character(),
                        agency = as.character(),
                        area = as.character(),
                        variable = as.character(),
                        issue = as.character(),
                        old_value = as.character(),
                        new_value = as.character(),
                        fix = as.character(),
                        checked_by = as.character())
  
  
  print("No issues with phone numbers. The dataset seems clean.")   }

### Check that formal sites have more than 20 HH
formal <- select(response, "uuid", "q0_3_organization", "a4_site_name3", "a7_site_population_hh", "a9_formal_informal")
formal <- formal %>% mutate(less_than_20 = ifelse((a9_formal_informal == "formal" & a7_site_population_hh < 20), 1,0))

formal_red <- filter(formal, formal$less_than_20 == 1)

if(nrow(formal_red) >=1) {
  
  formal_red$new_value <- " "
  formal_red$fix <- "Checked with partner"
  formal_red$checked_by <- "ON"
  formal_red$issue_type <- "Site identied as 'Formal' although the number of HHs is lower than 20"
  formal_red$variable <- "a7_site_population_hh"
  
  formal_log <- data.frame(uuid = formal_red$uuid, 
                          agency = formal_red$q0_3_organization, 
                          area = formal_red$a4_site_name3, 
                          variable = formal_red$variable, 
                          issue = formal_red$issue_type, 
                          old_value = formal_red$a7_site_population_hh, 
                          new_value = formal_red$new_value, 
                          fix = formal_red$fix, 
                          checked_by = formal_red$checked_by)
  
} else {
  
  formal_log <- data.frame(uuid = as.character(),
                        agency = as.character(),
                        area = as.character(),
                        variable = as.character(),
                        issue = as.character(),
                        old_value = as.character(),
                        new_value = as.character(),
                        fix = as.character(),
                        checked_by = as.character())
  
  print("Formal site population size check not needed. The dataset seems clean.")}

#write.csv(formal, paste0("./output/formal_site_population_issue_",today,".csv"), row.names = F)
#browseURL(paste0("./output/formal_site_population_issue_",today,".csv"))

### Check that collective centre is not a makeshift or emergency or transitional or open-air type of shelter
collective <- select(response, "uuid", "q0_3_organization", "a4_site_name3", "c1_type_of_site", "c9_primary_shelter_type")
collective <- collective %>% mutate(collective_issue = ifelse((c1_type_of_site == "collective_centre" & (c9_primary_shelter_type == "emergency_shelter" |
                                                                                      c9_primary_shelter_type == "makeshift_shelter" |
                                                                                              c9_primary_shelter_type == "transitional_shelter" |
                                                                                              c9_primary_shelter_type == "open_air_no_shelter")), 1, 0))


collective_red <- filter(collective, collective$collective_issue == 1)

if(nrow(collective_red) >= 1){
  
  collective_red$new_value <- " "
  collective_red$fix <- "Checked with partner"
  collective_red$checked_by <- "ON"
  collective_red$issue_type <- "Primary shelter type marked as 'makeshfit', 'emergency', 'transitional', or 'open-air' although type of site was identified as 'Collective centre'"
  collective_red$variable <- "c9_primary_shelter_type"
  
  collective_log <- data.frame(uuid = collective_red$uuid, 
                           agency = collective_red$q0_3_organization, 
                           area = collective_red$a4_site_name3, 
                           variable = collective_red$variable, 
                           issue = collective_red$issue_type, 
                           old_value = collective_red$c9_primary_shelter_type, 
                           new_value = collective_red$new_value, 
                           fix = collective_red$fix, 
                           checked_by = collective_red$checked_by)
  
} else {
  
  collective_log <- data.frame(uuid = as.character(),
                        agency = as.character(),
                        area = as.character(),
                        variable = as.character(),
                        issue = as.character(),
                        old_value = as.character(),
                        new_value = as.character(),
                        fix = as.character(),
                        checked_by = as.character())
  
  print("Collective centre type checks not needed. The dataset seems clean.")}

#write.csv(collective, paste0("./output/collective_centre_issue_",today,".csv"), row.names = F)
#browseURL(paste0("./output/collective_centre_issue_",today,".csv"))

### Check that waste disposal cannot be "non-existant" if they selected flush latrines
waste_disposal <- select(response, "uuid", "q0_3_organization", "a4_site_name3", "c10_primary_latrine_type", "waste_disposal_services")
waste_disposal <- waste_disposal %>% mutate(waste_disposal_issue = ifelse((waste_disposal_services == "non_existant" & c10_primary_latrine_type == "flush_latrine_to_tank_sewage_system_pit"), 1, 0))

waste_disposal_red <- filter(waste_disposal, waste_disposal$waste_disposal_issue == 1)


if (nrow(waste_disposal_red) >= 1){
  
  waste_disposal_red$new_value <- " "
  waste_disposal_red$fix <- "Checked with partner"
  waste_disposal_red$checked_by <- "ON"
  waste_disposal_red$issue_type <- "Waste disposal marked as 'non-existant' although  'flush latrines' was selected as primary latrine type"
  waste_disposal_red$variable <- "waste_disposal_services"
  
  waste_disposal_log <- data.frame(uuid = waste_disposal_red$uuid, 
                               agency = waste_disposal_red$q0_3_organization, 
                               area = waste_disposal_red$a4_site_name3, 
                               variable = waste_disposal_red$variable, 
                               issue = waste_disposal_red$issue_type, 
                               old_value = waste_disposal_red$waste_disposal_services, 
                               new_value = waste_disposal_red$new_value, 
                               fix = waste_disposal_red$fix, 
                               checked_by = waste_disposal_red$checked_by)
  
} else { 
  
  waste_disposal_log <- data.frame(uuid = as.character(),
                               agency = as.character(),
                               area = as.character(),
                               variable = as.character(),
                               issue = as.character(),
                               old_value = as.character(),
                               new_value = as.character(),
                               fix = as.character(),
                               checked_by = as.character())
  
  print("Waste disposal system checks not needed. The dataset looks clean.") }

#write.csv(waste_disposal, paste0("./output/waste_disposal_issue_",today,".csv"), row.names = F)
#browseURL(paste0("./output/waste_disposal_issue_",today,".csv"))

### Check that adequate WASH services do not include: flush latrine to the open / open defecation / pit uncovered / illegal water connection / unprotected well / surface water
adequate_wash <- select(response, "uuid", "q0_3_organization", "a4_site_name3", "c10_primary_latrine_type", "c8_primary_water_source", "wash_services")
adequate_wash <- adequate_wash %>% mutate(adequate_wash_issue = ifelse((wash_services == "adequate" & (c10_primary_latrine_type == "flush_latrine_to_the_open" |
                                                                                        c10_primary_latrine_type == "open_defecation" |
                                                                                        c10_primary_latrine_type == "pit_latrine_open" |
                                                                                        c8_primary_water_source == "illegal_connection_to_piped_network" |
                                                                                        c8_primary_water_source == "unprotected_well" |
                                                                                        c8_primary_water_source == "surface_water")), 1, 0))

adequate_wash_red <- filter(adequate_wash, adequate_wash$adequate_wash_issue == 1)

if(nrow(adequate_wash_red)>=1){
  
  adequate_wash_red$new_value <- " "
  adequate_wash_red$fix <- "Checked with partner"
  adequate_wash_red$checked_by <- "ON"
  adequate_wash_red$issue_type <- "WASH services identified as adequate although water source or primary latrine type was marked as unsafe or unprotected"
  adequate_wash_red$variable <- "c10_primary_latrine_type"
  
  adequate_wash_log <- data.frame(uuid = adequate_wash_red$uuid, 
                                   agency = adequate_wash_red$q0_3_organization, 
                                   area = adequate_wash_red$a4_site_name3, 
                                   variable = adequate_wash_red$variable, 
                                   issue = adequate_wash_red$issue_type, 
                                   old_value = adequate_wash_red$c10_primary_latrine_type, 
                                   new_value = adequate_wash_red$new_value, 
                                   fix = adequate_wash_red$fix, 
                                   checked_by = adequate_wash_red$checked_by)
  
} else {
  
  adequate_wash_log <- data.frame(uuid = as.character(),
                                  agency = as.character(),
                                  area = as.character(),
                                  variable = as.character(),
                                  issue = as.character(),
                                  old_value = as.character(),
                                  new_value = as.character(),
                                  fix = as.character(),
                                  checked_by = as.character())
  
  print("No issues with adequacy of wash services has been detected. The dataset seems clean.")}

#write.csv(adequate_wash, paste0("./output/adeqate_facility_issue_",today,".csv"), row.names = F)
#browseURL(paste0("./output/adeqate_facility_issue_",today,".csv"))

### Check that eviction risk does not come with a tenanacy agreement
eviction <- select(response, "uuid", "q0_3_organization", "a4_site_name3", "c3_tenancy_agreement_for_at_least_6_months", "f1_threats_to_the_site.eviction")
eviction <- eviction %>% mutate(eviction_issue = ifelse((c3_tenancy_agreement_for_at_least_6_months == "yes" & f1_threats_to_the_site.eviction == 1), 1, 0))

eviction_red <- filter(eviction, eviction$eviction_issue == 1)

if (nrow(eviction_red) >=1){
  
  eviction_red$new_value <- " "
  eviction_red$fix <- "Checked with partner"
  eviction_red$checked_by <- "ON"
  eviction_red$issue_type <- "Eviction was identied as a risk although the site holds a tennacy agreement"
  eviction_red$variable <- "f1_threats_to_the_site.eviction"
  
  eviction_log <- data.frame(uuid = eviction_red$uuid, 
                             agency = eviction_red$q0_3_organization, 
                             area = eviction_red$a4_site_name3, 
                             variable = eviction_red$variable, 
                             issue = eviction_red$issue_type, 
                             old_value = eviction_red$f1_threats_to_the_site.eviction, 
                             new_value = eviction_red$new_value, 
                             fix = eviction_red$fix, 
                             checked_by = eviction_red$checked_by)
  
} else {
  
  eviction_log <- data.frame(uuid = as.character(),
                        agency = as.character(),
                        area = as.character(),
                        variable = as.character(),
                        issue = as.character(),
                        old_value = as.character(),
                        new_value = as.character(),
                        fix = as.character(),
                        checked_by = as.character())
  
    print("No issues with eviction and type of tennacy has been detected. The dataset seems clean.")}

#write.csv(eviction, paste0("./output/eviction_issue_",today,".csv"), row.names = F)
#browseURL(paste0("./output/eviction_issue_",today,".csv"))

### Check cooking stuff
#cooking <- select(response, "uuid", "q0_3_organization", "a4_site_name3", "c5_fuel_available_in_site_close_proximity", "c6_electricity_solar_power_available_in_site", "primary_cooking_modality")
#cooking <- cooking %>% mutate(cooking_issue = ifelse((c5_fuel_available_in_site_close_proximity == "yes" | c6_electricity_solar_power_available_in_site == "yes" & 
                                                        #primary_cooking_modality == "Electrical_stove" | primary_cooking_modality == "Gas_stove"), 0, 1))

#cooking_red <- filter(cooking, cooking$cooking_issue == 1)


#if (nrow(cooking_red ) >=1){
  
  #cooking_red$new_value <- " "
  #cooking_red$fix <- "Checked with partner"
  #cooking_red$checked_by <- "ON"
  #cooking_red$issue_type <- "Eviction was identied as a risk although the site holds a tennacy agreement"
  #cooking_red $variable <- "c5_fuel_available_in_site_close_proximity"
  
  #cooking_log <- data.frame(uuid = cooking_red$uuid, 
                             #agency = cooking_red$q0_3_organization, 
                             #area = cooking_red$a4_site_name3, 
                             #variable = cooking_red$variable, 
                             #issue = cooking_red$issue_type, 
                             #old_value = cooking_red$f1_threats_to_the_site.eviction, 
                             #new_value = cooking_red$new_value, 
                             #fix = cooking_red$fix, 
                             #checked_by = cooking_red$checked_by)
  
#} else {
  
  #cooking_log <- data.frame(uuid = as.character(),
                             #agency = as.character(),
                             #area = as.character(),
                             #variable = as.character(),
                             #issue = as.character(),
                             #old_value = as.character(),
                             #new_value = as.character(),
                             #fix = as.character(),
                             #checked_by = as.character())
  
  
  #print("No issues with cooking and availability of fuel. The dataset seems clean.")}

### Check lenght of the survey, 10 = minimum, 40 = maximum (can be changed)
time_stamp <- select(response, "uuid", "start", "end", "q0_3_organization", "a4_site_name3")

check_time <- check.time(time_stamp, duration_threshold_lower = 15, duration_threshold_upper = 60)
check_time$q0_3_organization <- response$q0_3_organization[match(check_time$uuid, response$uuid)]
check_time$a4_site_name3 <- response$a4_site_name3[match(check_time$uuid, response$uuid)]

 if(nrow(check_time) >= 1){
  
  check_time$new_value <- " "
  check_time$fix <- "Checked with partner"
  check_time$checked_by <- "ON"
  check_time$issue_type <- "The survey was completed in less than 10 minutes or more than 40 minutes"
  check_time$variable <- "Lenght of survey"
  
  check_time_log <- data.frame(uuid = check_time$uuid, 
                               agency = check_time$q0_3_organization, 
                               area = check_time$a4_site_name3, 
                               variable = check_time$variable, 
                               issue = check_time$issue_type, 
                               old_value = check_time$value, 
                               new_value = check_time$new_value, 
                               fix = check_time$fix, 
                               checked_by = check_time$checked_by)
  
 } else {
  
  check_time_log <- data.frame(uuid = as.character(),
                        agency = as.character(),
                        area = as.character(),
                        variable = as.character(),
                        issue = as.character(),
                        old_value = as.character(),
                        new_value = as.character(),
                        fix = as.character(),
                        checked_by = as.character())
  
  
  print("The lenghts of the survey are within acceptable values. No cleaning needed.") }

#### Rbind everything if they exist
cleaning_log <- plyr::rbind.fill(duplicates_log, 
                           adequacy_log, 
                           adequate_wash_log, 
                           collective_log,
                           eviction_log,
                           formal_log,
                           issue_log,
                           waste_disposal_log,
                           gps_issue_log) %>%
  mutate(change = NA)

#write.csv(cleaning_log, paste0("./output/cleaning_log_",today,".csv"), row.names = F)
#browseURL(paste0("./output/cleaning_log_",today,".csv"))


#### Save adequacy check file for easier user
final_log <- list("cleaning_log" = cleaning_log,
                  "Service adequacy vs needs" = check_adequacy)

write.xlsx(final_log, paste0("./output/CCCM_SiteID_cleaning log_",today,".xlsx"))
browseURL(paste0("./output/CCCM_SiteID_cleaning log_",today,".xlsx"))

#if (nrow(check_time)>=1) {
 # write.xlsx(check_time_log, paste0("./output/CCCM_SiteID_time checks log_",today,".xlsx"))
#} else {print("Surveys were all between 5 and 40 minutes long.")}           

### Save everything in one file
#data_cleaning <- list("duplicated sites" = duplicate_sites,
                     # "Time stamp check" = check_time,
                     # "Wrong phone numbers" = phonenumber_df,
                     # "Outliers and others" = response_issue,
                     # "Service adequacy vs needs" = check_adequacy,
                     # "Longitude and latitude" = wrong_lat_long,
                     # "Number of HH in formal site" = formal,
                     # "Shelter type in collective shelter" = collective,
                     # "Waste disposal" = waste_disposal,
                     # "Adequate WASH facilities" = adequate_wash,
                     # "Tennecy agreement and eviction" = eviction)

#write.xlsx(data_cleaning, paste0("./output/CCCM_SiteID_data cleaning checks_",today,".xlsx"), colNames = TRUE)
#browseURL(paste0("./output/CCCM_SiteID_data cleaning checks_",today,".xlsx"))


#### After data is cleaning updated dataset by adding admin units names
## Load cleaned data
#response_clean <- read.csv("./data/CCCM_SiteID_24102019_cleaned.csv", stringsAsFactors = F)

#source("./R/add_locations.R")
#source("./R/moveme.R")

#response_clean <- add.location(response_clean)
