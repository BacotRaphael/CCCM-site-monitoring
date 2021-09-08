# CCCM Site Monitoring Tool - Data Cleaning script
# REACH Yemen - raphael.bacot@reach-initiative.org
# V10
# 27/06/2021

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
today <- Sys.Date()

## Install/Load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, openxlsx, reshape2,sf, leaflet, readxl, withr, mapview, randomcoloR, arabicStemR)
p_load_gh("mabafaba/cleaninginspectoR","impact-initiatives-research/clog")

## Source
#source("./R/check_time_RBA.R")
source("./R/utils.R")

## Paramaters to be updated each month

## Uncomment the relevant tool version and comment the other one
# tool.version <- "V1"                                                            # V1 or V2 update according to the kobo tool used
tool.version <- "V2"

## Update the directory for all files each month with the latest version. Beware of getting V1 and V2 right!
rawdata.filename.v1 <- "data/tbc/CCCM_Site_Reporting_Form_V1_gwq_week 36_tbc.xlsx"    # Update the name of the rawdata filename
rawdata.filename.v2 <- "data/tbc/CCCMSiteReporting_V2_Week 35_tfhd_radf_jaahd_tbc.xlsx"
tool.filename.v1 <- "data/kobo/CCCM_Site_Reporting_Kobo_tool_V1_23_05_2021_FINAL_all ngo names.xlsx"
tool.filename.v2 <- "data/kobo/CCCM_Site_Reporting_Kobo_tool_V2_23_05_2021_FINAL_all ngo names.xlsx"
sitemasterlist.filename <- "data/CCCM IDP Sites_making NEW site names and IDs_May 2021_11082021.xlsx"
# sitemasterlist.filename <- "data/CCCM_IDP Hosting Site List_coordinates verification exercise_March2021.xlsx"

## Tool columns comparison
# responsev1 <- read.xlsx(rawdata.filename.v1)
# responsev2 <- read.xlsx(rawdata.filename.v2)
# colv1 <- colnames(responsev1)
# colv2 <- colnames(responsev2)
# col.all <- unique(colv1, colv2) %>% as.data.frame
# setdiff(colv1, colv2)
# setdiff(colv2, colv1)

## Load data 
## response <- read_excel("data/Copy of CCCM_Site_Reporting_V2__Week 8_raw org data.xlsx", col_types="text") => to solve the issue with clean.gps? Dami's suggestion
response <- if (tool.version == "V1") {read.xlsx(rawdata.filename.v1)} else if (tool.version == "V2") {read.xlsx(rawdata.filename.v2)} else {print("invalid tool entered, should be either V1 or V2")}
response <- response %>%
  mutate(id = 1:n(), .before = "deviceid") %>%
  dplyr::rename(index = '_index', uuid = '_uuid') %>%
  setNames(tolower(colnames(.)))                                                # reduce to all lowercase
col.raw <- colnames(response)                                                   # Record original raw data columns for intermediate dataset sharing 

## Load Kobo Tool
tool<- if (tool.version == "V1") {read.xlsx(tool.filename.v1, sheet = "survey")} else if(tool.version == "V2") {read.xlsx(tool.filename.v2, sheet = "survey")} else {print("invalid tool entered, should be either V1 or V2")}
## Load survey choices Using kobo to rename the site name and than merge the other column
choices <- if (tool.version == "V1") {read.xlsx(tool.filename.v1, sheet = "choices")} else if(tool.version == "V2") {read.xlsx(tool.filename.v2, sheet = "choices")} else {print("invalid tool entered, should be either V1 or V2")}
external_choices <- if (tool.version == "V1") {read.xlsx(tool.filename.v1, sheet = "external_choices")} else if (tool.version == "V2") {read.xlsx(tool.filename.v2, sheet = "external_choices")} else {print("invalid tool entered, should be either V1 or V2")}
external_choices_site <- external_choices %>% filter(list_name == "sitename") %>% dplyr::rename(a4_site_name = name)
choices_all <- bind_rows(choices, external_choices) %>%                         # For cleaning log functions
  rbind(c("sitename", "other", "Other", "أخرى", NA, NA))

## Load site name masterlist
masterlist <- read.xlsx(sitemasterlist.filename) %>%                            
  setNames(gsub("\\.", "_", colnames(.))) %>% 
  setnames(old = c("Site_ID", "Site_Name_EN", "Site_Name_AR", "Gov_Pcode", "District_Pcode", "Sub_District_Pcode"),              # Put here whatever names CCCM cluster put in the latest masterlist 
           new = c("Site_ID", "Site_Name", "Site_Name_In_Arabic", "Master_list_GOV_ID", "Master_list_Dist_ID", "Master_list_Sub_Dist_ID"),              # Streamline colnames to keep the same script.
           skip_absent = TRUE) %>%
  mutate_at(vars(-matches("_Households|_Population|_IDPs")), as.character)      # mutate all names and IDs as character to enable matching

## Load pcodes
# file.pcode <- "data/yem_admin_ochayemen_20191002.xlsx"
# pcodes <- read.xlsx(file.pcode, sheet="admin3")
file.pcode <- "R/pcodes/22June2021_Final Draft_AoR_admin1,2,3_Final review_July2021.xlsx"
pcodes <- read.xlsx(file.pcode, sheet = "AoR_admin3")

## Format relevant columns as numerical + transform arabic numbers in european numbers
response <- response %>%
  mutate_at(vars(matches("a7_site_population_"),
                 matches("b1_cccm_Pillars_existing_on_s."),
                 matches("a8_population_groups_other_than_idps_in_site_select_all_applicable."),
                 matches("c7_presence_of_particularly_vulnerable_groups."),
                 matches("d1_most_common_reason_idps_left_place_of_origin."),
                 matches("additional_fire_safety_measures."),
                 matches("f1_threats_to_the_site."),
                 matches("primary_cooking_space."),
                 matches("pafe_cooking_practices."),
                 # any_of(c("subscriberid", "b3_SCMCHAIC_fp_number", "b3_exu_fp_mobile_number", "b6_cccm_agency_fp_mobile_number", "b9_community_committee_fp_cell", "phonenumber", "q1_3_key_informat_mobile_number")) ## Deleted because some phone number are entered with other symbols ("/ or -")
                 ), 
            ~ as.numeric(gsub(",", "", unlist(lapply(., arabic.tonumber))))) %>%
  mutate_at(vars(matches("\\.other$|_other$|_please_specify$|_other_site")), as.character)          # Ensure right format of sitename for matching [shouldn't be necessary if there are entries for this column]

### Initialize cleaning log - Will determine the columns to select in add.to.cleaning.log function 
coldf <- c("uuid", "q0_3_organization", "a4_site_name")
col.cl <- c("uuid", "agency", "area", "variable", "issue", "check_id", "old_value", "new_value", "fix", "checked_by")
cleaning.log <- initialise.cleaning.log()
cleaning.log.internal <- initialise.cleaning.log()

## Check 1: check that there are no site surveyed twice
n_occur <- response %>%                                                         # Check duplicates sitenames
  group_by(a4_site_name) %>%
  filter(a4_site_name!="other",a4_site_name!="new" ) %>%                                             # Filtering out "other" sitenames entry => should be cleaned beforehand
  summarise(Freq = n())
duplicate.site.names <- n_occur %>% filter(Freq>1) %>% select(a4_site_name)
duplicate.site.names
check_duplicate_sites <- response %>%
  mutate(flag = ifelse(a4_site_name %in% duplicate.site.names, T, F),
         area=a4_site_name, agency=q0_3_organization)
## Add to the cleaning log 
add.to.cleaning.log(checks = check_duplicate_sites,
                    check_id = "1",
                    question.names = c("a4_site_name"),
                    issue = "Duplicated site name")
if (nrow(check_duplicate_sites %>% filter(flag))==0) {print ("No sites with identical names have been detected. The dataset seems clean.")}
if (nrow(check_duplicate_sites %>% filter(a4_site_name=="other",a4_site_name=="new"))>0) {print(paste0("There are ", sum(response$a4_site_name == "other"), " sitenames recorded as other or new. Check later!"))}

##### Check 2: Sitename matching ###############################################
# 2.1. Match the sitename entered in arabic as "other site" with sitename in arabic in the masterlist
# 2.1.1. Cleaning specific patterns from arabic sitename in the masterlist: 
# pattern_english <- c("site", "school", "camp", "mosque", "center", "hospital", "souq", "valley", "building", "city", "street", "neighbourhood")
pattern_arabic <- c("ال","آل","وادي","موقع","مدرسة","مخيم","مركز","مستشفى","سوق","مبنى","حي ","مدينة","مسجد ")
replacement <- rep("", length(pattern_arabic))
# Trying to harmonise different arabic writings + remove village in both masterlist and dataset
masterlist <- masterlist %>%
  mutate(Site_Name_In_Arabic_tidy = str_replace_all(Site_Name_In_Arabic, setNames(replacement, pattern_arabic)))
response <- response %>%
  mutate(Site_Name_In_Arabic_tidy_df = str_replace_all(a4_other_site, setNames(replacement, pattern_arabic))) %>%
  relocate(Site_Name_In_Arabic_tidy_df, .after = "a4_other_site")

# 2.1.2. Comparing  perfect match with cleaned/tidyed arabic names match ("cleaned from patterns") as well as partial_match (which split sitename in substrings and look whether it matches part of a sitename in arabic in the masterlist)
check_sitename <- response %>% 
  select(matches("site_name|other_site"), everything()) %>%
  left_join(masterlist %>% select(Site_ID,
                                  Site_Name,
                                  Site_Name_In_Arabic,
                                  Master_list_Dist_ID,
                                  Master_list_Sub_Dist_ID) %>%  # Perfect match with arabic sitename in masterlist
              setnames(paste0(colnames(.),"_perfect_match")),
            by = c("a4_other_site" = "Site_Name_In_Arabic_perfect_match")) %>%
  left_join(masterlist %>%                                                      # Perfect match with arabic names tidy (cleaned from patterns)
              select(Site_ID,
                     Site_Name,
                     Site_Name_In_Arabic,
                     Site_Name_In_Arabic_tidy) %>%
              setnames(paste0(colnames(.),"_tidy_match")) %>% dplyr::rename(Site_Name_In_Arabic_before_tidy_match = Site_Name_In_Arabic_tidy_match), 
            by = c("Site_Name_In_Arabic_tidy_df" = "Site_Name_In_Arabic_tidy_tidy_match")) %>%
  partial_join(x = ., y = masterlist %>%                                        # Splitting sitename in arabic by space as separator, and returning all sitenames that any match part of the sitename in arabic
                 select(Site_ID, Site_Name, 
                        Site_Name_In_Arabic,
                        Master_list_GOV_ID,
                        Master_list_Dist_ID,
                        Master_list_Sub_Dist_ID) %>%                            # WARNING THIS STEP WILL/MIGHT GENERATE DUPLICATE MATCHES, FILTER THEM OUT MANUALLY
                 setnames(paste0(colnames(.),"_partial_match")),
               pattern_x = "a4_other_site", by_y = "Site_Name_In_Arabic_partial_match") %>%
  left_join(masterlist %>% select(Site_ID,
                                  Site_Name,
                                  Site_Name_In_Arabic,
                                  Master_list_Dist_ID,
                                  Master_list_Sub_Dist_ID) %>%  # Perfect match with arabic sitename in masterlist
              setnames(paste0(colnames(.),"_perfect_match_eng")),
            by = c("a4_other_site" = "Site_Name_perfect_match_eng")) %>%
  group_by(uuid, Site_ID_partial_match) %>%  
  mutate(n=n()) %>% filter(!duplicated(n)) %>% ungroup %>%                      # filter out the site sub-matches that have the same site match // i.e. => partial_match split sitename in substrings with " " as separator and look for all matches for each substring.
  relocate(matches("_match"), matches("Master_list_|a1_|a2_|a3_"), .before = "a4_other_site") %>%
  mutate(flag = ifelse(!(is.na(a4_other_site)) & (a4_site_name == "other"|a4_site_name == "new"), T, F),
         issue = ifelse(flag, "Sitename has been marked as 'Other' or 'New'. Please check the potential matches.", ""))

dup_match_sitename_log <- check_sitename %>% group_by(uuid) %>%                 # filter to keep only duplicates partial matches
  filter(flag & n()>1) %>% select(uuid, matches("a1_|a2_|a3_|a4_"), everything())

## 2.1.3. filtering the sitename matches to keep only the one in same gov/dist./sub-dist. than the survey 
dup_match_sitename_log1 <- dup_match_sitename_log %>% 
  filter((a1_governorate == Master_list_GOV_ID_partial_match))  # Filtering sitenames partial match in same governorate
dup_match_sitename_log2 <- dup_match_sitename_log %>% 
  filter(a2_district == Master_list_Dist_ID_partial_match)                      # Filtering sitenames partial match in same district
dup_match_sitename_log3 <- dup_match_sitename_log %>% 
  filter(a3_sub_district == Master_list_Sub_Dist_ID_partial_match)              # Filtering sitenames partial match in same sub district

id.duplicate.site <- dup_match_sitename_log$uuid %>% unique                     # get id of all site with multiple partial matches
id.filtered.duplicate.site <- dup_match_sitename_log3$uuid %>% unique           # get id of sites for which we found a partial match in the same sub-district
id.duplicate.no.match <-                                                        # get id of sites that have no partial match in the same sub-district (and have been filtered out)
  id.duplicate.site[!id.duplicate.site %in% id.filtered.duplicate.site]
duplicate.no.match <- dup_match_sitename_log %>%                                # retrieve surveys of site that were filtered out
  filter(uuid %in% id.duplicate.no.match, !duplicated(uuid))

## 2.1.4. Binding together the duplicate matches and single matches
sitename_log <- check_sitename %>%
  filter(!uuid %in% dup_match_sitename_log$uuid) %>%                            # Filter out all duplicate matches
  bind_rows(duplicate.no.match) %>%                                             # 
  bind_rows(dup_match_sitename_log3) %>%                                        # Bind the geographically filtered duplicate matches - adapt to dup_match_sitename_log1 or 2 if you don't have enough matches
  group_by(uuid) %>% mutate(n=n()) %>% arrange(q0_3_organization, -n, uuid) %>% # Sort so duplicate matches appear next to each other
  filter(flag) %>% select(uuid, q0_3_organization, a4_site_name, issue, a4_other_site, everything()) %>% 
  mutate(a4_other_site_translation = transliterate(a4_other_site),              # Transliterate the arabic sitename into english alphabet // test
         change = ifelse((!is.na(Site_ID_perfect_match) | !is.na(Site_ID_perfect_match_eng)), "TRUE" , ""),           # TRUE / FALSE to flag easy site names that will be done without external follow-up
         new_site = "",                                                         # TRUE / FALSE to mark new sites
         follow_up = ifelse(n==1, "TRUE", ""),                                  # TRUE / FALSE to filter partial matches requiring follow-up with partners - is set to TRUE by default when there is 0 or 1 partial match
         a4_site_name_new = ifelse(!is.na(Site_ID_perfect_match), Site_ID_perfect_match,
                                   ifelse(!is.na(Site_ID_perfect_match_eng), Site_ID_perfect_match_eng,
                                          ifelse(!is.na(Site_ID_tidy_match), Site_ID_tidy_match,
                                                 ifelse(!is.na(Site_ID_partial_match), Site_ID_partial_match, "")))), # To be filled with final site code and name / propose the perfect match, then tidy match then partial match as proposition, but needs to be checked!
         a4_site_name_new_en = ifelse(!is.na(Site_ID_perfect_match), Site_Name_perfect_match,
                                      ifelse(!is.na(Site_ID_perfect_match_eng), a4_other_site,
                                             ifelse(!is.na(Site_Name_tidy_match), Site_Name_tidy_match,
                                                    ifelse(!is.na(Site_Name_partial_match), Site_Name_partial_match, "")))),
         a4_site_name_new_ar = ifelse(!is.na(Site_ID_perfect_match), a4_other_site,
                                      ifelse(!is.na(Site_ID_perfect_match_eng), Site_Name_In_Arabic_perfect_match_eng,
                                             ifelse(!is.na(Site_Name_In_Arabic_before_tidy_match), Site_Name_In_Arabic_before_tidy_match,
                                                    ifelse(!is.na(Site_Name_In_Arabic_partial_match), Site_Name_In_Arabic_partial_match, "")))),
         .after = "a4_other_site") %>% select(uuid:a5_2_gps_latitude, -start, -end, -today, -deviceid, -imei, -q0_2_gender, -calca11, -q1_3_key_informat_mobile_number, -subscriberid, -q1_1_key_informant_name, -q1_2_key_informat_gender, -phonenumber, -q0_1_enumerator_name ) %>%
  relocate(Site_Name_In_Arabic_partial_match, .after = "Site_Name_partial_match") %>% dplyr::rename(agency=q0_3_organization, area=a4_site_name)%>% 
  mutate(across(matches("_partial_match$"), ~ ifelse(a1_governorate != Master_list_GOV_ID_partial_match, NA, .)))

# 2.1.5. Write all sitename matches in excel file (with duplicates to be filtered out manually)
dir.create("output/cleaning log/site name", showWarnings = F, recursive = T)
save.sitename.follow.up(sitename_log, filename.out = paste0("output/cleaning log/site name/site_name_log_", today, ".xlsx"))
browseURL(paste0("output/cleaning log/site name/site_name_log_", today, ".xlsx"))

##### 2.2. Manual sitename log filtering & updating ############################

## 2.2.1. Group and hide column a4_site_name_new to Site_Name_In_Arabic_before_tidy_match in excel sheet to ease process. 
## 2.2.2. Go through each group of duplicate partial matches that have the same color to inspect which  
##        Site_Name_In_Arabic_partial_match correspond to the value in a4_other_site. 
##        If you don't read arabic, try to filter using a4_other_site_translation (which is an automatic arabic transliteration)
##        and compare it with the different Site_Name_partial_match in english from the masterlist.
## 2.2.3. You can either delete the rows that are false match or write "TRUE" to the column G "follow_up". YOU NEED TO HAVE AT LEAST ONE TRUE per group of multiple match corresponding to one uuid
##        => Only sitenames with duplicate match requires updating the column “follow_up”.
##        For all other sitenames, the column is automatically set to TRUE in the script.
## 2.2.4. For site name matches you're 100% confident about, write "TRUE" in the column "change". 
##        The script will update the site name without including this to the partner's cleaning log.
## 2.2.5. For new sites, just write the english name and the arabic name in the columns a4_site_name_new_name_en & a4_site_name_new_name_ar
##        and leave the a4_site_name_new blank
## 2.2.6. After updating the sitename log, save it in the same folder with "_updated" at the end of the name

################################################################################

## 2.2.6. Reading the manually updated sitename cleaning log
sitenamelog.updated <- "output/cleaning log/site name/site_name_log_2021-08-30_updated_raph.xlsx"
sitename_log_updated <- read.xlsx(sitenamelog.updated) 

# Make sure no rubbish has been recorded in the the important columns
sitename_log_updated %>% select(change, new_site, follow_up) %>% summarise_all(~unique(.))

sitename_log_updated <- sitename_log_updated %>%                                # Make sure we flag out duplicate partial matches
  group_by(uuid) %>% mutate(n=n()) %>% ungroup %>%
  filter((follow_up %in% c("TRUE", "T")) | n == 1) %>%                          # Keep only the validated partial matches and non duplicated sitenames
  filter(!(!is.na(Site_ID_perfect_match) &                                      # Filter out partial matches that conflict with perfect matches
             (Site_ID_perfect_match != Site_ID_partial_match))) %>%   
  group_by(uuid) %>% mutate(n=n()) %>% ungroup                                  # Check eventual remaining duplicates that might have been forgotten

if ((test <- nrow(sitename_log_updated %>% filter(n>1))) > 0) {
  print(paste0("There are ", test," remaining duplicate partial matches in sitename log! \n\nOpen the updated cleaning log file and make sure it corresponds to potential valid matches to be sent to partners as follow-up."))}
remaining.dup.site <- sitename_log_updated %>% filter(n>1)

## Check sites that still have no attributed code
nocode_sites <- sitename_log_updated %>% filter(is.na(a4_site_name_new) & !(new_site  %in% c("TRUE", "T")))
if ((s <- nrow(nocode_sites)) > 0) {print(paste0("There are ", s, " sites with no attributed site code. Please make sure that there are no potential match and if relevant mark them as potential new sites by writing 'TRUE' in column 'new_site'."))}
nocode_sites

## Filter new sites // Data cleaning script Move that part so it's part of the main cleaning log
# mutate(flag.new=ifelse(is.na(a4_site_name_new) & (new_site %in% c("TRUE", "T")), T, F),
sitename_log_updated <- sitename_log_updated %>% 
  mutate(issue = ifelse(new_site %in% c("TRUE","T"), "We were unable to find this site in the current CCCM site ID master list. Is this a new site, or has the name changed?", issue))          
check_new_sites <- sitename_log_updated %>% filter(new_site)                    ## this is the list of site that will be flagged as potential new sites to partners in the cleaning log
if ((s <- nrow(sitename_log_updated %>% filter(new_site))) > 0) {print(paste0("There are ", s, " potential new sites requiring feedback from partners: "))
  print(paste0(check_new_sites$a4_other_site))}

# clean site name cleaning log new proposition for potential new sites to avoid confusion
sitename_log_updated <- sitename_log_updated %>%
  mutate(area = ifelse(area=="other", a4_other_site, area)) %>%
  mutate_at(vars(matches("a4_site_name_new|_match")), ~ifelse(new_site %in% c("TRUE", "T"), "", .))

## Apply changes to sitename column a4_site_name for all straightforward perfect or partial matches (change=TRUE)
## Question: do we need to update the new name en and ar in response or useless?
sitename_log_internal <- sitename_log_updated %>% filter(change %in% c("TRUE")) %>% mutate_all(as.character)
for (r in seq_along(nrow(sitename_log_internal))){
  new_id <- sitename_log_internal[r, "a4_site_name_new"]
  new_name <- sitename_log_internal[r, "a4_site_name_new_en"]
  new_name_ar <- sitename_log_internal[r, "a4_site_name_new_ar"]
  if (!is.na(new_id) & !is.na(new_name) & !is.na(new_name_ar)){
    response[response$uuid == sitename_log_internal[r,"uuid"], "a4_site_name"] <- new_id
  }
}

## Reformatting final sitename cleaning log to put everything into one single cleaning log for all changes at once in data cleaning script
sitename_log <- sitename_log_updated %>%
  dplyr::select(uuid, agency, issue, a4_other_site, a4_site_name_new, a4_site_name_new_en, a4_site_name_new_ar, new_site, change) %>%
  mutate(area=a4_other_site, variable = "a4_site_name", old_value = "other", check_id = "2", fix="Checked with partner", checked_by="ON") %>%
  dplyr::rename(new_value = a4_site_name_new) %>% select(all_of(col.cl), new_site, everything()) %>% mutate_all(as.character)
sitename_log_external <- sitename_log %>% 
  filter(!change %in% c("TRUE")) %>% 
  mutate(issue=ifelse(new_site %in% c("TRUE"), paste0(issue, ". Update the column new_site with TRUE to confirm if this is a new site, and fill the column a4_site_name_new_en and a4_site_name_new_ar with the sitename in english and in arabic."), issue), new_site=gsub("TRUE","?", new_site)) %>%
  mutate_all(as.character)
sitename_log_internal <- sitename_log %>% filter(change %in% c("TRUE")) %>% mutate(issue="The sitename has been entered as other although it was part of the available choices.") %>% mutate_all(as.character)

# Add sitenames requiring partner feedback to cleaning log
cleaning.log <- cleaning.log %>% bind_rows(sitename_log_external)               
# cleaning.log <- cleaning.log %>% filter(check_id!=2)                          # If you ran it twice or more, uncomment and run this line, then you can do the line below

# Add sitenames that have been internally updated to internal cleaning log for the record ##/ DONT RUN TWICE OR FILTER THEM OUT THEN
# cleaning.log.internal <- cleaning.log.internal %>% filter(check_id!=2)          # If you ran it twice or more, uncomment and run this line, then you can do the line below
cleaning.log.internal <- cleaning.log.internal %>% bind_rows(sitename_log_internal)

## Check 3: Match q3 organization other names with kobo list
## 3.1. Do a partial match for Partner name in arabic from the external choice list
choices.ngo <- choices %>% filter(list_name == "ngo") %>% select(-governorate, -list_name) %>% setNames(c("ngo_code", "ngo_name_en", "ngo_name_ar"))
check_ngo_names <- response %>%
  mutate(org_other_lower = q0_3_organization_other %>% tolower, .before =1) %>%
  left_join(choices.ngo,                                                        # Add ngo names in english and arabic matching ngo code
            by = c("q0_3_organization" = "ngo_code")) %>% 
  left_join(choices.ngo %>%                                                     # Matching organization other in arabic with ngo code and english names  
              mutate(ngo_name_ar_lower = tolower(ngo_name_ar)) %>%
              setNames(paste0(colnames(.), "_match_other")), 
            by = c("q0_3_organization_other" = "ngo_name_ar_lower_match_other")) %>%
  partial_join(x = . , y = choices.ngo %>%                                      # partial matching of organization_other with ngo code as usually they mix name in arabic and acronyms in brakets
                 setNames(paste0(colnames(.), "_match_other_partial_code")),
               pattern_x = "org_other_lower", 
               by_y = "ngo_code_match_other_partial_code") %>%                  
  mutate(flag = ifelse(q0_3_organization == "other", T, F),
         issue = ifelse(flag, "Organization has been marked as 'Other'. Check the potential matches", "")) %>%
  select(uuid, a4_site_name, issue, matches("organization"), matches("^ngo_"), matches("Site_|a4_"), id, flag)
  
ngo_log <- check_ngo_names %>% select(uuid, matches("organization"), matches("_partial_code"), matches("a4_"), everything()) %>%
  arrange(uuid, ngo_code_match_other_partial_code) %>% filter(flag) %>%
  mutate(q0_3_organization_new_code = ifelse(!is.na(ngo_code_match_other), ngo_code_match_other, ""),
         q0_3_organization_new_name_en = ifelse(!is.na(ngo_name_en_match_other), ngo_name_en_match_other, ""),
         q0_3_organization_new_name_ar = ifelse(!is.na(ngo_name_ar_match_other), ngo_name_ar_match_other, ""),
         keep = "", new_org="", .after = "q0_3_organization_other") %>%
  dplyr::rename(ngo_code_partial_match=ngo_code_match_other_partial_code,
                ngo_name_en_partial_match=ngo_name_en_match_other_partial_code,
                ngo_name_ar_partial_match=ngo_name_ar_match_other_partial_code,
                agency=q0_3_organization, area=a4_site_name) %>%
  select(uuid:ngo_name_ar_match_other) %>% relocate(area, issue, .after = "agency")

if (nrow(ngo_log)==0) {print("Skip the rest of the checks dedicated to the organisation name matching. There is no problem, jump to Check 4.0.")}

# Write all sitename matches in excel file (with duplicates!)
dir.create("output/cleaning log/organisation name", showWarnings = F, recursive = T)
save.org.name.follow.up(ngo_log, filename.out = paste0("output/cleaning log/organisation name/org_name_log_", today, ".xlsx"))
browseURL(paste0("output/cleaning log/organisation name/org_name_log_", today, ".xlsx"))

##### Organization name log updating ####################################################

## 1. Go through each group of duplicate partial matches that have the same color to inspect which ngo_code_partial_match
##    correspond to the value in q0_3_organization_other. 
## 2. You can either delete the rows that are false match or write "TRUE" to the column I "keep"
## 3. After updating the sitename log, save it in the same folder with "_updated" at the end of the name

################################################################################

ngo.filename.updated <- "output/cleaning log/organisation name/org_name_log_2021-07-12_updated.xlsx"
ngo_log_updated <- read.xlsx(ngo.filename.updated)

# Make sure we filtered out duplicate partial matches
ngo_log_updated <- ngo_log_updated %>%
  group_by(uuid) %>% mutate(n=n()) %>% ungroup %>%
  filter((keep %in% c("TRUE", "T")) | n == 1) %>%                               # Keep only the validated partial matches and non duplicated sitenames
  group_by(uuid) %>% mutate(n=n()) %>% ungroup                                  # Flag eventual remaining duplicates that might have been forgotten

if ((test<-nrow(ngo_log_updated %>% filter(n>1)))>0) {
  stop(paste0("There are ", test," remaining duplicate partial matches in organisation name log! \n\nOpen the updated cleaning log file and filter them out either by deleting the non relevant matches or by setting keep column to TRUE for relevant matches"))}
remaining.dup.org <- ngo_log_updated %>% filter(n>1)

## Highlight new organizations
new_ngos <- ngo_log_updated %>% filter(new_org=="TRUE") %>%
  mutate(issue=paste0("Marked as 'other' because it is a new partner. New code attributed is ", q0_3_organization_new_code))
new.ngos.uuid <- new_ngos$uuid

## Update organisation log to reflect new oganisation with different "issue"
ngo_log_final <- ngo_log_updated %>% filter(!uuid %in% new.ngos.uuid) %>% bind_rows(new_ngos)

## Apply changes to sitename column a4_site_name according to Partner's feedback.
## Question: do we need to update the new name en and ar in response or useless?
for (r in seq_along(nrow(ngo_log_updated))){
  new_id <- ngo_log_updated[r, "q0_3_organization_new_code"]
  new_name <- ngo_log_updated[r, "q0_3_organization_new_name_en"]
  new_name_ar <- ngo_log_updated[r, "q0_3_organization_new_name_ar"]
  if (!is.na(new_id) & !is.na(new_name) & !is.na(new_name_ar)){
    response[response$uuid == ngo_log_updated[r,"uuid"], "q0_3_organization"] <- new_id
  }
}

# ## Reformat and export organisation cleaning log to append all in a single one cleaning log => check_id == 3 will be saved as internal
org_cleaning_log <- ngo_log_updated %>%
  dplyr::rename(new_value=q0_3_organization_new_code) %>%
  mutate(variable="q0_3_organization", old_value="other", check_id = "3", fix="Checked with partner", checked_by="ON") %>%
  dplyr::select(all_of(col.cl), matches("new_name")) %>% mutate_all(as.character)
org_cleaning_log %>% write.xlsx(paste0("output/cleaning log/organisation name/org_cleaning_log_final_", today, ".xlsx"))
cleaning.log <- cleaning.log %>% bind_rows(org_cleaning_log)
  
## Check 4.0 that the right tool has been used
## summarised masterlist by subdistrict / cleaning all subdistricts in which there is two versions

# For new sites, join the full list of area of control and associate the kobo version tool they should have depending on control north/south
aor3 <- read.xlsx("data/aor_admin3.xlsx", sheet=3) %>%
  filter(!duplicated(admin3Pcode)) %>%
  mutate(tool = ifelse(AoC=="Mixed", "V1 & V2", ifelse(AoC=="SCHMCHA", "V2", "V1")))

## Update the rest of that part for the tool using the aor + masterlist 
check_tool_version <- response %>%
  left_join(masterlist %>% dplyr::select(Site_ID, Tool) %>% dplyr::rename(tool.masterlist=Tool), by=c("a4_site_name"="Site_ID")) %>%
  left_join(aor3 %>% dplyr::select(admin3Pcode,tool) %>% dplyr::rename(tool.aor=tool), by=c("a3_sub_district"="admin3Pcode")) %>%
  mutate(tool=ifelse(!is.na(tool.masterlist),tool.masterlist, tool.aor)) %>% select(matches("tool"), everything()) %>%
  filter(!is.na(tool)) %>%                                                      # Filter out sub-district of response file that are not in the aor document
  mutate(flag = ifelse(!grepl(tool.version, tool), T, F),
         issue = ifelse(flag, paste0("This site survey has been conducted with a different Kobo tool: version, ", tool, ". Under which area of control is this site?"), ""))
survey_wrong_tool <- check_tool_version %>% filter(flag)
if ((t <- nrow(survey_wrong_tool)) > 0){print(paste0(t, " site may have used the wrong kobo tool. Please check."))} else {"No issue with Kobo tool version have been detected."}
wrong_survey_summary <- survey_wrong_tool %>% select(uuid, matches("a3_sub_district|organization|Pcode|admin3Name|issue$|flag|tool"))

## If doubt => ask under which area of control is this site?
## mutate(issue (ifelse(wrongtool => "which area of control?")))
survey_version_log <- survey_wrong_tool %>%
  dplyr::rename(agency=q0_3_organization, area=a4_site_name) %>%
  mutate(variable="tool version", old_value=tool.version, new_value=tool, fix="Checked with partner", checked_by="ON", check_id="4.0") %>%
  dplyr::select(all_of(col.cl)) %>% mutate_all(as.character)

## Add to cleaning log when there is a doubt ## 
cleaning.log <- cleaning.log %>% bind_rows(survey_version_log)

### Check 4: GPS Coordinates check
## Initial check 4_admin - Ensuring that a valid Pcode is entered at subdistrict level
check_admin <- response %>% 
  mutate(flag=ifelse(is.na(a3_sub_district) | !a3_sub_district %in% pcodes$admin3Pcode, T, F),
         issue=ifelse(flag, "There is no entered sub-district or it is not a recognized Pcode, Please update", "")) %>%
  dplyr::rename(agency=q0_3_organization, area=a4_site_name)
add.to.cleaning.log(check_admin, check_id = "4_admin", question.names = c("a3_sub_district"), issue = "issue")

## 0. Importing layers
adm1 <- st_read(dsn = "data/shapes/yem_adm_govyem_cso_ochayemen_20191002_GDB.gdb", layer="yem_admbnda_adm1_govyem_cso")
adm2 <- st_read(dsn = "data/shapes/yem_adm_govyem_cso_ochayemen_20191002_GDB.gdb", layer="yem_admbnda_adm2_govyem_cso")
adm3 <- st_read(dsn = "data/shapes/yem_adm_govyem_cso_ochayemen_20191002_GDB.gdb", layer="yem_admbnda_adm3_govyem_cso") # Importing the sbd boundaries
adm3_loc <- st_read(dsn = "data/shapes/yem_adm_govyem_cso_ochayemen_20191002_GDB.gdb", layer="yem_admbndp_adm3_govyem_cso")
# admin4 <- read.xlsx("data/shapes/yem_administrative_levels_122017.xlsx", sheet = "main_locality_standard_v0") %>%
#   dplyr::rename(admin1Pcode = admin1pcode, admin2Pcode = admin2pcode, admin3Pcode = admin3pcode, admin4Pcode = admin4pcode)
# admin4 <- admin4 %>% mutate(geometry = mapply(c, longitude, latitude, SIMPLIFY = F) %>% map(st_point)) %>% filter(!(longitude == 0 | latitude == 0))

# 1. Joining site location to boundaries
df.loc <- response %>% mutate(longitude = a5_1_gps_longitude %>% as.numeric,
                              latitude = a5_2_gps_latitude %>% as.numeric ,
                              SHAPE = mapply(c, longitude, latitude, SIMPLIFY = F) %>% map(st_point)) %>%
  st_sf(crs = 4326, sf_column_name = "SHAPE") %>%
  filter(!is.na(longitude) & !is.na(latitude) & 
           abs(longitude) < 180, abs(latitude) < 90) %>%                        # Filter out non valid gps coordinates and NAs. 
  select(longitude, latitude, a4_site_name)

# 2. Plotting map of non cleaned location, setting as NA any non valid DD GPS coordinate
m <- leaflet() %>% addTiles() %>%
  addMarkers(data = df.loc, lng = ~longitude, lat = ~latitude,
             label = paste0("Site name: ", df.loc$a4_site_name, ", lon: ", df.loc$longitude, ", lat: ", df.loc$latitude)) 
m

# 3. Sanitizing gps coordinates entries: Convert to DD when possible and flag gps coordinates syntax issues 
# Manually gsub ، as it doesn't work properly within the clean.invalid.char.gps function
response <- response %>% mutate_at(vars(matches("longitude|latitude")), ~as.character(gsub("،", ".",.)))
response <- clean.invalid.char.gps(response, "a5_1_gps_longitude", "a5_2_gps_latitude")
response <- sanitize.gps(response, "a5_1_gps_longitude", "a5_2_gps_latitude")

# Check what remaining entries have not been cleaned:
check.not.cleaned <- response %>% select(uuid, matches("longitude|latitude|issue|_clean|flag_")) %>% filter(is.na(Longitude_clean)|is.na(Latitude_clean))
# Check the automatically cleaned lon/lat entries
check.cleaned <- response %>% select(uuid, matches("longitude|latitude|issue|_clean|flag_")) %>% filter(!is.na(Longitude_clean) | !is.na(Latitude_clean) & (!is.na(issue_lon) | !is.na(issue_lat)))

# 4. Second GPS check and cleaning step - Join valid GPS coordinates with corresponding districts/subdistrict, flag inconsistencies between GPS and recorded sub-district
gps.check.admin()                                                               # function defined in utils.R

### Check 4 - Flagging all gps issues + non matching subdistrict and reworking format to include in cleaning log
check_gps <- response.df %>%
  bind_rows(empty.gps.sites) %>%
  mutate(flag = ifelse(!(is.na(issue.gps) | issue.gps == "") | 
                         !(is.na(issue.admin3) | issue.admin3 == ""), T, F),
         agency=q0_3_organization, area=a4_site_name,
         issue = paste(issue.gps, issue.admin3, sep = " - ") %>% gsub(" - $|^ - ", "",.),
         internal.check = ifelse(flag & (is.na(Longitude_clean) & is.na(Latitude_clean) | issue.admin3!="" | grepl("outside of Yemen boundaries|Invalid|not recognized", issue)),
                                 F, ifelse(flag, T, NA)),
         .before = "q0_3_organization_other")

# Manually update the long/lat entries if relevant in Longlitude_clean and Latitude_clean
dir.create("output/cleaning log/gps check", showWarnings = F)
check_gps %>% select(uuid, matches("internal.check|longitude|latitude|issue|_clean|flag")) %>% write.xlsx(paste0("output/cleaning log/gps check/gps_manual_update", today,".xlsx"))
browseURL(paste0("output/cleaning log/gps check/gps_manual_update", today,".xlsx"))

## Open the gps_manual_update file to update manually any Longitude_clean and Latitude_clean entries that the script has missed
## If you're confident about the changes, put "TRUE" in the column internal.check to keep it in internal cleaning log
## After looking in the file and updating the Longitude_clean and Latitude_clean when relevant, save it with "_updated" at the end and run the lines below
check_gps <- read.xlsx("output/cleaning log/gps check/gps_manual_update2021-09-08_updated.xlsx") %>%
  left_join(response %>% select(-matches("longitude|latitude|issue|_clean|flag")), by="uuid")

## Separate internal gps cleaning log and external requiring Partner's feedback + small formatting changes for non flagged gps coordinates
gps_log_int <- check_gps %>% filter(flag == T & internal.check == T) %>%
  select(uuid, matches("internal|flag|longitude|latitude|issue|clean")) 

## Apply changes on response file for straightforward gps corrections
for (r in seq_along(1:nrow(gps_log_int))){
  if (nrow(gps_log_int)>0) {
    response[response$uuid == gps_log_int[r, "uuid"], c("a5_1_gps_longitude", "a5_2_gps_latitude")] <- c(gps_log_int[r, "Longitude_clean"], gps_log_int[r, "Latitude_clean"])
  }
}

## Map to display gps points that have issues  
df.adm2 <- adm2 %>% st_join(response.with.gps %>% st_as_sf() %>% select(issue.gps)) %>% mutate(has.gps.issue = ifelse(!is.na(issue.gps), 1, 0))
df.adm_loc <- response.with.gps %>% filter(!is.na(issue.gps)) %>% st_as_sf() %>%
  select(matches("^admin|longitude|latitude|Pcode"), everything())

pal.adm2 <- function(x) return(ifelse(x!=0, "indianred", "ghostwhite"))

map <- leaflet() %>% addTiles() %>%
  addPolygons(data=df.adm2, color = "#B5B5B5", weight = 2, opacity = 0.5,
              highlightOptions = highlightOptions(color = "white", weight = 2), fillOpacity = 0.5,
              fillColor = ~pal.adm2(df.adm2$has.gps.issue), label = df.adm2$admin2Name_en) %>%
  addMarkers(data = df.adm_loc, group = "All",
             label = paste0("Site name: ", df.adm_loc$a4_site_name," - The admin name entered in dataset is ", df.adm_loc$admin3Name_en_df, ",\r\nactual district according to GPS location is: ", df.adm_loc$admin3RefName_en)) %>%
  addMarkers(data = df.adm_loc %>% filter(admin3Name_en_df!=admin3Name_en), group = "Non matching GPS",
             label = paste0("Site name: ", df.adm_loc$a4_site_name," - The admin name entered in dataset is ", df.adm_loc$admin3Name_en_df, ",\r\nactual district according to GPS location is: ", df.adm_loc$admin3RefName_en)) %>%
  addLayersControl(baseGroups = c("All", "Non matching"), position =  "bottomleft", options = layersControlOptions(collapsed = FALSE))

map                                                                             # Display map
# withr::with_dir("./output/", map %>% mapshot(url="sitemap_cleaned_gps.html"))    # Export map as html file // uncomment if you want it as html, but it takes a while so commented so far

## Add internal and external gps issues
check_gps <- check_gps %>% dplyr::rename(agency=q0_3_organization, area=a4_site_name)

add.to.cleaning.log(checks = check_gps, check_id = "4",
                    question.names = c("a3_sub_district", "a5_1_gps_longitude", "a5_2_gps_latitude"), 
                    issue = "issue",
                    add.col = c("Longitude_clean", "Latitude_clean", "admin3Pcode.gps.matched", "admin3Name_en_df", "admin3Name_en.gps.matched", "internal.check"))
cleaning.log <- cleaning.log %>%                                                # only keep sub-district feedback when GPS falls outside of admin3 / could keep it for all (outside of ye / lon/lat switched) 
  filter(!(variable == "a3_sub_district" &
             !grepl("The sub-district name entered is not corresponding to the gps location entered", issue))) %>%
  mutate(new_value =                                                            # replace new_value in cleaning log by pre-cleaned value
           ifelse(variable == "a5_1_gps_longitude", Longitude_clean,
                  ifelse(variable == "a5_2_gps_latitude", Latitude_clean, new_value)))

## Filter out internal gps checks and append them to the internal cleaning log
cleaning.log.internal <- cleaning.log.internal %>% bind_rows(cleaning.log %>% filter(internal.check))
cleaning.log <- cleaning.log %>% filter(is.na(internal.check) | internal.check == F) %>% select(-internal.check)

## Check 4.1 Numerical outliers checks
col.num <- c("a7_site_population_hh", "a7_site_population_individual")          # Put all columns requiring numerical outlier check
method <- "sd-log"

check_outliers <- response %>% select(uuid, any_of(col.num)) %>%
  detect.outliers(., method=method, n.sd=3) %>%                               # n.sd will calibrate sensitivity of outlier detection. / see utils for other methods
  left_join(response %>% mutate(agency=q0_3_organization, area=a4_site_name, issue=paste0(method, " numerical outlier. The value seems to high/too low."), new_value="", check_id="4.1", fix="Checked with partner", checked_by="ON")
            %>% dplyr::select(any_of(col.cl)), by="uuid") %>%
  dplyr::select(all_of(col.cl)) %>% mutate_all(as.character)

if((s <- nrow(check_outliers)) > 0){print(paste0("There are ", s, " numerical outliers detected using ", method, " method for the following variables"))
  print(col.num)} else {print("No numerical outliers detected.")}

# Add to the cleaning log
cleaning.log <- cleaning.log %>% bind_rows(check_outliers)                      

## Check 5: Check adequacy
### Check that a service defined as a priority need is not classified as adequate
## The above function will add all priority needs check identified in priority_need_log to the existing cleaning log
# cleaning.log <- cleaning.log %>% filter(!grepl("check_priority", check_id))     # To clear out duplicates in case you run the function priority.need.check more than once
priority.need.checks(response) 
# adequacy_log %>% view                                                           # priority need log is stored in this dataframe

if(nrow(adequacy_log)==0) {print("No issues across avilability and service provisions have been detected. The dataset seems clean.")} else {
  print("Issues across avilability and service provisions have been detected. To be checked")}

### Check 6: Phone number 
### Flag all phone numbers in the data that don't start with the correct phone code for Yemen
phone.col <- c("phonenumber", "q1_3_key_informat_mobile_number", "b3_exu_fp_mobile_number", "b3_SCMCHAIC_fp_number",  "b6_cccm_agency_fp_mobile_number", "b4_smc_agency_fp_mobile_number", "b6_smc_agency_fp_mobile_number", "b5_smc_agency_fp_mobile_number")
# phone.col <- colnames(response)[grepl("phone|number|mobile", colnames(response))]
# v <- c(colnames(responsev1)[grepl("mobile|number", colnames(responsev1))], colnames(responsev2)[grepl("mobile|number", colnames(responsev2))]) %>% unique
# v %in% phone.col  # check that we have all columns with phone numbers from both tools
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
         issue = "The phone number provided may contain errors", check_id="6",
         old_value = number, new_value = "", 
         fix="Checked with partner", checked_by="ON") %>% 
  filter(number_wrong_number==1) %>% select(-number_wrong_number) %>%
  dplyr::rename(agency=q0_3_organization, area=a4_site_name)

if (nrow(phone_log)==0) {print("No issues with phone numbers. The dataset seems clean.")} else {
  print("Issue with some phone numbers detected. To be checked.")}

# Add phone issues to the general cleaning log
cleaning.log <- bind_rows(cleaning.log, phone_log)                               

### Check 7: Check that collective centre/location/Urban IDP displaced location is not a makeshift / emergency / transitional / open-air type of shelter
check_sitetype <- response %>%
  mutate(flag = ifelse(((c1_type_of_site == "collective_centre" | c1_type_of_site == "location" | c1_type_of_site == "Urban_displaced_IDP_location") & 
                          (c9_primary_shelter_type == "emergency_shelter" | c9_primary_shelter_type == "makeshift_shelter" | c9_primary_shelter_type == "transitional_shelter" | c9_primary_shelter_type == "open_air_no_shelter")), T, F),
         issue = ifelse(flag, "Primary shelter type marked as 'makeshfit', 'emergency', 'transitional', or 'open-air' although type of site was identified as 'Collective centre', 'Location', or 'Urban displaced IDP location'.", "")) %>%
  dplyr::rename(agency=q0_3_organization, area=a4_site_name)

if (nrow(check_sitetype %>% filter(flag))==0){print("Site type checks not needed. The dataset seems clean.")} else {
  print("Some collective centres/location/IDP urban displaced location have been reported as makeshift or emergency or transitional or open-air type of shelter. To be checked.")}

# Add to the cleaning log
add.to.cleaning.log(check_sitetype, check_id = "7", question.names = c("c1_type_of_site", "c9_primary_shelter_type"), issue = "issue")

### Check 7.1: Add check that spontaneous settlement is not a host-family apartment/house / rented house/apartment / public building?? 

check_settlement_type <- response %>%
  mutate(flag = ifelse((c1_type_of_site == "spontaneous_settlement") &
                         (c9_primary_shelter_type %in% c("host_family_house_apartment", "rented_house_appartment", "public_building")),
                       T, F),
         issue = ifelse(flag, "Primary shelter type marked as 'Host family house appartment', 'Rented house appartment' or 'Public building' although type of site was identified as 'Spontaneous settlement", "")) %>%
  dplyr::rename(agency=q0_3_organization, area=a4_site_name)

if (nrow(check_settlement_type %>% filter(flag))==0){print("Site type checks not needed. The dataset seems clean.")} else {
  print("Some spontaneous settlements have been reported as 'Host family house appartment', 'Rented house appartment' or 'Public building' type of shelter. To be checked.")}

# Add to the cleaning log
add.to.cleaning.log(check_settlement_type, check_id = "7.1", question.names = c("c1_type_of_site", "c9_primary_shelter_type"), issue = "issue")

### Check 8: Check that sites marked as type 'location" do not have more than 5 HH 
check_location <- response %>%    
  mutate(flag = ifelse((c1_type_of_site == "location" &  a7_site_population_hh > 5), T, F),
         issue = ifelse(flag, "Site type 'Location' can usually be only selected for IDP sites housing less than 5 HHs in ONE building.", "")) %>%
  dplyr::rename(agency=q0_3_organization, area=a4_site_name)

if (nrow(check_location %>% filter(flag))==0){print("Site type 'Location' checks not needed. The dataset seems clean.")} else {print("Some 'locations' have been reported to house more than 5 HHs. To be checked.")}          

# Add to the cleaning log
add.to.cleaning.log(check_location, check_id = "8", question.names = c("c1_type_of_site", "a7_site_population_hh"), issue = "issue")

### Check 9: Waste disposal cannot be "non-existent" if they selected flush latrines
check_waste_disposal <- response %>% 
  mutate(flag = ifelse((waste_disposal_services == "non_existant" & c10_primary_latrine_type == "flush_latrine_to_tank_sewage_system_pit"), T, F),
         issue = ifelse(flag, "Waste disposal marked as 'non-existant' although  'flush latrines' was selected as primary latrine type", "")) %>%
  dplyr::rename(agency=q0_3_organization, area=a4_site_name)

if (nrow(check_waste_disposal %>% filter(flag))==0){print("Waste disposal system checks not needed. The dataset looks clean.")} else {
  print("Some sites have reported having lush latrines while reporting waste disposal as non-existent. To be checked")}

# Add to the cleaning log
add.to.cleaning.log(check_waste_disposal, check_id = "9", question.names = c("waste_disposal_services", "c10_primary_latrine_type"), issue = "issue")
#write.csv(waste_disposal, paste0("./output/waste_disposal_issue_",today,".csv"), row.names = F)
#browseURL(paste0("./output/waste_disposal_issue_",today,".csv"))

### Check 10: that adequate WASH services do not include: flush latrine to the open / open defecation / pit uncovered / illegal water connection / unprotected well / surface water
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
add.to.cleaning.log(check_adequate_wash, check_id = "10", question.names = c("wash_services", "c10_primary_latrine_type"), issue = "issue")

### Check 11: Eviction risk should not be reported if there is a tenanacy agreement
check_eviction <- response %>%
  mutate(flag = ifelse((c3_tenancy_agreement_for_at_least_6_months == "yes" & f1_threats_to_the_site.eviction == 1), T, F),
         issue = ifelse(flag, "Eviction was identied as a risk although the site holds a tennacy agreement", "")) %>%
  dplyr::rename(agency=q0_3_organization, area=a4_site_name)

if (nrow(check_eviction %>% filter(flag))==0){print("No issues with eviction and type of tennacy has been detected. The dataset seems clean.")} else {
  print("For some sites, eviction was identied as a risk although the site holds a tennacy agreement. To be checked.")}

# Add to the cleaning log
add.to.cleaning.log(check_eviction, check_id = "11", question.names = c("c3_tenancy_agreement_for_at_least_6_months", "f1_threats_to_the_site.eviction"), issue = "issue")

### Check 12: Match and recode the other entries for if_ngo questions 
check_service_provider <- response %>%
  select(uuid, matches("^if_ngo.*_other$")) %>%
  pivot_longer(cols = 2:ncol(.)) %>% filter(!is.na(value) & !(value %in% c("لااعلم", "لا اعلم"))) %>%
  mutate(ngo.name = gsub("\\(|\\)", "", tolower(value)), id = 1:nrow(.)) %>%
  left_join(choices.ngo %>%                                                     # Matching organization other in arabic with ngo code and english names  
              mutate(ngo_name_ar_lower = tolower(ngo_name_ar)), 
            by = c("ngo.name" = "ngo_name_ar_lower"))

check_service_provider2 <- check_service_provider %>% 
  filter(is.na(ngo_code)) %>% select(-matches("ngo_")) %>%
  mutate(ngo.name = gsub("^si$", "sol", ngo.name)) %>%                          # clean code for sol / si
  left_join(choices.ngo %>% mutate(ngo_code_lower=str_trim(tolower(ngo_code))), # Trim whitespace + lower fhi360 code which is upper and has a space
            by = c("ngo.name" = "ngo_code_lower")) %>%
  mutate(ngo_code = ifelse(!is.na(ngo_name_en), ngo.name, NA))                  # extract ngo code when match worked 

service_provider_log <- check_service_provider2 %>%                             # Bind together the perfect arabic matches with ngo code matches
  bind_rows(check_service_provider %>% filter(!id %in% check_service_provider2$id)) %>%
  relocate(id, .before = 1) %>% relocate(ngo_code, .before = "ngo_name_en") %>%
  dplyr::rename(variable=name, old_value=value, value_cleaned=ngo.name, new_value=ngo_code) %>%
  left_join(response %>% select(uuid, q0_3_organization, a4_site_name) %>%
              dplyr::rename(agency=q0_3_organization, area=a4_site_name), by="uuid") %>%
  mutate(issue="The organisation name is not recognized or there are multiple organisation names.",
         check_id = paste0("12_","check_", gsub("_other|if_","",variable)),
         fix="Checked with partner", checked_by="ON") %>%
  select(all_of(col.cl), everything(), -value_cleaned, -id) %>%
  mutate(external= ifelse(is.na(new_value), T, F), new_org="")                  # flag as external issues with no perfect match + add column to flag new organizations

service_provider_log_ext <- service_provider_log %>% filter(external)
service_provider_log_int <- service_provider_log %>% filter(external==FALSE)

# Manually updated the remaining other entries before adding to external cleaning log if necessary
dir.create("output/cleaning log/service provider", showWarnings = F)
service_provider_log_ext %>% save.service.provider.follow.up(paste0("output/cleaning log/service provider/service_provider_other_", today, ".xlsx"))
 browseURL(paste0("output/cleaning log/service provider/service_provider_other_", today, ".xlsx"))

# After updating the service provider other log, save it with _updated at the end of the file name / update filename below to read the updated file
file.service.provider.log <- "output/cleaning log/service provider/service_provider_other_2021-09-01_updated.xlsx"
service_provider_log_ext_updated <- read.xlsx(file.service.provider.log) %>% 
  mutate_all(as.character) %>%
  mutate(external=ifelse(new_value %in% choices.ngo$ngo_code, F, T),            # flag as non external follow-up when matching has been done manually with a ngo code from the list
         issue=ifelse(is.na(new_org), issue, ifelse(new_org=="TRUE", "No matching organization has been found. Is this a new organization?", issue))) 

service_provider_log_int <- service_provider_log_int %>%                        # Add changes done internally to internal cleaning log
  bind_rows(service_provider_log_ext_updated %>% filter(external==FALSE)) %>%
  mutate(issue = "Service provider organization has been marked as 'other' although it is part of the available choices.")
service_provider_log_ext <- service_provider_log_ext_updated %>% filter(external==TRUE)

## reformat cleaning log to include recode question with the other entry
service_provider_log_other <- service_provider_log_int %>% bind_rows(service_provider_log_ext) %>%
  mutate(new_value = ifelse(!is.na(new_value), NA, new_value))
service_provider_log_recode <- service_provider_log_int %>% bind_rows(service_provider_log_ext) %>%
  mutate(variable = gsub("_other", "", variable), old_value = "other")  
service_provider_log_final <- service_provider_log_other %>% bind_rows(service_provider_log_recode) %>%
  arrange(uuid, variable) 

# Add perfect matches to internal cleaning log
cleaning.log.internal <- cleaning.log.internal %>% bind_rows(service_provider_log_final %>% filter(external==F) %>% select(-external))

# Add remaining other entries that require feedback from partners to the cleaning log
cleaning.log <- cleaning.log %>% bind_rows(service_provider_log_final %>% filter(external==T) %>% select(-external))

## Apply changes for perfect matches - recode service provider with other entry and put other as NA
service_provider_log_final_int <- service_provider_log_final %>% filter(external==F)
for (r in seq_along(nrow(service_provider_log_final_int))){
  var <- service_provider_log_final_int[r, "variable"] %>% as.character
  new_value <- service_provider_log_final_int[r, "new_value"] %>% as.character
  response[response$uuid == as.character(service_provider_log_final_int[r, "uuid"]), var] <- new_value
}

### Check 13: Survey length 
### Check lenght of the survey, 15 = minimum, 60 = maximum (can be changed)
# check_survey_length <- check.time(response, duration_threshold_lower = 15, duration_threshold_upper = 60) %>%
#   mutate(old_value = as.character(old_value), check_id = "13") 
# if (nrow(check_survey_length)==0){print(paste0("No survey has been flagged as too short/long with the given parameters entered."))} else {
#   print("Some survey lengths are outside of the minimum and maximum duration parameters entered. To be checked.")}
## Add to the cleaning log
# cleaning.log <- bind_rows(cleaning.log, check_survey_length)                  # time check commented for now as it seems not relevant

cleaning.log <- cleaning.log %>%
  mutate(change = "", comment="") %>%                                           # adding "change" column to be filled later (will determine whether changes must be done or not)
  relocate(change, comment, .after="new_value") %>%
  arrange(agency, check_id, uuid)                                               

## Internal check for duplicate (To make sure you didn't do rubbish when copypasting code for a new logical check
cleaning.log.dup <- cleaning.log %>%
  group_by(uuid, variable, check_id) %>% arrange(uuid, variable)  %>% mutate(n=n()) %>% filter(n>1)
if ((t <- nrow(cleaning.log.dup)) > 0) {print(paste0("There are ", t, " duplicates in the cleaning log. Check if it makes sense. You might have run the function 'add.to.cleaning.log' more than once for the same check or you have copy pasted some code without updating the 'check' argument of the function."))}

### Cleaning log for Partners
### Saves and put in format cleaning log 
dir.create("output/cleaning log", showWarnings = F)
save.follow.up.requests(cleaning.log, filename.out=paste0("output/cleaning log/cleaning_log_ext_all_", today,".xlsx"))
 browseURL(paste0("output/cleaning log/cleaning_log_ext_all_", today,".xlsx"))

### Split all cleaning log by partner
dir.create("output/cleaning log/partners", showWarnings = F, recursive = T)
for (org in unique(cleaning.log$agency)) {
  cl.org <- cleaning.log %>% filter(agency == org) %>%
    save.follow.up.requests(., filename.out=paste0("output/cleaning log/partners/cleaning_log_", today, "_", org, ".xlsx"))
  print(paste0("cleaning log for ", org, " organization saved in folder output/cleaning log/partners"))
}

### Internal cleaning log [Sitename, organisation name matching and obvious gps coordinates correction] 
save.follow.up.requests(cleaning.log.internal, filename.out = paste0("output/cleaning log/cleaning_log_int_all_", today, ".xlsx"))

## Anonymize dataset - Update sensible columns vector by adding all new headers containing sensible information that is not needed for data followup.
# It will ignore non matching columns, just put all columns from both tools
sensible.columns <- c("deviceid", "subscriberid", "imei", "phonenumber", "q0_1_enumerator_name", "q0_2_gender", "q1_1_key_informant_name",
                      "q1_2_key_informat_gender", "q1_3_key_informat_mobile_number", "a5_gps_coordinates", "calca11", "__version__", "_id", "_submission_time", "_validation_status")

### Append relevant info to response file and save version with PII for internal use + anonymised to share to partners
response <- response %>% 
  select(any_of(col.raw)) %>%                                                   # take only the original columns
  left_join(sitename_log_updated %>%                                            # add the sitename match proposed (id, english, arabic)
              filter(!duplicated(uuid)) %>%
              select(uuid, a4_site_name_new, a4_site_name_new_en, a4_site_name_new_ar),
            by = "uuid") %>% relocate(matches("a4_site_name_new"), .after = "a4_other_site")  
response %>% write.xlsx(paste0("output/internal/CCCM_SiteReporting_",tool.version,"_Internal_",today,".xlsx"))

### Save anonymized version of response file with cleaned long/lat + intermediate columns created in case
response_to_share <- response %>% 
  select(any_of(sensible.columns))                                              # anonymise dataset, deselect geometry column
  
response_to_share %>% write.xlsx(file = paste0("output/response_updated_", today,".xlsx"))          # Write whole dataset to be shared

### Save one dataset to share by partner
dir.create("output/cleaning log/data partners", showWarnings = F, recursive = T)
for (org in unique(response_to_share$q0_3_organization)){
  response_org <- response_to_share %>% filter(q0_3_organization==org) %>%
    write.xlsx(paste0("output/cleaning log/data partners/data_anonymised_", today, "_", org, ".xlsx"))
  print(paste0("Anonymised dataset for ", org, " organization saved in folder output/cleaning log/data partners"))
}

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
