# Site Reporting Aggreagation Tool
# REACH YEMEN Team - alberto.gualtieri@reach-initiative.org
# V1
# 27/01/2020

rm(list=ls())
today <- Sys.Date()

### Download custom packages
# devtools::install_github("mabafaba/hypegrammaR")
# devtools::install_github("mabafaba/composR", force = T, build_vignettes = T)
# devtools::install_github("mabafaba/xlsformfill", force = T, build_vignettes = T)

### Load required library
library("tidyverse")
library("hypegrammaR")
library("xlsformfill")
library("openxlsx")
library("stringr")
library("srvyr")
library("reshape")

### Load source
source("./R/functions/functions.R")
source("./R/functions/from_hyperanalysis_to_datamerge.R")
source("./R/functions/moveme.R")
source("./R/add_locations.R")

## Set up filenames
filename.data <- "data/CCCM_SiteReporting_All Internal_2021-10-05.xlsx"
filename.tool <- "data/CCCM_Site_Reporting_Kobo_tool_V1_23_05_2021_FINAL.xlsx"
# filename.tool <- "data/CCCM_Site_Reporting_Kobo_tool_V2_23_05_2021_FINAL.xlsx"
filename.dap <- "data analysis plan/cccm_analysis_plan_v4_country.csv"

### Load data and filter out "informal"
response <- read.xlsx(filename.data)
# response <- filter(response, response$a9_formal_informal == "formal")

### Load questionnaire
questions <- read.xlsx(filename.tool, sheet=1)
choices <- read.xlsx(filename.tool, sheet=2)
external_choices <- read.xlsx(filename.tool, sheet=3)
# questions <- read.csv("./data/kobo/questions.csv", check.names = F)
# choices <- read.csv("./data/kobo/choices.csv", check.names = F)
# external_choices <- read.csv("./data/kobo/external_choices.csv", check.names = F)

questionnaire <- load_questionnaire(response,
                                    questions,
                                    choices,
                                    choices.label.column.to.use = "label::english")

### Load Data Analysis Plan
dap <- load_analysisplan(filename.dap)

### Create sampling function and load the weights (useless but the analysis output function requires it)
#sf <- read.csv("./data/sf.csv")
#sf <- sf[!duplicated(sf), ]

#response$stratum_id <- response$a4_site_code
#response$stratum_id <- str_replace(response$stratum_id, " - ", "_")

#weight.function <- map_to_weighting(sf, "stratum_id", "population", "stratum_id")


### Fix old entries with new names
response$c10_primary_latrine_type <- ifelse(response$c10_primary_latrine_type == "open_air", "open_defaction", response$c10_primary_latrine_type)


### Create extra variables
### Issues with eviction but having tennecy agreement
response <- mutate(response, agreement_with_issue= ifelse((response$c3_tenancy_agreement_for_at_least_6_months == "yes" & 
                                                             response$f1_threats_to_the_site.eviction == "1"), "1", "0"))

response <- mutate(response, agreement_with_issue = ifelse((response$c3_tenancy_agreement_for_at_least_6_months == "no"), NA, agreement_with_issue))

#check <- select(response, c("a1_governorate_name", "c3_tenancy_agreement_for_at_least_6_months", "f1_threats_to_the_site.eviction", "agreement_with_issue"))
#check <- check %>% filter(a1_governorate_name == "Dhamar" | a1_governorate_name == "Sana'a")

### Max number of site by country
response$site_num <- 1
max_dist <- aggregate(response$site_num, list(country_name = response$country_name), sum)
names(max_dist)[names(max_dist) == "x"] <- "tot_sites"

### Total number of households and individuals by country
tot_hh <- aggregate(as.numeric(response$a7_site_population_hh), list(country_name = response$country_name), sum)
names(tot_hh)[names(tot_hh) == "x"] <- "tot_hh"

tot_ind <- aggregate(as.numeric(response$a7_site_population_individual), list(country_name = response$country_name), sum)
names(tot_ind)[names(tot_ind) == "x"] <- "tot_ind"

#### Most common district of origin analysis
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

external_choices <- filter(external_choices, external_choices$list_name == "district")

### Weighted analysis
reported_dist <- select(response, c("country_name", "a7_site_population_individual", "d2_1_most_common_district_of_idp_origin"))

names(external_choices)[names(external_choices) == "name"] <- "d2_1_most_common_district_of_idp_origin"

reported_dist$d2_1_most_common_district_of_idp_origin <- external_choices$`label::english`[match(reported_dist$d2_1_most_common_district_of_idp_origin, 
                                                                                                 external_choices$d2_1_most_common_district_of_idp_origin)]


most_reported_dis_wgt <- aggregate(as.numeric(reported_dist$a7_site_population_individual), 
                                   list(country_name = response$country_name, most_district = reported_dist$d2_1_most_common_district_of_idp_origin), sum)

most_reported_dis_wgt <- most_reported_dis_wgt %>% group_by(country_name) %>% top_n(1, x)

most_reported_dis_wgt$x <- NULL



### Most common reason for leaving
leaving_reas <- select(response, c("country_name", "a7_site_population_individual", "d1_most_common_reason_idps_left_place_of_origin"))

names(choices)[names(choices) == "name"] <- "d1_most_common_reason_idps_left_place_of_origin"

leaving_reas$d1_most_common_reason_idps_left_place_of_origin <- choices$`label::english`[match(leaving_reas$d1_most_common_reason_idps_left_place_of_origin, 
                                                                                                 choices$d1_most_common_reason_idps_left_place_of_origin)]

most_reported_leave_wgt <- aggregate(as.numeric(leaving_reas$a7_site_population_individual),
                                     list(country_name = response$country_name, most_leave_reason = leaving_reas$d1_most_common_reason_idps_left_place_of_origin), sum)

most_reported_leave_wgt <- most_reported_leave_wgt %>% group_by(country_name) %>% top_n(1, x)

most_reported_leave_wgt$x <- NULL


### Most common intention in the next three months
intentions <- select(response, c("country_name", "a7_site_population_hh", "d3_most_common_intention_in_next_three_months"))


names(choices)[names(choices) == "d1_most_common_reason_idps_left_place_of_origin"] <- "d3_most_common_intention_in_next_three_months"

intentions$d3_most_common_intention_in_next_three_months <- choices$`label::english`[match(intentions$d3_most_common_intention_in_next_three_months, 
                                                                                               choices$d3_most_common_intention_in_next_three_months)]


most_reported_int_wgt <- aggregate(as.numeric(intentions$a7_site_population_hh),
                                   list(country_name = response$country_name, most_intention = intentions$d3_most_common_intention_in_next_three_months), sum)

most_reported_int_wgt <- most_reported_int_wgt %>% group_by(country_name) %>% top_n(1, x)

most_reported_int_wgt$x <- NULL



### Join everything
library("plyr")

external_analysis <- join_all(list(max_dist, tot_hh, tot_ind, most_reported_dis_wgt, most_reported_int_wgt, most_reported_leave_wgt),
                              by = "country_name")

### Label full dataset before running the analysis
names(choices)[names(choices) == "d3_most_common_intention_in_next_three_months"] <- "name"
response_ren <- response

response_ren <- response_ren[moveme(names(response_ren), "uuid first")]
response_ren[18:147] <- choices$`label::english`[match(unlist(response_ren[18:147]), choices$name)]
response_ren <- response_ren %>% select(-any_of(c("X__version__", "X_id", "X_submission_time", "X_validation_status")))

dap <- dap %>%
  ## For now get rid of the two additionnal indicators to match existing question from the tool. see if needed to change it 
  ## If both needed, uncomment the three lines below and update manually dap to add needed columns and mutate the new indicators in the script
  dplyr::mutate(across(matches("research|hypothesis$"), ~ifelse(dependent.variable=="c1_2_type_of_site", "% of sites, by type of site", .)),
                dependent.variable = ifelse(dependent.variable=="c1_2_type_of_site", "c1_type_of_site", dependent.variable)) %>%
  filter(!dependent.variable %in% c("c1_1_type_of_site")) %>%
  ## Filter to keep only columns that exist in input data // if you want to keep all original indicators from the dap, make sure the columns exist in dataset or mutate them
  filter(dependent.variable %in% colnames(response_ren)) %>% 
  ## hypegrammar needs at least one row with repeat var as non NA to work, independent.var set up as repeat.for.variable [will be reset as independent.var later]
  dplyr::mutate(repeat.for.variable="country_name", across(matches("independent"), ~NA)) 

### Launch Analysis Script
analysis <- from_analysisplan_map_to_output(data = response_ren,
                                            analysisplan = dap,
                                            weighting = NULL,
                                            questionnaire = questionnaire)




## SUMMARY STATS LIST ##
summarystats <- analysis$results %>% lapply(function(x) x[["summary.statistic"]]) %>% bind_rows %>%
  mutate(independent.var="country_name", independent.var.value=repeat.var.value, repeat.var.value=NA)
# questions
write.csv(summarystats, paste0("./output/summarystats_final_nat_",today,".csv"), row.names = F)

### Load the results and lunch data merge function
final_analysis <- read.csv(paste0("./output/summarystats_final_nat_",today,".csv"), stringsAsFactors = F)
final_melted_analysis <- from_hyperanalysis_to_datamerge(final_analysis)

#### Multiply everything by 100, round everything up, and replace NAs with 0
final_dm <- cbind(final_melted_analysis[1], lapply(final_melted_analysis[-1],function(x) x*100))

final_dm[,-1] <- round(final_dm[,-1],0)

final_dm[is.na(final_dm)] <- 0


### Join indicators not analyzed by hypegrammaR
names(final_dm)[names(final_dm) == "independent.var.value"] <- "country_name"
data_merge <- left_join(final_dm, external_analysis, by = "country_name")

#write.xlsx(data_merge, paste0("./output/governorate_data_merge_",today,".xlsx"))
#browseURL(paste0("./output/governorate_data_merge_",today,".xlsx"))


### Add maps to the fina data merge and save it as .csv file
data_merge$`@map` <- paste0("./maps/YEM_CCCM_",data_merge$country_name,".pdf")

names(data_merge) <- tolower(names(data_merge))

write.csv(data_merge, paste0("./output/national_data_merge_",today,".csv"), row.names = F)                     
browseURL(paste0("./output/cccm_national_full_merge_",today,".csv"))
