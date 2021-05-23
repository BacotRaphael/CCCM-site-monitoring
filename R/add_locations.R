add.location<-function(data){
  #library("dplyr")
  location.data <- read.csv("./R/pcodes/admin_list.csv",header=T,sep=",", encoding = "UTF-8", check.names=F, stringsAsFactors=FALSE)
  location <- as.data.frame(location.data)
  
  #admin1Name
  location_merge1<-location[,c(colnames(location[c(4,6)]))]
  location_merge1 <- unique(location_merge1)
  plyr::rename(location_merge1, c("admin1Name_en" = "a1_governorate_name", "admin1Pcode" = "a1_governorate")) -> admin1_merge
  
  
  #admin2Name
  location_merge2<-location[,c(colnames(location[c(7,9)]))]
  lOcation_merge2 <- unique(location_merge2)
  plyr::rename(lOcation_merge2, c("admin2Name_en" = "a2_district_name", "admin2Pcode" = "a2_district")) -> admin2_merge
  
  #admin3Name
  location_merge3<-location[,c(colnames(location[c(12,14)]))]
  location_merge3<-unique(location_merge3)
  plyr::rename(location_merge3, c("admin3RefName_en" = "a3_sub_district_name", "admin3Pcode" = "a3_sub_district")) -> admin3_merge
  
  #Merge
  data <- dplyr::left_join(data, admin1_merge, by = "a1_governorate") 
  data <- dplyr::left_join(data, admin2_merge, by = "a2_district")
  data <- dplyr::left_join(data, admin3_merge, by = "a3_sub_district")
  
  #Add Country name and code
  data$country_name <- "Yemen"
  data$country_id <- "YE"
  
  return(data)
}

