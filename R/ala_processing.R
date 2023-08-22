library(tidyverse)
library(data.table)
library(APCalign) #remotes::install_github("traitecoevo/APCalign")
datasets_of_interest<-c("Australia's Virtual Herbarium","iNaturalist observations","iNaturalist research-grade observations")
ala_dl<-list.files("ala_data/records-2023-08-07/",full.names = TRUE)

read_in_and_subset<-function(path){
  read_csv(path) %>%
    select(species,decimalLatitude,decimalLongitude,eventDate,datasetName,
           coordinateUncertaintyInMeters,basisOfRecord,references,institutionCode,family,recordedBy)
}

all_ss<-map_df(ala_dl,read_in_and_subset)
#filter(all_ss,references=="https://www.inaturalist.org/observations/88908727")
#table(all_ss$basisOfRecord)
#problems<-filter(all_ss[[5]],is.na(year(eventDate)))


datasets_of_interest<-c("Australia's Virtual Herbarium","iNaturalist observations","iNaturalist research-grade observations")
all_ss %>%
  select(species,decimalLatitude,decimalLongitude,eventDate,datasetName,
         coordinateUncertaintyInMeters,basisOfRecord,references,institutionCode,family,recordedBy) %>%
  filter(basisOfRecord=="PRESERVED_SPECIMEN" | datasetName %in% datasets_of_interest) %>%
  filter(is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters<=1000) %>%
  filter(!is.na(decimalLatitude) & !is.na(species))-> ala_inat_avh

ala_inat_avh$voucher_location<-case_when(!is.na(ala_inat_avh$references) ~ ala_inat_avh$references,
                                         TRUE ~ ala_inat_avh$institutionCode)


ala_inat_avh$genus<-stringr::word(ala_inat_avh$species,1,1)
ala_inat_avh$voucher_type<-case_when(ala_inat_avh$basisOfRecord=="PRESERVED_SPECIMEN" ~ "Collection",
                            TRUE ~ "Photograph") 
ala_inat_avh$lat<-ala_inat_avh$decimalLatitude  
ala_inat_avh$long<-ala_inat_avh$decimalLongitude
ala_inat_avh$collectionDate<-lubridate::ymd(word(ala_inat_avh$eventDate,1,1)) # check that time zones are working?
lu<-native_anywhere_in_australia(unique(ala_inat_avh$species[stringi::stri_count_words(ala_inat_avh$species)==2]))
ala_inat_avh<-left_join(ala_inat_avh,lu)
ala_inat_avh$native_anywhere_in_aus<-case_when(ala_inat_avh$native_anywhere_in_aus=="considered native to Australia by APC" ~ "Native",
                                               ala_inat_avh$native_anywhere_in_aus=="not considered native to Australia by APC" ~ "Introduced",
                                               TRUE ~ "unknown") 

#trying to get file as small as possible
ala_inat_avh %>%
  select(species,genus,family,collectionDate,lat,long,voucher_type,voucher_location,recordedBy,native_anywhere_in_aus) %>%
  filter(!is.na(genus)&!is.na(species)&!is.na(lat)&!is.na(long)&!is.na(voucher_type)) %>%
 # filter(collectionDate>1800) %>%
  write_csv("ala_nsw_inat_avh.csv")


#ala_inat_avh %>% filter(basisOfRecord=="PRESERVED_SPECIMEN") -> only_collections
#ala<-read_csv("ala_nsw_inat_avh.csv")


# read_in_and_filter<-function(path){
#   read_csv(path) %>%
#     filter(basisOfRecord=="PRESERVED_SPECIMEN" | datasetName %in% datasets_of_interest)
# }
# 
# all_filt<-map(ala_dl[[1]],read_in_and_filter)
