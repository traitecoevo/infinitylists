library(tidyverse)
library(data.table)
datasets_of_interest<-c("Australia's Virtual Herbarium","iNaturalist observations","iNaturalist research-grade observations")
ala_dl<-list.files("ala_data/records-2023-08-07/",full.names = TRUE)

read_in_and_subset<-function(path){
  read_csv(path) %>%
    select(species,decimalLatitude,decimalLongitude,eventDate,datasetName,
           coordinateUncertaintyInMeters,basisOfRecord,references)
}

all_ss<-map_df(ala_dl,read_in_and_subset)
#filter(all_ss,references=="https://www.inaturalist.org/observations/88908727")
#table(all_ss$basisOfRecord)
#problems<-filter(all_ss[[5]],is.na(year(eventDate)))


datasets_of_interest<-c("Australia's Virtual Herbarium","iNaturalist observations","iNaturalist research-grade observations")
all_ss %>%
  select(species,decimalLatitude,decimalLongitude,eventDate,datasetName,
         coordinateUncertaintyInMeters,basisOfRecord,references) %>%
  filter(basisOfRecord=="PRESERVED_SPECIMEN" | datasetName %in% datasets_of_interest) %>%
  filter(is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters<=1000) %>%
  filter(!is.na(decimalLatitude) & !is.na(species))-> ala_inat_avh

ala_inat_avh$voucher_location<-case_when(!is.na(ala_inat_avh$references) ~ ala_inat_avh$references,
                                         TRUE ~ ala_inat_avh$datasetName)


ala_inat_avh$taxa<-stringr::word(ala_inat_avh$species,1,1)
ala_inat_avh$voucher_type<-case_when(ala_inat_avh$basisOfRecord=="PRESERVED_SPECIMEN" ~ "Collection",
                            TRUE ~ "Photograph") 
ala_inat_avh$lat<-ala_inat_avh$decimalLatitude  
ala_inat_avh$long<-ala_inat_avh$decimalLongitude
ala_inat_avh$year<-year(ala_inat_avh$eventDate)

#trying to get file as small as possible
ala_inat_avh %>%
  select(taxa,species,year,lat,long,voucher_type,voucher_location) %>%
  filter(!is.na(taxa)&!is.na(species)&!is.na(year)&!is.na(lat)&!is.na(long)&!is.na(voucher_type)) %>%
  filter(year>1800) %>%
  write_csv("ala_nsw_inat_avh.csv")


#ala_inat_avh %>% filter(basisOfRecord=="PRESERVED_SPECIMEN") -> only_collections
#ala<-read_csv("ala_nsw_inat_avh.csv")
