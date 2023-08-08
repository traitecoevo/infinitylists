library(tidyverse)
library(data.table)
datasets_of_interest<-c("Australia's Virtual Herbarium","iNaturalist observations","iNaturalist research-grade observations")
ala_dl<-list.files("ala_data/records-2023-08-07/",full.names = TRUE)

read_in_and_subset<-function(path){
  read_csv(path) %>%
    select(species,decimalLatitude,decimalLongitude,eventDate,datasetName,
           coordinateUncertaintyInMeters,basisOfRecord,references)
}

#test<-read_csv(ala_dl[5])


all_ss<-map_df(ala_dl,read_in_and_subset)
filter(all_ss,references=="https://www.inaturalist.org/observations/88908727")

table(all_ss$basisOfRecord)

datasets_of_interest<-c("Australia's Virtual Herbarium","iNaturalist observations","iNaturalist research-grade observations")
all_ss %>%
  select(species,decimalLatitude,decimalLongitude,eventDate,datasetName,
         coordinateUncertaintyInMeters,basisOfRecord,references) %>%
  filter(basisOfRecord=="PRESERVED_SPECIMEN" | datasetName %in% datasets_of_interest) %>%
  filter(is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters<=1000) %>%
  filter(!is.na(decimalLatitude) & !is.na(species))-> ala_inat_avh
write_csv(ala_inat_avh,"ala_nsw_inat_avh.csv")


ala_inat_avh %>% filter(basisOfRecord=="PRESERVED_SPECIMEN") -> only_collections
  

ala<-read_csv("ala_nsw_inat_avh.csv")
