library(galah)
library(tidyverse)
library(janitor)
library(arrow)

galah_config(email = Sys.getenv("ALA_EMAIL"),
             atlas = "Australia")

datasets_of_interest <- c("Australia's Virtual Herbarium","iNaturalist observations","iNaturalist research-grade observations")

# NSW_plants <- 
  galah_call() |> 
  galah_identify("Plantae") |> 
  galah_filter(stateProvince == "New South Wales", 
               datasetName == datasets_of_interest,
               species != "",
               decimalLatitude != "",
               year >= 1923) |> 
  galah_select(recordID, species, genus,family,
               decimalLatitude,decimalLongitude,coordinateUncertaintyInMeters,
               eventDate,datasetName,basisOfRecord,
               references,institutionCode,recordedBy) |> 
   #atlas_occurrences() 
   atlas_counts()

  NSW_plants |>
  filter(is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters <=1000) |>
  nrow()

write_parquet(NSW_plants, paste0("data/NSW_plants_", Sys.Date()))

# NSW_plants |> 
#   filter(datasetName == "Australia's Virtual Herbarium")
# 
# ala |> 
#   mutate(year = year(collectionDate)) |> 
#   filter(year >= 1923) |> 
#   drop_na(year, species, lat) |> 
#   nrow()
