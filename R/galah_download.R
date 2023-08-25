

# Libraries
library(galah)
library(tidyverse)
library(janitor)
library(arrow)
library(here)
library(APCalign)
library(skimr)

download_ala_obs <- function(taxa = "Plantae", output_dir = "infinity-app/data/") {
  
  # 1. Data retrieval
  NSW_obs <- retrieve_data(taxa)
  
  # 2. Filtering and processing
  NSW_cleaned <- process_data(NSW_obs)
  
  # 3. Save processed data
  save_data(NSW_cleaned, taxa, output_dir)
}

retrieve_data <- function(taxa) {
  galah_call() |> 
    galah_identify(taxa) |>
    galah_apply_profile("CSDM") |>
    galah_filter(
      #stateProvince == state,
      species != "",
      decimalLatitude != "",
      year >= 1923,
      basisOfRecord == c("HUMAN_OBSERVATION", "PRESERVED_SPECIMEN")
    ) |>
    galah_select(
      recordID, species, genus, family, decimalLatitude, decimalLongitude, 
      coordinateUncertaintyInMeters, eventDate, datasetName, basisOfRecord, 
      references, institutionCode, recordedBy
    ) |>
    atlas_occurrences()
}

process_data <- function(data) {
  datasets_of_interest <- c(
    "Australia's Virtual Herbarium",
    "iNaturalist observations",
    "iNaturalist research-grade observations"
  )
  
  data |> 
    filter(
      basisOfRecord == "PRESERVED_SPECIMEN" | datasetName %in% datasets_of_interest,
      is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters <= 1000,
      !is.na(eventDate),
      !str_detect(species, "spec.$")
    ) |>
    mutate(
      voucher_location = if_else(!is.na(references), references, institutionCode),
      voucher_type = if_else(basisOfRecord == "PRESERVED_SPECIMEN", "Collection", "Photograph"),
      lat = decimalLatitude,
      long = decimalLongitude,
      collectionDate = ymd_hms(eventDate, tz = "UTC", quiet = TRUE),
      collectionDate = if_else(is.na(collectionDate), ymd(eventDate, tz = "UTC", quiet = TRUE), collectionDate)
    ) |> 
    select(
      species:family, collectionDate, lat, long, voucher_type, voucher_location, recordedBy
    ) |> 
    clean_names("title")
}

save_data <- function(data, taxa, output_dir) {
  write_parquet(
    data,
    paste0(output_dir, "Australia-", taxa, "-", Sys.Date(), ".parquet")
  )
}



download_ala_obs()
