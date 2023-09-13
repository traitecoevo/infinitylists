

# Libraries
library(galah)
library(dplyr)
library(janitor)
library(arrow)

download_ala_obs <- function(taxa, 
                             year_range = c(1923, 2023),
                             save_raw_data = FALSE,
                             output_dir = "infinity-app/data/") {
  
  # 1. Data retrieval
  ala_obs <- retrieve_data(taxa, year_range, save_raw_data, output_dir)
  
  # 2. Filtering and processing
  ala_cleaned <- process_data(ala_obs)
  
  # 3. Add additional columns
  ala_cleaned_with_add_ons <- get_establishment_status(ala_cleaned, taxa)
  
  # 4. Save processed data
  save_data(ala_cleaned_with_add_ons, taxa, output_dir)
  
 
}


retrieve_data_by_years <- function(taxa, 
                                   years, 
                                   save_raw_data = NULL, 
                                   output_dir =NULL) {
  
  # Download data from ALA
  download <- galah_call() |> 
    galah_identify(taxa) |> 
    galah_filter(
      spatiallyValid == TRUE, 
      species != "",
      decimalLatitude != "",
      year == years,
      basisOfRecord == c("HUMAN_OBSERVATION", "PRESERVED_SPECIMEN")
    ) |> 
    galah_select(
      recordID, species, genus, family, decimalLatitude, decimalLongitude, 
      coordinateUncertaintyInMeters, eventDate, datasetName, basisOfRecord, 
      references, institutionCode, recordedBy, outlierLayerCount, isDuplicateOf
    ) |> 
    atlas_occurrences()
  
  # Save download (optional)
  if(save_raw_data)
    arrow::write_parquet(
      download,
      paste0(output_dir, "ALA-Australia-", taxa, "-", first(years), 
             "-", last(years), "-",  Sys.Date(), ".parquet")
    )
  
  return(download)
}



retrieve_data <- function(taxa, 
                          year_range = c(1923, 2023),
                          save_raw_data = FALSE, 
                          output_dir = "ignore/"){
  
  # Split years into chunks of 10 year intervals
  years <- seq(year_range[1], year_range[2])
  
  year_chunks <- split(years, ceiling(seq_along(years)/9))
  
  download <-  purrr::map(year_chunks,
                          purrr::possibly(~retrieve_data_by_years(taxa, years = .x, save_raw_data, output_dir))
  )
  
  # Collapse list
  output <- dplyr::bind_rows(download)
  
  return(output)
  
}


get_establishment_status <- function(ala_cleaned, taxa = taxa) {
  if (taxa == "Plantae") {
    resources <- APCalign::load_taxonomic_resources()
    lookup <-
      native_anywhere_in_australia(ala_cleaned$Species, resources = resources)
    lookup <- rename(lookup, Species = species)
    ala_cleaned <-
      ala_cleaned %>% dplyr::left_join(lookup, by = join_by("Species"))
  }
  return(ala_cleaned)
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
      species:family, collectionDate, lat, long, voucher_type, voucher_location, recordedBy,recordID
    ) |> 
    clean_names("title")
}

save_data <- function(data, taxa, output_dir) {
  write_parquet(
    data,
    paste0(output_dir, "Australia-", taxa, "-", Sys.Date(), ".parquet")
  )
}

# job::job(packages = c("purrr", "dplyr", "arrow", "janitor", "galah", "stringr", "lubridate"), {
#   download_ala_obs(taxa = "Odonata")
# })

