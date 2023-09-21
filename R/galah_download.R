#' Download Observations for Infinity List
#'
#' This function downloads observation data for the given taxa and year range
#' from the "Infinity List" source, processes and cleans the data, adds additional
#' columns (e.g., establishment status), and optionally saves the raw and processed data.
#'
#' @param taxa A character vector or string specifying the taxa (e.g., species, genus) 
#'             for which observations are to be downloaded.
#' @param year_range A numeric vector of length 2 indicating the start and end years 
#'                   for data retrieval. Default is from 1923 to 2023.
#' @param save_raw_data A logical value indicating whether to save the raw data. 
#'                      By default, raw data is not saved (`FALSE`).
#' @param output_dir A character string specifying the directory where any saved data 
#'                   (raw or processed) will be stored. Default is `system.file(package = "infinitylists", "data/")`.
#'
#' @details The function carries out the following steps:
#' 1. Retrieve the data from the "Infinity List" source.
#' 2. Process and clean the retrieved data to remove any inconsistencies.
#' 3. Add additional columns to the cleaned data, such as the establishment status 
#'    for the given taxa.
#' 4. If `save_raw_data` is `TRUE`, save the processed data to the specified `output_dir`.
#'
#' @return This function saves the processed data and returns invisibly. The structure and 
#'         content of the returned value (if any) is determined by the functions called 
#'         within (e.g., `retrieve_data`, `process_data`).
#'
#' @export
#'
download_ala_obs <- function(taxa,
                             year_range = c(1923, 2023),
                             save_raw_data = FALSE,
                             output_dir = system.file(package = "infinitylists", "data/")) {
  # 1. Data retrieval
  ala_obs <-
    retrieve_data(taxa, year_range, save_raw_data, output_dir)
  
  # 2. Filtering and processing
  ala_cleaned <- process_data(ala_obs)
  
  # 3. Add additional columns
  ala_cleaned_with_add_ons <-
    get_establishment_status(ala_cleaned, taxa)
  
  # 4. Save processed data
  save_data(ala_cleaned_with_add_ons, taxa, output_dir)
  
  
}


#' Default ALA query
#'
#' @noRd
query <- function(taxa, years) {
  galah::galah_call() |>
    galah::galah_identify(taxa) |>
    galah::galah_filter(
      spatiallyValid == TRUE,
      species != "",
      decimalLatitude != "",
      year == years,
      basisOfRecord == c("HUMAN_OBSERVATION", "PRESERVED_SPECIMEN")
    ) |>
    galah::galah_select(
      recordID,
      species,
      genus,
      family,
      decimalLatitude,
      decimalLongitude,
      coordinateUncertaintyInMeters,
      eventDate,
      datasetName,
      basisOfRecord,
      references,
      institutionCode,
      recordedBy,
      outlierLayerCount,
      isDuplicateOf,
      sounds
    )
}


#' @noRd
retrieve_data_by_years <- function(taxa,
                                   years,
                                   save_raw_data = NULL,
                                   output_dir = NULL) {
  # Download data from ALA
  download <- query(taxa, years) |>
    galah::atlas_occurrences()
  
  # Save download (optional)
  if (save_raw_data)
    arrow::write_parquet(
      download,
      paste0(
        output_dir,
        "ALA-Australia-",
        taxa,
        "-",
        first(years),
        "-",
        last(years),
        "-",
        Sys.Date(),
        ".parquet"
      )
    )
  
  return(download)
}


#' @noRd
retrieve_data <- function(taxa,
                          year_range = c(1923, 2023),
                          save_raw_data = FALSE,
                          output_dir = NULL) {
  # Split years into chunks of 10 year intervals
  years <- seq(year_range[1], year_range[2])
  
  # Determine atlas counts
  n_obs <- query(taxa, years) |>
    galah::atlas_counts()
  
  # If less than 1 mil records
  if (n_obs < 1000000)
    output <-
    retrieve_data_by_years(taxa, years, save_raw_data, output_dir)
  else{
    year_chunks <- split(years, ceiling(seq_along(years) / 9))
    
    download <-  purrr::map(year_chunks,
                            purrr::possibly(
                              ~ retrieve_data_by_years(taxa, years = .x, save_raw_data, output_dir)
                            ))
    
    # Collapse list
    output <- dplyr::bind_rows(download)
  }
  
  return(output)
  
}

#' @noRd
get_establishment_status <- function(ala_cleaned, taxa = taxa) {
  if (taxa == "Plantae") {
    resources <- APCalign::load_taxonomic_resources()
    lookup <-
      APCalign::native_anywhere_in_australia(ala_cleaned$Species, resources = resources)
    
    lookup <- dplyr::rename(lookup, Species = species)
    
    ala_cleaned <-
      ala_cleaned |> dplyr::left_join(lookup, by = join_by("Species"))
    
    return(ala_cleaned)
  }
  if (taxa %in% c("Cicadoidea", "Marsupialia", "Odonata", "Papilionoidea")) {
    ala_cleaned$native_anywhere_in_aus <- "native"
    ala_cleaned$native_anywhere_in_aus[ala_cleaned$Species %in% c("Danaus plexippus", "Pieris rapae")] <-
      "introduced"
    return(ala_cleaned)
  }
  if (!taxa %in% c("Cicadoidea",
                   "Marsupialia",
                   "Odonata",
                   "Papilionoidea",
                   "Plantae")) {
    ala_cleaned$native_anywhere_in_aus <- "unknown"
  }
  return(ala_cleaned)
}


#' @noRd
process_data <- function(data) {
  datasets_of_interest <- c(
    "Australia's Virtual Herbarium",
    "iNaturalist observations",
    "iNaturalist research-grade observations"
  )
  
  data |>
    dplyr::filter(
      basisOfRecord == "PRESERVED_SPECIMEN" |
        datasetName %in% datasets_of_interest,
      is.na(coordinateUncertaintyInMeters) |
        coordinateUncertaintyInMeters <= 1000,!is.na(eventDate),!str_detect(species, "spec.$")
    ) |>
    dplyr::mutate(
      voucher_location = dplyr::if_else(!is.na(references), references, institutionCode),
      voucher_type = dplyr::case_when(
        basisOfRecord == "PRESERVED_SPECIMEN" ~ "Collection",!is.na(sounds) ~ "Audio",
        TRUE ~ "Photograph"
      ),
      lat = decimalLatitude,
      long = decimalLongitude,
      collectionDate = lubridate::ymd_hms(eventDate, tz = "UTC", quiet = TRUE),
      collectionDate = dplyr::if_else(
        is.na(collectionDate),
        lubridate::ymd(eventDate, tz = "UTC", quiet = TRUE),
        collectionDate
      )
    ) |>
    dplyr::select(
      species:family,
      collectionDate,
      lat,
      long,
      voucher_type,
      voucher_location,
      recordedBy,
      recordID
    ) |>
    janitor::clean_names("title")
}


#' @noRd
save_data <- function(data, taxa, output_dir) {
  arrow::write_parquet(data,
                       paste0(output_dir, "Australia-", taxa, "-", Sys.Date(), ".parquet"))
}

# galah_config(email = Sys.getenv("ALA_EMAIL"),
#              atlas = "Australia")
#
# requireNamespace("job", quietly = TRUE)
#job::job(packages = c("purrr", "dplyr", "arrow", "janitor", "galah", "stringr", "lubridate"), {
#   download_ala_obs(taxa = "Papilionoidea")
#   download_ala_obs(taxa = "Odonata")
#   download_ala_obs(taxa = "Marsupialia")
#   download_ala_obs(taxa = "Cicadoidea")
#   download_ala_obs(taxa = "Plantae")
# })
