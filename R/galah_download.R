#' Download Observations for Infinity List
#'
#' This function downloads observation data for the given taxon and year range
#' from the "Infinity List" source, processes and cleans the data, adds additional
#' columns (e.g., establishment status), and optionally saves the raw and processed data.
#'
#' @param taxon A character vector or string specifying the taxon (e.g., species, genus)
#'             for which observations are to be downloaded.
#' @param year_range A numeric vector of length 2 indicating the start and end years
#'                   for data retrieval. Default is from 1923 to 2023.
#' @param save_raw_data A logical value indicating whether to save the raw data.
#'                      By default, raw data is not saved (`FALSE`).
#' @param output_dir A character string specifying the directory where any saved data
#'                   (raw or processed) will be stored. Default is `file.path(system.file(package = "infinitylists"), "data/")`.
#'
#' @details The function carries out the following steps:
#' 1. Retrieve the data from the "Infinity List" source.
#' 2. Process and clean the retrieved data to remove any inconsistencies.
#' 3. Add additional columns to the cleaned data, such as the establishment status
#'    for the given taxon.
#' 4. If `save_raw_data` is `TRUE`, save the processed data to the specified `output_dir`.
#'
#' @return This function saves the processed data and returns invisibly. The structure and
#'         content of the returned value (if any) is determined by the functions called
#'         within (e.g., `retrieve_data`, `process_data`).
#'
#' @export
#'
download_ala_obs <- function(taxon,
                             year_range = c(1923, 2023),
                             save_raw_data = FALSE,
                             output_dir = file.path(system.file(package = "infinitylists"), "data/")) {
  # 1. Data retrieval
  ala_obs <-
    retrieve_data(taxon, year_range, save_raw_data, output_dir)
  
  # 2. Filtering and processing
  ala_cleaned <- process_data(ala_obs)
  
  # 3. Add additional columns
  ala_cleaned_with_add_ons <-
    get_establishment_status(ala_cleaned, taxon)
  
  # 4. Save processed data
  save_data(ala_cleaned_with_add_ons, taxon, output_dir)
  
  
}


#' ALA Query Function
#'
#' This function fetches data from the ALA (Atlas of Living Australia) based on
#' the provided taxon and year criteria.
#'
#' @param taxon A character string representing the name of the taxon to query.
#' @param years A numeric vector specifying the year(s) for the data.
#'
#' @return A list containing the identify, filter, and select criteria to be
#'         used with the ALA API.
#'
#' @noRd

query <- function(taxon, years) {
  identify <- galah::galah_call() |>
    galah::galah_identify(taxon)
  
  filter <- galah::galah_filter(
    spatiallyValid == TRUE,
    species != "",
    decimalLatitude != "",
    year == years,
    basisOfRecord == c("HUMAN_OBSERVATION", "PRESERVED_SPECIMEN")
  )
  
  select <- galah::galah_select(
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
  
  identify$filter <- filter
  identify$select <- select
  
  return(identify)
}


#' @noRd
retrieve_data_by_years <- function(taxon,
                                   years,
                                   save_raw_data = NULL,
                                   output_dir = NULL) {
  # Download data from ALA
  download <- query(taxon, years) |>
    galah::atlas_occurrences()
  
  # Save download (optional)
  if (save_raw_data)
    arrow::write_parquet(
      download,
      paste0(
        output_dir,
        "ALA-Australia-",
        taxon,
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
retrieve_data <- function(taxon,
                          year_range = c(1923, 2023),
                          save_raw_data = FALSE,
                          output_dir = NULL) {
  # Split years into chunks of 10 year intervals
  years <- seq(year_range[1], year_range[2])
  
  # Determine atlas counts
  n_obs <- query(taxon, years) |>
    galah::atlas_counts()
  
  # If less than 1 mil records
  if (n_obs < 1000000)
    output <-
    retrieve_data_by_years(taxon, years, save_raw_data, output_dir)
  else{
    year_chunks <- split(years, ceiling(seq_along(years) / 9))
    
    download <-  purrr::map(year_chunks,
                            purrr::possibly(
                              ~ retrieve_data_by_years(taxon, years = .x, save_raw_data, output_dir)
                            ))
    
    # Collapse list
    output <- dplyr::bind_rows(download)
  }
  
  return(output)
  
}

#' @noRd
get_establishment_status <- function(ala_cleaned, taxon = taxon) {
  if (taxon == "Plantae") {
    resources <- APCalign::load_taxonomic_resources()
    suppressWarnings(
    lookup <-
      APCalign::native_anywhere_in_australia(unique(ala_cleaned$Species), resources = resources)
    )
    
    lookup <- dplyr::rename(lookup, Species = species)
    
    ala_cleaned <-
      ala_cleaned |> dplyr::left_join(lookup, by = dplyr::join_by("Species"))
  }
  if (taxon %in% c("Cicadoidea", "Marsupialia", "Odonata", "Papilionoidea")) {
    ala_cleaned$native_anywhere_in_aus <- "native"
    ala_cleaned$native_anywhere_in_aus[ala_cleaned$Species %in% c("Danaus plexippus", "Pieris rapae")] <-
      "introduced"
  }
  if (!taxon %in% c("Cicadoidea",
                    "Marsupialia",
                    "Odonata",
                    "Papilionoidea",
                    "Plantae")) {
    ala_cleaned$native_anywhere_in_aus <- "unknown"
  }
  # Rename native_anywhere_in_aus
  ala_cleaned <- dplyr::rename(ala_cleaned,
                `Establishment means` = native_anywhere_in_aus)
  
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
        coordinateUncertaintyInMeters <= 1000,
      !is.na(eventDate),
      !stringr::str_detect(species, "spec.$")
    ) |>
    dplyr::mutate(
      repository = dplyr::if_else(!is.na(references), references, institutionCode),
      voucher_type = dplyr::case_when(
        basisOfRecord == "PRESERVED_SPECIMEN" ~ "Collection",
        !is.na(sounds) ~ "Audio",
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
      repository,
      recordedBy,
      recordID
    ) |>
    dplyr::mutate(link = dplyr::case_when(grepl("https", repository) ~ repository,
                            TRUE ~ paste0("https://biocache.ala.org.au/occurrences/", recordID))
    ) |> 
    janitor::clean_names("title")
}


#' @noRd
save_data <- function(data, taxon, output_dir) {
  if (!file.exists(file.path(output_dir))) {
    dir.create(file.path(output_dir), recursive = TRUE)
  }
  
  
  arrow::write_parquet(x = data,
                       sink = file.path(
                         output_dir,
                         paste0("Australia-",
                                taxon,
                                "-",
                                Sys.Date(),
                                ".parquet")
                       ))
}

#  galah::galah_config(email = "wcornwell@gmail.com")
# 
#  requireNamespace("job", quietly = TRUE)
#  job::job(packages = c("purrr", "dplyr", "arrow", "janitor", "galah", "stringr", "lubridate", "infinitylists"), {
#     download_ala_obs(taxon = "Papilionoidea")
#     download_ala_obs(taxon = "Odonata")
#     download_ala_obs(taxon = "Marsupialia")
#     download_ala_obs(taxon = "Cicadoidea")
#     download_ala_obs(taxon = "Plantae")
#   })
