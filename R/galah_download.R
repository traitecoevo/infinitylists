#' Download observations for Infinity List
#'
#' @param taxa
#' @param year_range
#' @param save_raw_data
#' @param output_dir
#' @export
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
#' @param taxa
#' @param years
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
