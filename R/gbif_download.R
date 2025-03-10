#' Download and Process GBIF Observations
#'
#' This function retrieves, processes, and saves GBIF (Global Biodiversity Information Facility) observation data for a specified taxon.
#'
#' @param taxon Character. The taxon (species, genus, etc.) for which to retrieve GBIF data.
#' @param min_year Numeric. The minimum year for the observations to be retrieved. Default is 1923.
#' @param max_year Numeric. The maximum year for the observations to be retrieved. Default is the current year.
#' @param country_code Character. The ISO 3166-1 alpha-2 country code to filter observations by country. Default is NULL (no country filter).
#' @param save_raw_data Logical. Whether to save the raw data retrieved from GBIF. Default is FALSE.
#' @param output_dir Character. The directory where the processed data will be saved. Default is a "data" directory within the "infinitylists" package.
#'
#' @return None. The function saves the processed data to the specified output directory.
#' @export

download_gbif_obs <- function(taxon,
                              min_year = 1923,
                              max_year = as.numeric(format(Sys.Date(), "%Y")),
                              country_code= NULL,
                              save_raw_data = FALSE,
                              output_dir = file.path(system.file(package = "infinitylists"), "data/")){
  # 1. Data retrieval
  gbif_obs <-
    retrieve_gbif_data(taxon, min_year, max_year, country_code, save_raw_data, output_dir)
  
  # 2. Filtering and processing
  gbif_cleaned <- suppressWarnings(gbif_process_data(gbif_obs))
  
  # 4. Save processed data
  save_gbif_data(gbif_cleaned, taxon, country_code, output_dir)
}


#' Generate galah query for Global GBIF Node
#'
#' @param taxon character, genus/family/kingdom
#' @param min_year numeric, year cut off for query, only records where year >= min_year will be included 
#' @param max_year numeric, year cut off for query, only records where year <= max_year will be included 
#' @param country_code character, code for country
#' @export

query_gbif_global<- function(taxon, 
                             min_year,
                             max_year,
                             country_code = NULL){
  if(is.null(country_code))
    query  <- galah::galah_call() |>
      galah::galah_identify(taxon) |> 
      galah::galah_filter(
        hasGeospatialIssue == "false",
        year >= min_year,
        year <= max_year)
  else(
    query <- galah::galah_call() |>
      galah::galah_identify(taxon) |> 
      galah::galah_filter(
        country == country_code,
        hasGeospatialIssue == "false",
        year >= min_year,
        year <= max_year)
  )
  return(query)
}

#' Download GBIF data and save as output
#' @noRd
#' @keywords internal
retrieve_gbif_data_by_year_range <- function(taxon, min_year, max_year, country_code= NULL, 
                                             save_raw_data = FALSE, output_dir){
  
  download <- query_gbif_global(taxon, min_year, max_year, country_code)  |> 
    galah::atlas_occurrences()
  
  # Save download (optional)
  if (save_raw_data)
    arrow::write_parquet(
      download,
      paste0(
        output_dir,
        "GBIF-preprocessed-",
        taxon,
        "-",
        min_year,
        "-",
        max_year,
        "-",
        Sys.Date(),
        ".parquet"
      )
    )
  
  return(format_date_as_character(download))
}

#' @noRd
#' @keywords internal
format_date_as_character <- function(data){
  data |> 
    dplyr::mutate(eventDate = as.character(eventDate))
}


#' Retrieve GBIF records 
#'
#' @param taxon 
#' @param min_year 
#' @param max_year 
#' @param country_code 
#' @param save_raw_data 
#' @param output_dir 
#' @noRd
#' @keywords internal

retrieve_gbif_data <- function(taxon, min_year, max_year,
                               country_code = NULL,
                               save_raw_data = FALSE,
                               output_dir = file.path(system.file(package = "infinitylists"), "data/")
                               ){
  
  n_obs <- query_gbif_global(taxon, min_year, max_year, country_code) |> 
    galah::atlas_counts() |> 
    dplyr::pull(count)
  
  # If less than 1 mil records
  if (n_obs < 1000000){
    download <- retrieve_gbif_data_by_year_range(taxon, 
                                     min_year, max_year,
                                     country_code,
                                     save_raw_data,
                                     # output_dir = file.path(system.file(package = "infinitylists"), "data/")
                                     output_dir)
  } else {
    
    years <- seq(min_year, max_year)
    length(years)
    
    # Split years
    year_chunks <- split(years, ceiling(seq_along(years) / 10))
  
    # Map this
    download <-  purrr::map(year_chunks,
                            purrr::possibly(
                              ~ retrieve_gbif_data_by_year_range(taxon,
                                                                 min_year = range(.x)[1], max_year = range(.x)[2],
                                                                 country_code,
                                                                 save_raw_data,
                                                                 # output_dir = file.path(system.file(package = "infinitylists"), "data/")
                                                                 output_dir))
                            ) |> 
      purrr::list_rbind()
                            
  }
 return(download)
}

#' Process downloaded data from Atlas of Living Australia 
#' @noRd
#' @keywords internal

gbif_process_data <- function(data){
  data |> 
    tidyr::drop_na(decimalLatitude) |> 
    dplyr::filter(
      basisOfRecord == "PRESERVED_SPECIMEN" |
        stringr::str_detect(institutionCode, stringr::regex("inaturalist", ignore_case = TRUE)),
      is.na(coordinateUncertaintyInMeters) |
        coordinateUncertaintyInMeters <= 1000,
      !is.na(eventDate),
      !stringr::str_detect(species, "spec.$"),
      !stringr::str_count(eventDate) <= 7, # Exclude strings with 7 or fewer characters, these are years or year + month e.g 2006-06 or just 2006
      !stringr::str_count(eventDate) > 20) |> # Exclude strings with greater than 20 characters - a few records had date ranges e.g. 2017-12-10T00:00Z/2017-12-23T00:00Z
    dplyr::mutate(
      eventDate_as_date = lubridate::as_date(eventDate), # Convert to dates
      eventDate_ymd = lubridate::ymd_hm(eventDate, tz = "UTC", quiet = TRUE), # Convert dates that have time zones
      collectionDate = dplyr::coalesce(eventDate_as_date, eventDate_ymd) # Put the two date columns together as one complete one. 
    ) |> 
    dplyr::mutate(
      repository = dplyr::case_when(grepl("inatur", occurrenceID) ~ occurrenceID, # Create Repository column, if occurrence ID contains "inatur", keep occurrenceID
                                    TRUE ~ institutionCode),  # Otherwise take institutionCode
      link = dplyr::case_when(grepl("https", repository) ~ repository, # Create link
                              TRUE ~ paste0("https://www.gbif.org/occurrence/", gbifID)
      ),
      sounds = dplyr::case_when( # Logical variable to determine if there voucher_type
        grepl("Sound", mediaType) ~ 1, 
        TRUE ~ 0
      ),
      voucher_type = dplyr::case_when(
        basisOfRecord == "PRESERVED_SPECIMEN" ~ "Collection",
        sounds == 1 ~ "Audio",
        TRUE ~ "Photograph"
      ),
      lat = decimalLatitude,
      long = decimalLongitude,
    ) |> 
    dplyr::mutate(lat=as.numeric(lat),long=as.numeric(long))|>
    dplyr::select(
      species, genus, family,
      collectionDate,
      lat,
      long,
      voucher_type,
      repository,
      recordedBy, 
      establishmentMeans,
      link
    ) |> 
    janitor::clean_names("title")
}

save_gbif_data <- function(data, taxon, country_code = NULL, output_dir) {
  if (!file.exists(file.path(output_dir))) {
    dir.create(file.path(output_dir), recursive = TRUE)
  }
  
  
  arrow::write_parquet(x = data,
                       sink = file.path(
                         output_dir,
                         paste0("Living-Atlas-",
                                taxon,
                                "-",
                                country_code,
                                "-",
                                Sys.Date(),
                                ".parquet")
                       ))
}
