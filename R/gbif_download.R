#' Generate galah query for Global GBIF Node
#'
#' @param taxon character, genus/family/kingdom
#' @param min_year numeric, year cut off for query, only records where year >= min_year will be included 
#' @param country_code character, code for country

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
        "GBIF-",
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


#' Title
#'
#' @param taxon 
#' @param min_year 
#' @param max_year 
#' @param country_code 
#' @param save_raw_data 
#' @param output_dir 
#'
#' @return
#' @export
#'
#' @examples

retrieve_gbif_data <- function(taxon, min_year, max_year,
                               country_code = NULL,
                               save_raw_data = FALSE,
                               # output_dir = file.path(system.file(package = "infinitylists"), "data/")
                               output_dir = "./"
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
      !stringr::str_count(eventDate) > 16
    ) |> # Exclude strings with greater than 16 characters - a few records had date ranges e.g. 2017-12-10T00:00Z/2017-12-23T00:00Z
    dplyr::mutate(
      eventDate_as_date = lubridate::as_date(eventDate), # Convert to dates
      eventDate_ymd = lubridate::ymd_hm(eventDate, tz = "UTC", quiet = TRUE), # Convert dates that have time zones
      collectionDate = dplyr::coalesce(eventDate_as_date, eventDate_ymd) # Put the two date columns together as one complete one. 
    ) |> 
    dplyr::mutate(
      repository = dplyr::case_when(grepl("inatur", occurrenceID) ~ occurrenceID, # Create Repository column, if occurrence ID contains "inatur", keep occurrenceID
                                    TRUE ~ institutionCode),  # Otherwise take institutionCode
      link = dplyr::case_when(grepl("https", repository) ~ repository, # Create link
                              TRUE ~ paste0("https://www.gbif.org/dataset/", datasetKey)
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


