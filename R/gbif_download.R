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
  # galah's field list doesn't include hasGeospatialIssue (see
  # query_gbif_global()), so skip its field check, then restore the setting.
  run_checks <- galah::galah_config()$package$run_checks
  galah::galah_config(run_checks = FALSE)
  on.exit(galah::galah_config(run_checks = run_checks), add = TRUE)

  # 1 + 2. Data retrieval, filtering and processing (chunk by chunk)
  gbif_cleaned <-
    retrieve_gbif_data(taxon, min_year, max_year, country_code, save_raw_data, output_dir,
                       process = function(d) suppressWarnings(gbif_process_data(d)))

  # 3. Save processed data
  save_gbif_data(gbif_cleaned, taxon, country_code, output_dir)
}


#' Generate galah query for Global GBIF Node
#'
#' @param taxon character, genus/family/kingdom
#' @param min_year numeric, year cut off for query, only records where year >= min_year will be included 
#' @param max_year numeric, year cut off for query, only records where year <= max_year will be included 
#' @param country_code character, code for country
#' @export

query_gbif_global <- function(taxon,
                              min_year,
                              max_year,
                              country_code = NULL){
  # GBIF's key is HAS_GEOSPATIAL_ISSUE, but galah >= 2.2 lists the field as
  # hasGeospatialIssues (which GBIF rejects), so its field check refuses
  # hasGeospatialIssue. download_gbif_obs() turns that check off while it
  # runs; do the same (galah_config(run_checks = FALSE)) to use this alone.
  # Keep each query as one pipe from galah_call(): galah_filter() doesn't
  # recognise a request stored in a variable and returns bare filters.
  if (is.null(country_code)) {
    query <- galah::galah_call() |>
      galah::galah_identify(taxon) |>
      galah::galah_filter(
        hasGeospatialIssue == "false",
        year >= min_year,
        year <= max_year)
  } else {
    query <- galah::galah_call() |>
      galah::galah_identify(taxon) |>
      galah::galah_filter(
        country == country_code,
        hasGeospatialIssue == "false",
        year >= min_year,
        year <= max_year)
  }
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


#' Retrieve and process GBIF records
#'
#' Downloads in chunks of consecutive years of at most `max_records` records
#' (by record count, as for ALA), retrying each chunk and failing loudly
#' rather than silently dropping one. Each chunk is processed as soon as it
#' downloads, so only the kept rows stay in memory.
#'
#' @param process function applied to each downloaded chunk
#' @param max_records largest number of records requested from GBIF at once
#' @param min_fraction a chunk with fewer than this fraction of the records
#'   GBIF counted for it is treated as a failed download
#' @noRd
#' @keywords internal

retrieve_gbif_data <- function(taxon, min_year, max_year,
                               country_code = NULL,
                               save_raw_data = FALSE,
                               output_dir = file.path(system.file(package = "infinitylists"), "data/"),
                               process = identity,
                               max_records = 1000000,
                               min_fraction = 0.95){

  n_obs <- query_gbif_global(taxon, min_year, max_year, country_code) |>
    galah::atlas_counts() |>
    dplyr::pull(count)

  # Split by halving year ranges using total counts only. (galah returns at
  # most 10 years from a grouped GBIF count, so chunk_years() would miss
  # years here.)
  count_years <- function(from, to) {
    query_gbif_global(taxon, from, to, country_code) |>
      galah::atlas_counts() |>
      dplyr::pull(count)
  }
  year_chunks <- merge_year_ranges(
    split_year_range(min_year, max_year, n_obs, count_years, max_records),
    max_records)

  output <- purrr::imap(year_chunks, function(chunk, i) {
    what <- sprintf("%s-%s %d-%d (chunk %d of %d)", taxon, country_code,
                    chunk$from, chunk$to, i, length(year_chunks))
    message("Downloading ", what)
    download <- with_retries(function() {
      d <- retrieve_gbif_data_by_year_range(taxon, chunk$from, chunk$to,
                                            country_code, save_raw_data,
                                            output_dir)
      # A truncated download would otherwise go unnoticed.
      if (nrow(d) < min_fraction * chunk$count) {
        stop(sprintf("got %s records, expected about %s",
                     format(nrow(d), big.mark = ","),
                     format(chunk$count, big.mark = ",")), call. = FALSE)
      }
      d
    }, what)
    processed <- process(download)
    message(sprintf("  %s raw -> %s kept",
                    format(nrow(download), big.mark = ","),
                    format(nrow(processed), big.mark = ",")))
    rm(download)
    gc()
    processed
  })

  dplyr::bind_rows(output)
}

#' Split `from:to` into consecutive year ranges of at most `max_records`
#' records, halving ranges that are too big (a single year stays whole)
#'
#' @param n number of records in `from:to`
#' @param count_years function(from, to) returning the record count
#' @return list of `list(from, to, count)`
#' @noRd
#' @keywords internal
split_year_range <- function(from, to, n, count_years, max_records) {
  if (n <= max_records || from == to) {
    return(list(list(from = from, to = to, count = n)))
  }
  mid <- (from + to) %/% 2
  n_low <- count_years(from, mid)
  c(split_year_range(from, mid, n_low, count_years, max_records),
    split_year_range(mid + 1, to, n - n_low, count_years, max_records))
}

#' Join neighbouring ranges from split_year_range() while they stay within
#' `max_records`
#' @noRd
#' @keywords internal
merge_year_ranges <- function(chunks, max_records) {
  out <- list()
  for (x in chunks) {
    last <- length(out)
    if (last > 0 && out[[last]]$count + x$count <= max_records) {
      out[[last]]$to <- x$to
      out[[last]]$count <- out[[last]]$count + x$count
    } else {
      out[[last + 1]] <- x
    }
  }
  out
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
      # Always a string: when every value is NA it would otherwise be
      # written as bool, and the type would change from one refresh to the next.
      establishmentMeans = as.character(establishmentMeans)
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
