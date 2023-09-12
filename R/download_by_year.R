# Breaking up a large download of Australian Plantae by 5 year intervals

# Load libraries
library(galah)
library(dplyr)
library(purrr)
library(arrow)

# Configure galah
galah_config(email = Sys.getenv("ALA_EMAIL"),
             atlas = "Australia")

# Retrieve data by year intervals
retrieve_data_by_years <- function(taxa, 
                                   years, 
                                   save_raw_data, 
                                   output_dir) {
  
  # Download data from ALA
  download <- galah_call() |> 
    galah_identify(taxa) |> galah_filter(
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

# Try this out
# test <- retrieve_data_by_years(taxa = "Banksia serrata", years = c(1923, 1924), save_raw_data = FALSE)
# test <- retrieve_data_by_years("Banksia serrata", years = c(1923, 1924), save_raw_data = TRUE, output_dir = "ignore/")

download_data <- function(taxa, 
                          year_range = c(1923, 2023),
                          background_job = TRUE,
                          save_raw_data = FALSE, 
                          output_dir = "ignore/"){

  # Split years into chunks of 10 year intervals
  years <- seq(year_range[1], year_range[2])
  
  year_chunks <- split(years, ceiling(seq_along(years)/10))
  
  # Retrieve data download by years - I wanted to take this out but can't for some reason
  # Gives me error cannot find retrieve_data_by_years
  retrieve_data_by_years <- function(taxa, 
                                     years, 
                                     save_raw_data, 
                                     output_dir) {
    
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
  

  
  # Run data retrieval as a background job in RStudio (optional by recommended)
  if(background_job)
    job::job(packages = c("purrr", "dplyr", "galah", "arrow"),{
      download <- purrr::map(year_chunks,
                    purrr::possibly(~retrieve_data_by_years(taxa, years = .x, save_raw_data, output_dir))
      )
      
      job::export(value = "new")
      }) else
      download <-  purrr::map(year_chunks,
                              purrr::possibly(~retrieve_data_by_years(taxa, years = .x, save_raw_data, output_dir))
      )
  
  # Collapse list
  output <- bind_rows(download)
  
  return(output)
  
  }

