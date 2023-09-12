# Breaking up a large download of Australian Plantae by 5 year intervals

# Load libraries
library(galah)
library(dplyr)
library(lubridate)
library(janitor)
library(purrr)
library(arrow)

source("R/galah_download.R") #For process_data

# Configure galah
galah_config(email = Sys.getenv("ALA_EMAIL"),
             atlas = "Australia")

# Write a function to download what we want from galah
# We want dups and "outliers"
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

source("R/retrieve_data_by_years.R")
download_data <- function(taxa, 
                          years = seq(1923, 2023),
                          background_job = TRUE,
                          save_raw_data = FALSE, 
                          output_dir = "ignore/"){
  
  # Process years into chunks
  # Split years into chunks of 10 year intervals
  # TODO: Generalise this a bit more, what happens in with 1923 - 2025 for example?
  # Will need to somehow test length of last chunk and if it is less than 3 then tack on
  # If not then just leave it
  # Probably can generalise the intervals too and determine a multiple so that seq(1923, 2023) 
  # is split nicely but for now this is fine!
  year_chunks <- split(years, ceiling(seq_along(years)/10))
  
  # Tack on 2023 to 10th chunk 
  year_chunks$`10` <- c(year_chunks$`10`, year_chunks$`11`)
  
  # Remove chunk 11
  year_chunks_adjusted <- year_chunks[1:10]
  
  # Retrieve data download by years - I wanted to take this out but can't for some reason
  # Gives me error cannot find retrieve_data_by_years
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
  
  # Run data retrieval as a background job in RStudio (optional by recommended)
  if(background_job)
    job::job({
      output <- purrr::map(year_chunks_adjusted,
                    ~retrieve_data_by_years(taxa, years = .x, save_raw_data, output_dir))
    }) else
      output <-  purrr::map(year_chunks_adjusted,
                     ~retrieve_data_by_years(taxa, years = .x, save_raw_data, output_dir)
      )
  
  return(output)
  
  }

# Test download_data
test_download_data <- download_data(taxa = "Banksia serrata",
                                    years = c(1991, 1992),
                                    background_job = FALSE,
                                    save_raw_data = TRUE)


# Open all the files and save as a giant parquet
x <- open_dataset(sources = "ignore/") 
x |> write_parquet(sink = paste0("infinity-app/data/Australia-Plantae-", Sys.Date(), ".parquet")) 