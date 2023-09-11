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

# Create dates vector
years <- seq(1923, 2023)

# Split by 20
year_chunks <- split(years, ceiling(seq_along(years)/10))

# Tack on 2023 to 10th chunk 
year_chunks$`10` <- c(year_chunks$`10`, year_chunks$`11`)

# Remove chunk 11
year_chunks_adjusted <- year_chunks[1:10]

# Write a function to download what we want from galah
# We want dups and "outliers"
retrieve_save_data <- function(taxa = "Plantae", years, output_dir = "ignore/") {
  # Download
  download <- galah_call() |> 
    galah_identify(taxa) |>
    galah_filter(
      spatiallyValid == TRUE, 
      species != "",
      decimalLatitude != "",
      year == c(years),
      basisOfRecord == c("HUMAN_OBSERVATION", "PRESERVED_SPECIMEN")
    ) |>
    galah_select(
      recordID, species, genus, family, decimalLatitude, decimalLongitude, 
      coordinateUncertaintyInMeters, eventDate, datasetName, basisOfRecord, 
      references, institutionCode, recordedBy, outlierLayerCount, isDuplicateOf
    ) |>
    atlas_occurrences()
  
  # Process data
  processed <- process_data(download)
  
  # Save download
  write_parquet(
    processed,
    paste0(output_dir, "Australia-", taxa, "-", first(years), 
           "-", last(years), "-",  Sys.Date(), ".parquet")
    )
}

# Try this out
test <- retrieve_save_data("Banksia serrata", years = c(2022, 2023))



# Check - that seemed to work
test |> 
  group_by(year) |> 
  count()

test |> 
  mutate(year = year(eventDate)) |> 
  group_by(year) |> 
  count()

# Check outlierLayerCount
test |> 
  mutate(year = year(eventDate)) |> 
  group_by(year) |> 
  count()

# Check outlierLayerCount
test |> 
  mutate(year = year(eventDate)) |> 
  group_by(outlierLayerCount) |> 
  count()

# Check outlierLayerCount
test |> 
  select(species, eventDate, recordID, isDuplicateOf) |> 
  filter(! is.na(isDuplicateOf)) 


# Test map over years with our function
test_years_chunk <- list(`1` = c(2022, 2023), 
                         `2` = c(1991, 1992))
test_map_output <- map(test_years_chunk,
       ~retrieve_save_data("Banksia serrata", years = .x)) # Beautiful


# Set up RStudio job for large download
job::job({
map(year_chunks_adjusted,
    ~retrieve_save_data("Plantae", years = .x)) # Beautiful
})

# Open all the files
x <- open_dataset(sources = "ignore/") 
x |> write_parquet(sink = paste0("infinity-app/data/Australia-Plantae-", Sys.Date(), ".parquet")) # That works! 