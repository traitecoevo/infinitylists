library(galah)
library(tidyverse)
library(janitor)
library(arrow)
library(here)
library(APCalign)
library(skimr)

# Configure galah
#galah_config(email = Sys.getenv("ALA_EMAIL"),
#             atlas = "Australia")

taxa<-"Lepidoptera"

# Formulate query and download
NSW_plants <- 
  galah_call() |> 
  galah_identify(taxa) |> 
  galah_apply_profile("CSDM") |> 
  galah_filter(stateProvince == "New South Wales", 
               species != "",
               decimalLatitude != "",
               year >= 1923,
               basisOfRecord == c("HUMAN_OBSERVATION", "PRESERVED_SPECIMEN")) |> 
  galah_select(recordID, species, genus,family,
               decimalLatitude,decimalLongitude,coordinateUncertaintyInMeters,
               eventDate,datasetName,basisOfRecord,
               references,institutionCode,recordedBy) |> 
   atlas_occurrences() 
   #atlas_counts()

# Save massive download before processing
# TODO: This could be an optional step for reproducibility reasons
# write_parquet(NSW_plants, paste0("ala_data/NSW_plants_", Sys.Date(), ".parquet"))

## Processing 
# Not read into memory
# NSW_plants <- open_dataset(sources = here("ala_data/NSW_plants_2023-08-21"), format = "parquet") 

### Narrowing dataset to what we want 
datasets_of_interest <- c("Australia's Virtual Herbarium","iNaturalist observations","iNaturalist research-grade observations")

NSW_plants_targetrecords  <- NSW_plants |>
  filter(basisOfRecord == "PRESERVED_SPECIMEN" | datasetName %in% datasets_of_interest,
         is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters <=1000,
         !is.na(eventDate)) |> 
  filter(!str_detect(species,"spec.$")) # Exclude genus level taxon "spec." 

# Create new voucher variables
NSW_plants_vouchervars <- NSW_plants_targetrecords |> 
  mutate(voucher_location = case_when(!is.na(references) ~ references,
                                      TRUE ~ institutionCode),
         voucher_type = case_when(basisOfRecord=="PRESERVED_SPECIMEN" ~ "Collection",
                                  TRUE ~ "Photograph") 
  )

# Rename and clean names variables
NSW_plants_renamed <- NSW_plants_vouchervars |> 
  rename(lat = decimalLatitude,
         long = decimalLongitude) 



# Reformat date
NSW_plants_tzdate <- NSW_plants_renamed |> 
  mutate(
    collectionDate = ymd_hms(eventDate, tz = "UTC", quiet = TRUE),
    collectionDate = if_else(is.na(collectionDate), ymd(eventDate, tz = "UTC", quiet = TRUE), collectionDate)
  )

# Create native variable
# lu <- NSW_plants_tzdate |> 
#   mutate(num_words_species = stringi::stri_count_words(species)) |>  # Count the number of words in species
#   filter(num_words_species == 2) |> # Include the binomials only
#   pull(species) |> 
#   unique() |> # Filter the unique species
#   native_anywhere_in_australia() # Determine if species is native in Aus or not
# 
# NSW_plants_native <- left_join(NSW_plants_tzdate,lu) |> 
#   mutate(native_anywhere_in_aus = case_when(native_anywhere_in_aus=="considered native to Australia by APC" ~ "Native",
#                                             native_anywhere_in_aus=="not considered native to Australia by APC" ~ "Introduced",
#                                                TRUE ~ "unknown") 
#   )

# Subset for app
NSW_plants_cleaned <- NSW_plants_tzdate |>
  select(species:family, collectionDate,
         lat,long,
         voucher_type, voucher_location,
         recordedBy) |> 
clean_names("title")

### Saving
write_parquet(NSW_plants_cleaned, paste0("infinity-app/data/NSW-", taxa,Sys.Date(), ".parquet"))
