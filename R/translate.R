# Set atlas
galah_config(
  atlas = "Global",
  username = Sys.getenv("GBIF_USER"),
  password = Sys.getenv("GBIF_PWD")
)

# Search fields
search_fields("datasetName") |> 
  show_values()

show_all_fields() |> print(n = Inf)

# Snake case
snakecase_query <- function(taxon, min_year, country_code = NULL){
  galah::galah_call() |>
  galah::galah_identify(taxon) |> 
  galah::galah_filter(
    country == country_code,
    geospatial_kosher == "true",
    species != "",
    latitude != "",
     year >= min_year,
    basis_of_record == c("HumanObservation", "PreservedSpecimen")
  ) |> galah::galah_select(
    id,
    species,
    genus,
    family,
    latitude,
    longitude,
    coordinate_uncertainty,
    last_load_date,
    occurrence_date,
    dataset_name,
    basis_of_record,
    image_url,
    institution_code,
    collector,
    sounds
  )
}

# Camel case
camelcase_query<- function(taxon, min_year){
  galah::galah_call() |>
  galah::galah_identify(taxon) |> 
  galah::galah_filter(
    spatiallyValid == TRUE,
    species != "",
    decimalLatitude != "",
    year >= min_year,
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
    sounds
  )
}

# Gbif global
global_camelcase_query<- function(taxon, min_year, country_code = NULL){
  galah::galah_call() |>
    galah::galah_identify(taxon) |> 
    galah::galah_filter(
      country == country_code,
      hasGeospatialIssue == "false",
      # species != "",
      # decimalLatitude != "",
      year >= min_year,
      # basisOfRecord == c("HUMAN_OBSERVATION", "PRESERVED_SPECIMEN")
    ) 
  # galah::galah_select(
  #     recordID,
  #     species,
  #     genus,
  #     family,
  #     decimalLatitude,
  #     decimalLongitude,
  #     coordinateUncertaintyInMeters,
  #     eventDate,
  #     datasetName,
  #     basisOfRecord,
  #     occurrenceId,
  #     institutionCode,
  #     recordedBy,
  #     sounds
  #   )
}

# Check counts
global_camelcase_query("Podarcis", 1923, "ES") |> atlas_counts()
gbif_global_es_parakeet_2000 <- global_camelcase_query("Myiopsitta", 2000, "ES") |> atlas_occurrences()

snakecase_query("Odonata", 1923) |> atlas_counts()
gt_odonata <- snakecase_query("Odonata", 1923) |> atlas_occurrences()
gt_odonata <- camelcase_query("Odonata", 1923, "ES") |> atlas_counts()

# Check data
gbif_global_es_parakeet_2000 |> 
  count(basisOfRecord)

gbif_global_es_parakeet_2000 |> 
  count(institutionCode, basisOfRecord, mediaType) |> 
  print(n = Inf)

# Process data

gbif_global_es_parakeet_2000 |> 
  filter(!is.na(occurrenceID)) |> 
  count(institutionCode)
  print(n = Inf)

gbif_global_es_parakeet_2000 |> 
  dplyr::filter(
    basisOfRecord == "PRESERVED_SPECIMEN" |
      str_detect(institutionCode, regex("inaturalist", ignore_case = TRUE)),
    is.na(coordinateUncertaintyInMeters) |
      coordinateUncertaintyInMeters <= 1000,
    !is.na(eventDate),
    !stringr::str_detect(species, "spec.$")
  ) |> 
  dplyr::mutate(
    repository = dplyr::case_when(grepl("inatur", occurrenceID) ~ occurrenceID,
                                  TRUE ~ institutionCode)
    )  |> 
  mutate(sounds = case_when(
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
  collectionDate = lubridate::ymd_hms(eventDate, tz = "UTC", quiet = TRUE),
  collectionDate = dplyr::if_else(
    is.na(collectionDate),
    lubridate::ymd(eventDate, tz = "UTC", quiet = TRUE),
    collectionDate
  )
  ) |> 
  dplyr::select(
    species, genus, family,
    collectionDate,
    lat,
    long,
    voucher_type,
    repository,
    recordedBy,
    datasetKey,
    # institutionCode, 
    # occurrenceID
  ) |> 
  dplyr::mutate(link = dplyr::case_when(grepl("https", repository) ~ repository,
                                        TRUE ~ paste0("https://www.gbif.org/dataset/", datasetKey))
  ) |> 
  janitor::clean_names("title") |> 
  write_csv("data/test_GBIF_parakeet_ES.csv")
  
  # filter(!str_detect(repository, "inat")) |> 
  # select(institutionCode, occurrenceID, repository, link) |>  
  # print(n = 400)

         