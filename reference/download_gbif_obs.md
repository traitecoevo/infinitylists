# Download and Process GBIF Observations

This function retrieves, processes, and saves GBIF (Global Biodiversity
Information Facility) observation data for a specified taxon.

## Usage

``` r
download_gbif_obs(
  taxon,
  min_year = 1923,
  max_year = as.numeric(format(Sys.Date(), "%Y")),
  country_code = NULL,
  save_raw_data = FALSE,
  output_dir = file.path(system.file(package = "infinitylists"), "data/")
)
```

## Arguments

- taxon:

  Character. The taxon (species, genus, etc.) for which to retrieve GBIF
  data.

- min_year:

  Numeric. The minimum year for the observations to be retrieved.
  Default is 1923.

- max_year:

  Numeric. The maximum year for the observations to be retrieved.
  Default is the current year.

- country_code:

  Character. The ISO 3166-1 alpha-2 country code to filter observations
  by country. Default is NULL (no country filter).

- save_raw_data:

  Logical. Whether to save the raw data retrieved from GBIF. Default is
  FALSE.

- output_dir:

  Character. The directory where the processed data will be saved.
  Default is a "data" directory within the "infinitylists" package.

## Value

None. The function saves the processed data to the specified output
directory.
