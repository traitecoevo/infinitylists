# Download Observations for Infinity List

This function downloads observation data for the given taxon and year
range from the "Infinity List" source, processes and cleans the data,
adds additional columns (e.g., establishment status), and optionally
saves the raw and processed data.

## Usage

``` r
download_ala_obs(
  taxon,
  year_range = c(1923, as.numeric(format(Sys.Date(), "%Y"))),
  save_raw_data = FALSE,
  output_dir = file.path(system.file(package = "infinitylists"), "data/")
)
```

## Arguments

- taxon:

  A character vector or string specifying the taxon (e.g., species,
  genus) for which observations are to be downloaded.

- year_range:

  A numeric vector of length 2 indicating the start and end years for
  data retrieval. Default is from 1923 to the current year

- save_raw_data:

  A logical value indicating whether to save the raw data. By default,
  raw data is not saved (`FALSE`).

- output_dir:

  A character string specifying the directory where any saved data (raw
  or processed) will be stored. Default is
  `file.path(system.file(package = "infinitylists"), "data/")`.

## Value

This function saves the processed data and returns invisibly. The
structure and content of the returned value (if any) is determined by
the functions called within (e.g., `retrieve_data`, `process_data`).

## Details

The function carries out the following steps:

1.  Retrieve the data from the "Infinity List" source.

2.  Process and clean the retrieved data to remove any inconsistencies.

3.  Add additional columns to the cleaned data, such as the
    establishment status for the given taxon.

4.  If `save_raw_data` is `TRUE`, save the processed data to the
    specified `output_dir`.
