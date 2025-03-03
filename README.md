
<!-- README.md is generated from README.Rmd. Please edit that file -->

# infinitylists <img src="inst/figs/infinitylist_hex.png" align="right" alt="infinitylists logo" width="250" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/traitecoevo/infinitylists/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/traitecoevo/infinitylists/actions/workflows/R-CMD-check.yaml)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.12677594.svg)](https://doi.org/10.5281/zenodo.12677594)
[![Lifecycle:
Stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

`infinitylists` is a Shiny-based application that enables users to
extract species occurrence data from national and global databases to
generate species lists for any defined area.

<p align="center">
<img src="inst/figs/app_screenshot.png" alt="Infinitylists App Screenshot" width="600"/>
</p>

The application retrieves records associated with:

- **Physical vouchers** (e.g., specimens stored in herbaria or museums)
- **Photographic vouchers** (uploaded to platforms like iNaturalist)
- **Audio recordings** (uploaded to relevant databases)

For each species in the selected area, the app provides: - Voucher
type - Number of vouchers - Date of the most recent voucher - Spatial
coordinates - Voucher location - Voucher collector

Records are displayed in both a table and a map, and can be downloaded
as a CSV file.

## Data Updates

The pre-loaded dataset corresponds to the timestamp in the releases
section of GitHub. Users can also retrieve up-to-date data using the
code provided below.

## Record Display Rules

The map and table **only display the most recent record per species per
voucher type**. This means each species may be represented by up to two
records:

- **Physical voucher**
- **Photographic/audio voucher**

The total number of records is indicated in the text summary, while the
downloadable CSV contains **all records for the target area**, not just
the most recent ones. If you want all the observations in your area not
just the most recent one, use this button.

## Accessing the Web App

You can access the hosted version of `infinitylists` here:  
🔗 <https://unsw.shinyapps.io/infinitylists/>

## Local Installation

To install and run `infinitylists` locally from GitHub, use the
following commands:

``` r
# install.packages("remotes")

remotes::install_github("traitecoevo/infinitylists", build_vignettes = TRUE)

library(infinitylists)

infinitylistApp()
```

Running the application locally provides greater flexibility, allowing
you to load taxa-country combinations that may not be available in the
web app and retrieve data for any region worldwide.

## Adding New Taxa / Country Combinations

`infinitylists` includes several pre-loaded taxa-country combinations.
If you need to add a new taxon-country pair, follow these steps:

1.  Download data using the
    [`galah`](https://github.com/AtlasOfLivingAustralia/galah-R) R
    package, which interfaces with GBIF and ALA.
2.  Use the function `download_gbif_obs` to download the data and store
    it as a (fast to access) parquet file in a directory where
    `infinitylists` can access it.

``` r
# Example usage
# register with GBIF first before and include your information here
galah::galah_config(atlas = "Global",email = "youremail",password="yourpassword",username ="yourusername")

#check the size of your download first.  Some GBIF downloads may overwhelm your internet connection or storage capacity.  
# this checks the approximate size of reptile downloads from Madagascar. 
query_gbif_global("Reptilia",
                  min_year = 1980,
                  max_year = 2024,
                  country_code = "MG") |> 
  galah::atlas_counts()


download_gbif_obs(taxon = "Reptilia",country_code = "MG")

#restart the app locally:
infinitylistApp()
```

### Important Considerations

- The `taxon` argument must be a valid taxonomic group recognized by
  GBIF.
- Small taxa-country combinations download quickly, but larger datasets
  may take longer and could cause connection timeouts.
- Ensure the geographic location is correctly specified so the data
  appears in the app.
