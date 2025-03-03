
<!-- README.md is generated from README.Rmd. Please edit that file -->

# infinitylists <img src="inst/figs/infinitylist_hex.png" align="right" alt="" width="250" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/traitecoevo/infinitylists/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/traitecoevo/infinitylists/actions/workflows/R-CMD-check.yaml)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.12677594.svg)](https://doi.org/10.5281/zenodo.12677594)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

This Shiny-based application allows users to extract species occurrence
data from national or global databases to generate a species list for
any defined area. All records associated with either a physical voucher
(stored in herbaria or museums), or a photographic voucher or audio file
uploaded to iNaturalist are extracted. For each species within the
defined area, the application will return voucher type, number of
vouchers, date of the most recent voucher, spatial coordinates, voucher
location, and the voucher collector. Records are displayed both in a
table and on a map, and are downloadable as a CSV.

The pre-loaded data was downloaded at the time listed in the releases
section of github, but the user can also download up-to-date date
following the code below.

## Which records are returned?

The map and table outputs will only display the **most recent record per
species per voucher type**. Each species will therefore be represented
on the map and in the table by a maximum of 3 records (for species with
a physical voucher, photographic voucher and audio voucher in the target
area). The text statement indicates the total number of records, and the
downloadable CSV file contains **all records from the target area**, not
just the most recent records.

## Use the web app

The app can be accessed here: <https://unsw.shinyapps.io/infinitylists/>

## Local installation

You can install and run a local version of `infinitylists` from
[GitHub](https://github.com/traitecoevo/infinitylists). This allows more
flexibility for loading taxa that are not automatically loaded in the
web app, and loading data for other regions of the world. The code to do
this is:

``` r
# install.packages("remotes")

remotes::install_github("traitecoevo/infinitylists", build_vignettes = TRUE)

library(infinitylists)

infinitylistApp()
```

## Adding new taxa for Australia

`infinitylists` comes with seven taxa loaded to start with: plants,
butterflies, cicadas, marsupials, and dragonflies + damselflies (plus
arachnids for France, and lizards and snakes for Spain). If you want to
add another taxon from Australia, you’ll need to download the data first
via the [galah
interface](https://github.com/AtlasOfLivingAustralia/galah-R) to the
ALA. The function `download_ala_obs` will download the data and put it
into a directory where infinitylists can find it. The value for `taxon`
needs to be a [valid taxonomic group as recognized by the
ALA](https://support.ala.org.au/support/solutions/articles/6000261677-taxonomy-a-species-filing-system).
The download step is fast for taxa with small number of observations in
the ALA and slower for taxa with millions of observations.

``` r

# register with GBIF first before and include your information here
galah::galah_config(atlas = "Global",email = "youremail",password="yourpassword",username ="yourusername")

#check the size of your download first.  Some GBIF downloads may overwhelm your internet connection or storage capacity.  
# this checks the approximate size of reptile downloads from Madagascar
query_gbif_global("Reptilia",
                  min_year = 1980,
                  max_year = 2024,
                  country_code = "MG") |> 
  galah::atlas_counts()

# download the data, this needs to be a valid taxa name
download_gbif_obs(taxon = "Reptilia",country_code = "MG")

r<-arrow::read_parquet("/Library/Frameworks/R.framework/Versions/4.4-x86_64/Resources/library/infinitylists/data/Living-Atlas-Reptilia-MG-2025-03-03.parquet")

infinitylistApp()
```
