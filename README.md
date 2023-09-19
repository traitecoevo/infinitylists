
<!-- README.md is generated from README.Rmd. Please edit that file -->

# infinitylists <img src="inst/figs/infinitylist_hex.png" align="right" alt="" width="250" />

<!-- badges: start -->
<!-- badges: end -->

This shiny-based application allows users to extract plant occurrence
data from the Atlas of Living Australia and generate a species list for
any defined area. All records associated with either a physical voucher
(stored in Australian herbaria or museum), a sound file, or a
photographic voucher (stored in iNaturalist) are extracted. For each
species within the defined area, the application will return voucher
type, number of vouchers, date of the most recent voucher, spatial
coordinates, voucher location, and the voucher collector. Records are
displayed both in a table and on a map, and are downloadable as a CSV.

The app can be access
here:<https://posit-connect-unsw.intersect.org.au/infinitylists/>

## Installation

You can install the development version of ‘infinitylists’ from
[GitHub](https://github.com/traitecoevo/infinitylists) with:

``` r
# install.packages("remotes")
remotes::install_github("traitecoevo/infinitylists")
```

## Run the app locally

``` r
library(infinitylists)

infinitylistApp()
```
