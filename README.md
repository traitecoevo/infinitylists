
<!-- README.md is generated from README.Rmd. Please edit that file -->

# infinitylists

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

## Installation

You can install the development version of infinitylists from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("traitecoevo/plant-map-app")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(infinitylists)

runApp()
```
