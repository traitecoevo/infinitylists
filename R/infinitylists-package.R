#' @keywords internal
#' @import shiny
#' @import data.table
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

.onAttach <- function(libname, pkgname) {
  shiny::addResourcePath('www',
                         system.file('www',
                                     package = 'infinitylists'))
}


utils::globalVariables(
  c(
    ".",
    "Collection Date",
    "Family",
    "Genus",
    "In target area",
    "Lat",
    "Long",
    "N",
    "Record Id",
    "Recorded by",
    "Species",
    "Voucher Location",
    "Voucher Type",
    "basisOfRecord",
    "collectionDate",
    "coordinateUncertaintyInMeters",
    "datasetName",
    "decimalLatitude",
    "decimalLongitude",
    "download.file",
    "eventDate",
    "family",
    "genus",
    "institutionCode",
    "isDuplicateOf",
    "join_by",
    "lat",
    "long",
    "native_anywhere_in_aus",
    "outlierLayerCount",
    "recordID",
    "recordedBy",
    "references",
    "rgb",
    "setNames",
    "sounds",
    "spatiallyValid",
    "species",
    "str_detect",
    "voucher_location",
    "voucher_type",
    "write.csv"
  )
)