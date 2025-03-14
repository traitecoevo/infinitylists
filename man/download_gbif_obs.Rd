% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gbif_download.R
\name{download_gbif_obs}
\alias{download_gbif_obs}
\title{Download and Process GBIF Observations}
\usage{
download_gbif_obs(
  taxon,
  min_year = 1923,
  max_year = as.numeric(format(Sys.Date(), "\%Y")),
  country_code = NULL,
  save_raw_data = FALSE,
  output_dir = file.path(system.file(package = "infinitylists"), "data/")
)
}
\arguments{
\item{taxon}{Character. The taxon (species, genus, etc.) for which to retrieve GBIF data.}

\item{min_year}{Numeric. The minimum year for the observations to be retrieved. Default is 1923.}

\item{max_year}{Numeric. The maximum year for the observations to be retrieved. Default is the current year.}

\item{country_code}{Character. The ISO 3166-1 alpha-2 country code to filter observations by country. Default is NULL (no country filter).}

\item{save_raw_data}{Logical. Whether to save the raw data retrieved from GBIF. Default is FALSE.}

\item{output_dir}{Character. The directory where the processed data will be saved. Default is a "data" directory within the "infinitylists" package.}
}
\value{
None. The function saves the processed data to the specified output directory.
}
\description{
This function retrieves, processes, and saves GBIF (Global Biodiversity Information Facility) observation data for a specified taxon.
}
