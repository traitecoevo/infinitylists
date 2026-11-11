# Download and process one taxon-country dataset from ALA or GBIF, writing a
# release-ready parquet file (Living-Atlas-<taxon>-<CC>-<date>.parquet).
#
# Usage (from the repo root, with the package installed):
#   Rscript data-raw/refresh_dataset.R <taxon> <ala|gbif> <country_code> <out_dir>
#
# Credentials come from environment variables, never from code:
#   ALA:  ALA_EMAIL                                  (registered ALA email)
#   GBIF: GBIF_USERNAME, GBIF_PWD, GBIF_EMAIL        (gbif.org account)
#
# Run by .github/workflows/refresh-data.yaml for each row of
# data-raw/datasets.csv.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4) {
  stop("Usage: Rscript data-raw/refresh_dataset.R <taxon> <ala|gbif> <country_code> <out_dir>")
}
taxon <- args[[1]]
source <- tolower(args[[2]])
country_code <- args[[3]]
out_dir <- normalizePath(args[[4]], mustWork = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

need_env <- function(vars) {
  missing <- vars[!nzchar(Sys.getenv(vars))]
  if (length(missing) > 0) {
    stop("Missing environment variable(s): ", paste(missing, collapse = ", "),
         ". Set them as repository secrets for the refresh-data workflow.")
  }
}

suppressPackageStartupMessages(library(infinitylists))

started <- Sys.time()
message(sprintf("Refreshing %s-%s from %s", taxon, country_code, toupper(source)))

if (source == "ala") {
  if (country_code != "AU") stop("ALA datasets must have country_code AU")
  need_env("ALA_EMAIL")
  galah::galah_config(atlas = "Australia", email = Sys.getenv("ALA_EMAIL"), verbose = TRUE)
  download_ala_obs(taxon = taxon, output_dir = out_dir)
} else if (source == "gbif") {
  need_env(c("GBIF_USERNAME", "GBIF_PWD", "GBIF_EMAIL"))
  galah::galah_config(
    atlas = "Global",
    username = Sys.getenv("GBIF_USERNAME"),
    password = Sys.getenv("GBIF_PWD"),
    email = Sys.getenv("GBIF_EMAIL"),
    verbose = TRUE
  )
  download_gbif_obs(taxon = taxon, country_code = country_code, output_dir = out_dir)
} else {
  stop("source must be 'ala' or 'gbif', not '", source, "'")
}

pattern <- sprintf("^Living-Atlas-%s-%s-[0-9]{4}-[0-9]{2}-[0-9]{2}\\.parquet$", taxon, country_code)
written <- list.files(out_dir, pattern = pattern, full.names = TRUE)
if (length(written) != 1) {
  stop(sprintf("Expected exactly one output file matching %s in %s, found %d",
               pattern, out_dir, length(written)))
}
n <- nrow(arrow::open_dataset(written))
if (n == 0) stop(basename(written), " has no rows")

message(sprintf("Wrote %s: %s rows in %.1f min", basename(written),
                format(n, big.mark = ","), as.numeric(difftime(Sys.time(), started, units = "mins"))))
