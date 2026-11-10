# Build static-site data for the infinitylists web app.
#
# Turns the release parquet files (one per taxon-country) into small spatial
# tiles that a browser can fetch directly from GitHub Pages, plus the metadata
# the front end needs (datasets.json, per-dataset manifest.json, places.geojson).
#
# Usage (from the repo root):
#   Rscript data-raw/build_tiles.R <release_parquet_dir> [out_dir] [max_rows]
#
#   release_parquet_dir  directory holding Living-Atlas-*.parquet release assets
#   out_dir              defaults to site/app
#   max_rows             max rows per tile, defaults to 60000
#
# Output layout:
#   <out_dir>/places.geojson
#   <out_dir>/data/datasets.json
#   <out_dir>/data/<dataset_id>/manifest.json
#   <out_dir>/data/<dataset_id>/<tile_id>.parquet
#
# Tiles come from an adaptive quadtree: a cell is split into four until it holds
# at most `max_rows` records. Each manifest lists tile bounding boxes so the
# browser only downloads tiles that intersect the area being queried.

suppressPackageStartupMessages({
  library(arrow)
  library(data.table)
  library(jsonlite)
  library(sf)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop("Usage: Rscript data-raw/build_tiles.R <release_parquet_dir> [out_dir] [max_rows]")
}
in_dir   <- args[[1]]
out_dir  <- if (length(args) >= 2) args[[2]] else file.path("site", "app")
max_rows <- if (length(args) >= 3) as.integer(args[[3]]) else 60000L

# Columns the app uses, in output order.
app_columns <- c(
  "Species", "Genus", "Family", "Collection Date", "Lat", "Long",
  "Voucher Type", "Repository", "Recorded by", "Link", "Establishment Means"
)

# Max quadtree depth: guards against infinite splitting when more than
# `max_rows` records share identical coordinates.
max_depth <- 16L

# Dataset labels ------------------------------------------------------------

country_names <- c(
  AU = "Australia", PH = "Philippines", MG = "Madagascar",
  PE = "Peru", FR = "France", ES = "Spain"
)

#' Parse a release filename like Living-Atlas-Plantae-AU-2025-03-10.parquet
parse_dataset_name <- function(file) {
  m <- regmatches(
    file,
    regexec("^Living-Atlas-(.+)-([A-Z]{2})-([0-9]{4}-[0-9]{2}-[0-9]{2})\\.parquet$", file)
  )[[1]]
  if (length(m) == 0) stop("Unrecognised release filename: ", file)
  taxon <- m[[2]]; cc <- m[[3]]; date <- m[[4]]
  country <- if (cc %in% names(country_names)) country_names[[cc]] else cc
  list(
    id = paste(taxon, cc, sep = "-"),
    label = paste(taxon, country),
    taxon = taxon,
    country_code = cc,
    date = date
  )
}

# Quadtree ------------------------------------------------------------------

#' Recursively assign rows to quadtree tiles.
#'
#' @param lon,lat numeric vectors of all coordinates
#' @param idx integer row indices belonging to the current cell
#' @param bbox c(xmin, ymin, xmax, ymax) of the current cell
#' @param key tile id string ("r" for root, then one digit 0-3 per level)
#' @return list of list(key, idx)
quadtree <- function(lon, lat, idx, bbox, key = "r", depth = 0L) {
  if (length(idx) <= max_rows || depth >= max_depth) {
    if (length(idx) > max_rows) {
      warning(sprintf("Tile %s exceeds max_rows (%d) at max depth", key, length(idx)))
    }
    return(list(list(key = key, idx = idx)))
  }
  xm <- (bbox[1] + bbox[3]) / 2
  ym <- (bbox[2] + bbox[4]) / 2
  east  <- lon[idx] >= xm
  north <- lat[idx] >= ym
  # 0 = SW, 1 = SE, 2 = NW, 3 = NE
  quad <- as.integer(east) + 2L * as.integer(north)
  child_bbox <- list(
    c(bbox[1], bbox[2], xm, ym),
    c(xm, bbox[2], bbox[3], ym),
    c(bbox[1], ym, xm, bbox[4]),
    c(xm, ym, bbox[3], bbox[4])
  )
  out <- list()
  for (q in 0:3) {
    sub <- idx[quad == q]
    if (length(sub) == 0) next
    out <- c(out, quadtree(lon, lat, sub, child_bbox[[q + 1]],
                           paste0(key, q), depth + 1L))
  }
  out
}

# Per-dataset tiling -------------------------------------------------------

#' Normalise one release file to the columns and types the app expects.
read_release <- function(path) {
  dt <- as.data.table(arrow::read_parquet(path))
  missing <- setdiff(app_columns, names(dt))
  if (length(missing) > 0) {
    stop(basename(path), " is missing columns: ", paste(missing, collapse = ", "))
  }
  dt <- dt[, ..app_columns]
  # GBIF files store Establishment Means as an all-NA bool; ALA as a string.
  dt[, `Establishment Means` := as.character(`Establishment Means`)]
  # Timestamps stored as UTC milliseconds (TIMESTAMP_MILLIS), which browser
  # parquet readers decode straight to JS Date objects.
  dt[, `Collection Date` := as.POSIXct(`Collection Date`, tz = "UTC")]
  dt <- dt[!is.na(Lat) & !is.na(Long)]
  dt
}

tile_schema <- arrow::schema(
  Species = arrow::utf8(),
  Genus = arrow::utf8(),
  Family = arrow::utf8(),
  `Collection Date` = arrow::timestamp("ms", timezone = "UTC"),
  Lat = arrow::float64(),
  Long = arrow::float64(),
  `Voucher Type` = arrow::utf8(),
  Repository = arrow::utf8(),
  `Recorded by` = arrow::utf8(),
  Link = arrow::utf8(),
  `Establishment Means` = arrow::utf8()
)

build_dataset <- function(path, data_dir) {
  meta <- parse_dataset_name(basename(path))
  message(sprintf("[%s] reading %s", meta$id, basename(path)))
  dt <- read_release(path)

  ds_dir <- file.path(data_dir, meta$id)
  unlink(ds_dir, recursive = TRUE)
  dir.create(ds_dir, recursive = TRUE, showWarnings = FALSE)

  root_bbox <- c(min(dt$Long), min(dt$Lat), max(dt$Long), max(dt$Lat))
  tiles <- quadtree(dt$Long, dt$Lat, seq_len(nrow(dt)), root_bbox)

  tile_meta <- vector("list", length(tiles))
  for (i in seq_along(tiles)) {
    t <- tiles[[i]]
    td <- dt[t$idx]
    # Sorting by species then newest-first improves compression (dictionary
    # runs) and matches the order the app wants.
    setorder(td, Species, -`Collection Date`)
    file <- paste0(t$key, ".parquet")
    tbl <- arrow::Table$create(td, schema = tile_schema)
    arrow::write_parquet(
      tbl, file.path(ds_dir, file),
      compression = "snappy",
      chunk_size = nrow(td) + 1L # single row group per tile
    )
    # Tight bbox of the actual points (smaller than the quadtree cell),
    # so the browser can skip more tiles.
    tile_meta[[i]] <- list(
      file = file,
      xmin = min(td$Long), ymin = min(td$Lat),
      xmax = max(td$Long), ymax = max(td$Lat),
      n = nrow(td),
      bytes = file.size(file.path(ds_dir, file))
    )
  }

  total_rows <- sum(vapply(tile_meta, `[[`, integer(1), "n"))
  if (total_rows != nrow(dt)) {
    stop(sprintf("[%s] row count mismatch: tiles %d vs source %d",
                 meta$id, total_rows, nrow(dt)))
  }

  manifest <- c(meta, list(
    source_file = basename(path),
    n = nrow(dt),
    bbox = root_bbox,
    tiles = tile_meta
  ))
  jsonlite::write_json(manifest, file.path(ds_dir, "manifest.json"),
                       auto_unbox = TRUE, digits = NA, pretty = FALSE)

  sizes <- vapply(tile_meta, `[[`, numeric(1), "bytes")
  message(sprintf("[%s] %d rows -> %d tiles, %.1f MB total, largest %.2f MB",
                  meta$id, nrow(dt), length(tiles), sum(sizes) / 1e6, max(sizes) / 1e6))

  c(meta, list(n = nrow(dt), bbox = root_bbox, n_tiles = length(tiles),
               bytes = sum(sizes)))
}

# Preloaded places ----------------------------------------------------------

build_places <- function(out_file) {
  # global.R defines `places` (named list of sfc geometries, built with
  # load_place()) using paths relative to the repo root.
  env <- new.env()
  sys.source(file.path("R", "global.R"), envir = env)
  places <- env$places

  geoms <- lapply(places, function(g) sf::st_combine(g)[[1]])
  sf_places <- sf::st_sf(
    name = names(places),
    geometry = sf::st_sfc(geoms, crs = 4326)
  )
  unlink(out_file)
  sf::st_write(sf_places, out_file, driver = "GeoJSON", quiet = TRUE,
               layer_options = c("COORDINATE_PRECISION=6", "RFC7946=YES"))
  message(sprintf("places: %d places -> %s", nrow(sf_places), out_file))
}

# Main ----------------------------------------------------------------------

data_dir <- file.path(out_dir, "data")
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

files <- sort(list.files(in_dir, pattern = "^Living-Atlas-.*\\.parquet$", full.names = TRUE))
if (length(files) == 0) stop("No Living-Atlas-*.parquet files found in ", in_dir)

datasets <- lapply(files, build_dataset, data_dir = data_dir)

index <- list(
  generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  default = "Papilionoidea-AU",
  datasets = datasets
)
jsonlite::write_json(index, file.path(data_dir, "datasets.json"),
                     auto_unbox = TRUE, digits = NA, pretty = TRUE)

build_places(file.path(out_dir, "places.geojson"))

total <- sum(vapply(datasets, `[[`, numeric(1), "bytes"))
message(sprintf("Done: %d datasets, %.1f MB of tiles in %s", length(datasets), total / 1e6, data_dir))
