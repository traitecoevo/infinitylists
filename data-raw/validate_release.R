# Check a set of freshly downloaded release files against the previous release
# before publishing, and fill in any dataset that wasn't refreshed.
#
# Usage (from the repo root):
#   Rscript data-raw/validate_release.R <new_dir> <prev_dir> [summary.md] [max_drop]
#
#   new_dir   freshly downloaded Living-Atlas-*.parquet files
#   prev_dir  the previous release's files
#   summary   markdown table written here (used as the release notes)
#   max_drop  largest allowed fractional fall in row count, default 0.05
#
# For every dataset in data-raw/datasets.csv:
#   * not refreshed (no new file)  -> the previous file is copied into new_dir
#   * refreshed                    -> must have the same columns and column
#                                     types as the previous file (the live
#                                     Shiny app and build_tiles.R rely on it),
#                                     and must not have shrunk by > max_drop.
#                                     Large falls usually mean a download chunk
#                                     failed silently (purrr::possibly in the
#                                     download functions).
# Exits non-zero if any check fails, so nothing gets published.

suppressPackageStartupMessages(library(arrow))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript data-raw/validate_release.R <new_dir> <prev_dir> [summary.md] [max_drop]")
}
new_dir <- args[[1]]
prev_dir <- args[[2]]
summary_file <- if (length(args) >= 3) args[[3]] else ""
max_drop <- if (length(args) >= 4) as.numeric(args[[4]]) else 0.05

datasets <- utils::read.csv(file.path("data-raw", "datasets.csv"), stringsAsFactors = FALSE)

find_file <- function(dir, taxon, cc) {
  pattern <- sprintf("^Living-Atlas-%s-%s-[0-9]{4}-[0-9]{2}-[0-9]{2}\\.parquet$", taxon, cc)
  f <- sort(list.files(dir, pattern = pattern, full.names = TRUE))
  if (length(f) > 1) stop("More than one file for ", taxon, "-", cc, " in ", dir)
  if (length(f) == 0) NA_character_ else f
}

schema_of <- function(f) {
  s <- arrow::open_dataset(f)$schema
  stats::setNames(vapply(s$fields, function(x) x$type$ToString(), ""), s$names)
}

fmt <- function(n) format(n, big.mark = ",", scientific = FALSE)

rows <- list()
failures <- character()

for (i in seq_len(nrow(datasets))) {
  taxon <- datasets$taxon[i]
  cc <- datasets$country_code[i]
  id <- paste(taxon, cc, sep = "-")
  new_f <- find_file(new_dir, taxon, cc)
  prev_f <- find_file(prev_dir, taxon, cc)
  prev_n <- if (is.na(prev_f)) NA_integer_ else nrow(arrow::open_dataset(prev_f))

  if (is.na(new_f)) {
    if (is.na(prev_f)) {
      failures <- c(failures, sprintf("%s: no new file and no previous file", id))
      next
    }
    file.copy(prev_f, new_dir)
    rows[[id]] <- c(id, basename(prev_f), fmt(prev_n), fmt(prev_n), "0.0%", "carried over")
    next
  }

  new_n <- nrow(arrow::open_dataset(new_f))
  status <- "ok"
  if (is.na(prev_f)) {
    change <- "new"
  } else {
    pct <- (new_n - prev_n) / prev_n
    change <- sprintf("%+.1f%%", 100 * pct)
    new_s <- schema_of(new_f)
    prev_s <- schema_of(prev_f)
    if (!identical(new_s, prev_s)) {
      diff <- paste(
        sprintf("previous: %s", paste(names(prev_s), prev_s, sep = " <", collapse = ">, ")),
        sprintf("new: %s", paste(names(new_s), new_s, sep = " <", collapse = ">, ")),
        sep = "\n    "
      )
      failures <- c(failures, sprintf("%s: schema changed\n    %s>", id, diff))
      status <- "SCHEMA CHANGED"
    }
    if (pct < -max_drop) {
      failures <- c(failures, sprintf("%s: row count fell %.1f%% (%s -> %s), more than the %.0f%% allowed",
                                      id, -100 * pct, fmt(prev_n), fmt(new_n), 100 * max_drop))
      status <- "TOO FEW ROWS"
    }
  }
  if (new_n == 0) {
    failures <- c(failures, sprintf("%s: new file has no rows", id))
    status <- "EMPTY"
  }
  rows[[id]] <- c(id, basename(new_f), if (is.na(prev_n)) "-" else fmt(prev_n), fmt(new_n), change, status)
}

table <- c(
  "| Dataset | File | Previous rows | Rows | Change | Status |",
  "|---|---|---:|---:|---:|---|",
  vapply(rows, function(r) paste0("| ", paste(r, collapse = " | "), " |"), "")
)
cat(table, sep = "\n")
if (nzchar(summary_file)) writeLines(table, summary_file)

if (length(failures) > 0) {
  message("\nValidation FAILED:\n  ", paste(failures, collapse = "\n  "))
  quit(status = 1)
}
message("\nValidation passed for ", nrow(datasets), " datasets.")
