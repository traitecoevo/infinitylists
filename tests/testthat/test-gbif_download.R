fake_counter <- function(counts) {
  function(from, to) sum(counts[as.character(from:to)])
}

test_that("split_year_range + merge_year_ranges cover every year once", {
  counts <- stats::setNames(c(rep(10, 80), rep(1000, 24)), 1923:2026)
  count_years <- fake_counter(counts)
  n <- count_years(1923, 2026)

  chunks <- merge_year_ranges(
    split_year_range(1923, 2026, n, count_years, 5000), 5000)

  years <- unlist(lapply(chunks, function(x) seq(x$from, x$to)))
  expect_identical(years, 1923:2026)
  expect_equal(sum(vapply(chunks, function(x) x$count, numeric(1))), n)
  for (x in chunks) {
    expect_equal(x$count, count_years(x$from, x$to))
    expect_lte(x$count, 5000)
  }
})

test_that("a range under the limit stays as one chunk", {
  count_years <- function(from, to) stop("should not be called")
  chunks <- split_year_range(1923, 2026, 100, count_years, 5000)
  expect_length(chunks, 1)
  expect_equal(chunks[[1]][c("from", "to")], list(from = 1923, to = 2026))
})

test_that("a single year over the limit gets a chunk of its own", {
  counts <- stats::setNames(c(10, 10, 9000, 10), 2020:2023)
  count_years <- fake_counter(counts)
  chunks <- merge_year_ranges(
    split_year_range(2020, 2023, count_years(2020, 2023), count_years, 5000), 5000)
  ranges <- vapply(chunks, function(x) paste(x$from, x$to, sep = "-"), "")
  expect_identical(ranges, c("2020-2021", "2022-2022", "2023-2023"))
})
