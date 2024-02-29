test_that("Loading a place successfully", {
  places <- list.files(system.file(package = "infinitylists", "extdata", "places"))
  paths <- file.path(system.file(package = "infinitylists", "extdata", "places", places))
  
  kmls <- purrr::map(paths,
             ~load_place(.x)
             )
  
  expect_visible(purrr::map(paths,
              ~load_place(.x))
  )
  
  expect_type(kmls[1], "list")
})


test_that("Circle draws", {
  
})