test_that("Loading a place successfully", {
  places <- list.files(system.file(package = "infinitylists", "extdata", "places"))
  paths <- file.path(system.file(package = "infinitylists", "extdata", "places", places))
  
  kmls <- purrr::map(paths,
             ~load_place(.x)
             )
  
  expect_visible(purrr::map(paths,
              ~load_place(.x))
  )
  
  expect_type(kmls[[1]], "list")
  
  expect_visible(places)
  expect_type(places, "character")
})


test_that("Circle or buffer is added", {
  # malabar
  radii <- seq(10,100, by = 10)
  
  expect_visible(purrr::map(radii,
             ~create_circle_polygon(lat = 33.9679, long = 151.2511, .x)
  ))
  
  with_circles <- purrr::map(radii,
                ~create_circle_polygon(lat = 33.9679, long = 151.2511, .x)
  )
  expect_type(with_circles[[1]], "list")
  
  expect_error(add_buffer(with_circles, 5))
})

test_that("Intersections are working", {
  occ <- tibble::tibble(lat = 33.9679, long = 151.2511)
  occ_sf <- sf::st_as_sf(occ, coords = c("long", "lat"), crs = 4326)
  
  expect_true(points_in_target(occ_sf, create_circle_polygon(lat = 33.9679, long = 151.2511, 10)))
  
  expect_true(points_in_buffer(occ_sf, create_circle_polygon(lat = 33.9679, long = 151.2511, 10), buffer_size = 10))
})



  


