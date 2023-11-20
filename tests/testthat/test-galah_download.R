test_that("Download exceutes", {
  skip_if_offline()
  galah::galah_config(email = Sys.getenv("ALA_EMAIL"))
  
  odonata <- download_ala_obs(taxon = "Odonata",
                   year_range = c(2022, 2023)
                   )
  
  expect_snapshot(odonata)
  expect_named(odonata)
  expect_s3_class(odonata, "tbl_df")
})
