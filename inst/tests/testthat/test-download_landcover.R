test_file(system.file("tests/testthat/test-download_boundaries.R", package = "inAccessMod"))
test_that("test download_landcover", {
  expect_equal(download_landcover(temp_dir, country, alwaysDownload = TRUE, mostRecent = TRUE), TRUE)
})
cat("\n")


