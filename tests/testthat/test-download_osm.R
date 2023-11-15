test_file(system.file("tests/testthat/test-download_boundaries.R", package = "inAccessMod"))
test_that("test download_osm", {
  expect_equal(download_osm(mainPath = temp_dir, x = "roads", country = country, alwaysDownload = TRUE, 
                            countryName = TRUE, 
                            mostRecent = TRUE, 
                            defaultClasses = TRUE), TRUE)
  expect_equal(download_osm(mainPath = temp_dir, x = "waterLines", country = country, alwaysDownload = TRUE, 
                            countryName = TRUE, 
                            mostRecent = TRUE, 
                            defaultClasses = TRUE), TRUE)
  expect_equal(download_osm(mainPath = temp_dir, x = "naturalPolygons", country = country, alwaysDownload = TRUE, 
                            countryName = TRUE, 
                            mostRecent = TRUE, 
                            defaultClasses = TRUE), TRUE)
})
cat("\n")
