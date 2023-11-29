test_that("test download_osm", {
  expect_equal(download_osm(x = "roads", mainPath = temp_dir, country = country, alwaysDownload = TRUE, 
                            countryName = TRUE, 
                            mostRecent = TRUE, 
                            defaultClasses = TRUE), TRUE)
  expect_equal(download_osm(x = "waterLines", mainPath = temp_dir, country = country, alwaysDownload = TRUE, 
                            countryName = TRUE, 
                            mostRecent = TRUE, 
                            defaultClasses = TRUE), TRUE)
  expect_equal(download_osm(x = "naturalPolygons", mainPath = temp_dir, country = country, alwaysDownload = TRUE, 
                            countryName = TRUE, 
                            mostRecent = TRUE, 
                            defaultClasses = TRUE), TRUE)
})
cat("\n")
