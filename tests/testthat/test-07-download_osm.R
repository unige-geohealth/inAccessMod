test_that("test download_osm", {
  expect_equal(download_osm(mainPath = temp_dir, location = country, type = "roads", alwaysDownload = TRUE, 
                            mostRecent = TRUE, 
                            defaultClasses = TRUE), TRUE)
  expect_equal(download_osm(mainPath = temp_dir, location = country, type = "waterLines", alwaysDownload = TRUE, 
                            mostRecent = TRUE, 
                            defaultClasses = TRUE), TRUE)
  expect_equal(download_osm(mainPath = temp_dir, country = country, type = "waterPolygons", alwaysDownload = TRUE, 
                            mostRecent = TRUE, 
                            defaultClasses = TRUE), TRUE)
})
cat("\n")
