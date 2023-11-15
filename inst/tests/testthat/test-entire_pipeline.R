temp_dir <- tempdir()
country <- "Switzerland" # 168

test_that("test initiate_project", {
  expect_equal(initiate_project(temp_dir, testMode = TRUE), TRUE)
  # Inspect directory manually or test existence of files and folders
  expect_true(file.exists(file.path(temp_dir, country, 'data', 'config.txt')))
  expect_true(file.exists(file.path(temp_dir, country, 'data', 'log.txt')))
})
cat("\n")
test_that("test download_boundaries", {
  expect_equal(download_boundaries(temp_dir, country, adminLevel = 1, type = "gbOpen", alwaysDownload = TRUE), TRUE)
})
cat("\n")
test_that("test set_projection", {
  expect_equal(set_projection(temp_dir, country, mostRecent = TRUE, alwaysSet = TRUE, bestCRS = TRUE), TRUE)
})
cat("\n")
test_that("test download_population", {
  expect_equal(download_population(temp_dir, country, alwaysDownload = TRUE, testMode = T), TRUE)
})
cat("\n")
test_that("test download_dem", {
  expect_equal(download_dem(temp_dir, country, alwaysDownload = TRUE, mostRecent = TRUE), TRUE)
})
cat("\n")
test_that("test download_landcover", {
  expect_equal(download_landcover(temp_dir, country, alwaysDownload = TRUE, mostRecent = TRUE), TRUE)
})
cat("\n")
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
test_that("test HeRAMS_filter_hf", {
  expect_equal(HeRAMS_filter_hf(temp_dir, country, pathTableCode = NULL, 
                                pathTableText = NULL, 
                                scenario = NULL, 
                                mostRecentObs = TRUE, 
                                defaultParameters = TRUE, 
                                region = FALSE, 
                                type = FALSE, 
                                ownership = FALSE, 
                                status = FALSE, 
                                building = FALSE, 
                                equipment = FALSE, 
                                functionality = FALSE, 
                                accessibility = FALSE, 
                                support = FALSE, 
                                services = FALSE, 
                                partners = FALSE, 
                                barriers = FALSE,
                                testMode = TRUE), TRUE)
})
cat("\n")
test_that("test HeRAMS_create_hf_shapefile", {
  expect_equal(suppressWarnings({HeRAMS_create_hf_shapefile(temp_dir, country, mostRecentBoundaries = TRUE, 
                                                            rmNA = TRUE, 
                                                            rmOut = TRUE)}), TRUE)
})
cat("\n")
test_that("test process_inputs", {
  expect_equal(suppressWarnings({process_inputs(temp_dir, country, selectedInputs = "All",
                                                mostRecent = TRUE, 
                                                alwaysProcess = TRUE,
                                                defaultMethods = TRUE,
                                                changeRes = FALSE,
                                                popCorrection = TRUE,
                                                gridRes = 10000,
                                                testMode = TRUE)}), TRUE)
})
cat("\n")
test_that("test label_landcover", {
  expect_equal(label_landcover(temp_dir, country, mostRecent = TRUE, overwrite = TRUE, defaultLabels = TRUE), TRUE)
})
cat("\n")
test_that("test compile_processed_data", {
  expect_equal(compile_processed_data(temp_dir, country, mostRecent = TRUE), TRUE)
})





