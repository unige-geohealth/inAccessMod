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