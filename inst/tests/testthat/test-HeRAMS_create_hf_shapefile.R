test_file(system.file("tests/testthat/test-initiate_project.R", package = "inAccessMod"))
test_file(system.file("tests/testthat/test-HeRAMS_filter_hf", package = "inAccessMod"))
test_that("test HeRAMS-create_hf_shapefile", {
  expect_equal(suppressWarnings({HeRAMS_create_hf_shapefile(temp_dir, country, mostRecentBoundaries = TRUE, 
                                          rmNA = TRUE, 
                                          rmOut = TRUE)}), TRUE)
})
cat("\n")