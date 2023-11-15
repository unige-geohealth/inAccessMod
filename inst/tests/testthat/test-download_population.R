test_file(system.file("tests/testthat/test-initiate_project.R", package = "inAccessMod"))
test_that("test download_population", {
  expect_equal(download_population(temp_dir, country, alwaysDownload = TRUE, testMode = T), TRUE)
})
cat("\n")
