test_that("test download_population", {
  expect_equal(download_population(temp_dir, country, alwaysDownload = TRUE, testMode = T), TRUE)
})
cat("\n")
