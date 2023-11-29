test_that("test download_dem", {
  expect_equal(download_dem(temp_dir, country, alwaysDownload = TRUE, mostRecent = TRUE), TRUE)
})
cat("\n")
