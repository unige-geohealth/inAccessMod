test_that("test download_boundaries", {
  expect_equal(download_boundaries(temp_dir, country, adminLevel = 1, type = "gbOpen", alwaysDownload = TRUE), TRUE)
})
cat("\n")
