test_that("test label_landcover", {
  expect_equal(label_landcover(temp_dir, country, mostRecent = TRUE, overwrite = TRUE, defaultLabels = TRUE), TRUE)
})
cat("\n")
