test_file(system.file("tests/testthat/test-download_boundaries.R", package = "inAccessMod"))
test_that("test set_projection", {
  expect_equal(set_projection(temp_dir, country, mostRecent = TRUE, alwaysSet = TRUE, bestCRS = TRUE), TRUE)
})
cat("\n")