test_that("test compile_processed_data", {
  expect_equal(compile_processed_data(temp_dir, country, mostRecent = TRUE), TRUE)
})
cat("\n")


