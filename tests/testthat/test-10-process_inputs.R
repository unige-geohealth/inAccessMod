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
