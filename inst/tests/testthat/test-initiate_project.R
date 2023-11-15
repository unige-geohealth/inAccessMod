temp_dir <<- tempdir()
country <<- "Switzerland" # 168
test_that("test initiate_project", {
  expect_equal(initiate_project(temp_dir, testMode = TRUE), TRUE)
  expect_true(file.exists(file.path(temp_dir, country, 'data', 'config.txt')))
  expect_true(file.exists(file.path(temp_dir, country, 'data', 'log.txt')))
})
cat("\n")
