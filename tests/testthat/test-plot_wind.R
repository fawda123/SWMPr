# Test if the function returns the expected output class
test_that("Function returns the expected output class", {
  expect_s3_class(plot_wind(met), "openair")
})

# Test if the function throws an error when years argument is missing or invalid
test_that("Function throws an error for missing or invalid years argument", {
  expect_error(plot_wind(swmpr_in, years = NULL), "No weather data")
  expect_error(plot_wind(swmpr_in, years = 2024), "Check years")
})