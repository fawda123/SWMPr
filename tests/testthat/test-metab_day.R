
# Mock data for dat_in
dat_in <- data.frame(
  datetimestamp = as.POSIXct(c("2023-01-01 10:00:00", "2023-01-01 12:00:00", "2023-01-02 08:00:00", "2023-01-03 16:00:00")),
  temperature = c(25, 28, 20, 22),
  humidity = c(60, 65, 55, 50)
)

test_that("metab_day returns the expected output", {
  # Call the function
  output <- metab_day(dat_in, tz = "UTC", lat = 40, long = -75)
  
  # Check if the output has the expected structure
  expect_equal(ncol(output), 7)
  expect_equal(nrow(output), nrow(dat_in))
  
  # Check if the solar_period and solar_time columns are present
  expect_true("solar_period" %in% names(output))
  expect_true("solar_time" %in% names(output))
  
  # Check if the metab_date column is of class Date
  expect_true("metab_date" %in% names(output))
  expect_s3_class(output$metab_date, "Date")
  
  # Check if the day_hrs column is calculated correctly
  expect_true("day_hrs" %in% names(output))
  expect_equal(length(unique(output$metab_date)), 3)

})