# Unit tests for decompTs function
test_that("decompTs validates input and returns the expected class", {
  # Create a valid monthly 'ts' object
  x <- ts(rnorm(120), start = c(2000, 1), frequency = 12)
  
  # Test if decompTs returns the expected class
  expect_s3_class(decompTs(x), "mts")
})

test_that("decompTs throws an error for non-monthly 'ts' object", {
  # Create a non-monthly 'ts' object
  x <- ts(rnorm(100), start = c(2000, 1), frequency = 4)
  
  # Test if decompTs throws an error
  expect_error(decompTs(x), "x must be a monthly 'ts' vector")
})

test_that("decompTs correctly decomposes the time series", {
  # Create a monthly 'ts' object
  x <- ts(rnorm(120), start = c(2000, 1), frequency = 12)
  
  # Test if decompTs decomposes the time series correctly
  result <- decompTs(x)
  expect_s3_class(result, "mts")
  expect_equal(ncol(result), 5)  # Check the number of components
  
  # Check the components
  expect_s3_class(result[, 1], "ts")
  expect_s3_class(result[, 2], "ts")
  expect_s3_class(result[, 3], "ts")
  expect_s3_class(result[, 4], "ts")
  expect_s3_class(result[, 5], "ts")
})

test_that("decompTs handles different type and center options", {
  # Create a monthly 'ts' object
  x <- ts(rnorm(120), start = c(2000, 1), frequency = 12)
  
  # Test with type = "mult" and center = "median"
  result1 <- decompTs(x, type = "mult", center = "median")
  expect_s3_class(result1, "mts")
  
  # Test with type = "add" and center = "mean"
  result2 <- decompTs(x, type = "add", center = "mean")
  expect_s3_class(result2, "mts")
  
})
