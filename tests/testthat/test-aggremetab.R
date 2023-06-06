# Unit tests
test_that("aggremetab aggregates metabolism data by weeks", {
  result <- aggremetab(resmet, by = "weeks")
  
  # Check if the result is a data frame
  expect_s3_class(result, "data.frame")
  
  # Check the number of rows in the result
  expect_equal(nrow(result), 294)
  
  # Check the column names in the result
  expected_columns <- c("date", "Estimate", "val", "lower", "upper")
  expect_equal(colnames(result), expected_columns)
})

test_that("aggremetab aggregates metabolism data by months", {
  result <- aggremetab(resmet, by = "months")
  
  # Check if the result is a data frame
  expect_s3_class(result, "data.frame")
  
  # Check the number of rows in the result
  expect_equal(nrow(result), 72)
  
  # Check the column names in the result
  expected_columns <- c("date", "Estimate", "val", "lower", "upper")
  expect_equal(colnames(result), expected_columns)
})

test_that("aggremetab aggregates metabolism data using a moving window average", {
  result <- aggremetab(resmet, by = 30)
  
  # Check if the result is a data frame
  expect_s3_class(result, "data.frame")
  
  # Check the column names in the result
  expected_columns <- c("date", "Estimate", "val")
  expect_equal(colnames(result), expected_columns)
})

test_that("aggrematab returns appropriate errors", {
  
  expect_error(aggremetab(resmet, by = 'asdf'), 'Unknown value for by, see help documentation')
  expect_error(aggremetab(wq, by = 'years'), 'No metabolism data, use the ecometab function')
  expect_error(aggremetab(resmet, by = T), 'By argument must be character string of aggregation period or numeric indicating number of days')
  
})