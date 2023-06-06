# Unit tests
test_that("aggreswmp aggregates data by days using the mean function", {
  result <- aggreswmp(swmpr_in, by = "days")
  
  # Check if the result is a swmpr object
  expect_s3_class(result, "swmpr")
  
  # Check the number of rows in the result
  expect_equal(nrow(result), 731)
  
  # Check the number of columns in the result
  expect_equal(ncol(result), 9)
  
  # Check the column names in the result
  expected_columns <- c("datetimestamp", "temp", "spcond", "sal", "do_pct", "do_mgl", 
                        "depth", "ph", "turb")
  expect_equal(colnames(result), expected_columns)
})

test_that("aggreswmp aggregates data by hours using a user-defined function", {
  custom_mean <- function(x) sum(x) / length(x)
  result <- aggreswmp(swmpr_in, by = "hours", FUN = custom_mean)
  
  # Check if the result is a swmpr object
  expect_s3_class(result, "swmpr")
  
  # Check the number of rows in the result
  expect_equal(nrow(result), 17544)
  
  # Check the number of columns in the result
  expect_equal(ncol(result), 9)
  
  # Check the column names in the result
  expected_columns <- c("datetimestamp", "temp", "spcond", "sal", "do_pct", "do_mgl", 
                        "depth", "ph", "turb")
  expect_equal(colnames(result), expected_columns)
})

test_that("Check aggreswmp errors", {
  
  expect_error(aggreswmp(wq, params = 'asdf'), 'Aggregation parameters must be present in data')
  expect_error(aggreswmp(wq, by = 'asdf'), 'Unknown value for by, see help documentation')
  
})

test_that("Warning if QAQC columns present for aggreswmp", {
  
  expect_warning(aggreswmp(apacpwq, by = 'years'), 'QAQC columns present, removed in output')

})

test_that("Check output if aggs_out is TRUE", {
  
  result <- aggreswmp(wq, params = "do_mgl", by = "years", aggs_out = T)
  result <- unique(result$datetimestamp)
  
  expect_equal(result, structure(c(15340, 15706), class = "Date"))
  
})

test_that("Check output if plot is TRUE", {
  
  result <- aggreswmp(wq, params = "do_mgl", by = "years", plot = T)

  expect_s3_class(result, 'ggplot')
  
})

