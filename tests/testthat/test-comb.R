
# Test case 1: Combining two SWMP objects with default arguments
test_that("comb.swmpr combines two SWMP objects with default arguments", {

  # Call the comb.swmpr function
  result <- comb(nut, met, method = 'apacpnut')
  
  # Perform assertions to check the output
  expect_s3_class(result, "swmpr")  # Check if the result is of class "swmpr"
  
  expected_columns <- c("datetimestamp", "po4f", "nh4f", "no2f", "no3f", "no23f", "chla_n", 
                        "atemp", "rh", "bp", "wspd", "maxwspd", "wdir", "sdwdir", "totpar", 
                        "totprcp", "cumprcp", "totsorad")
  expect_equal(names(result), expected_columns) 
  
})

test_that("comb.default returns correct output", {
  
  swmpr_in1 <- data.frame(wq)
  swmpr_in2 <- data.frame(nut)
  res <- comb(swmpr_in1, swmpr_in2, date_col = 'datetimestamp', method = 2, timestep = 'years')
  
  expect_s3_class(res, 'data.frame')
  
})

# Test case 2: Combining three SWMP objects with specified timestep and method
test_that("comb.swmpr combines three SWMP objects with specified timestep and method", {
  
  # Call the comb function with specified arguments
  result <- comb(wq, met, timestep = 30, method = "intersect")
  
  # Perform assertions to check the output
  expect_s3_class(result, "swmpr")  # Check if the result is of class "swmpr"
  
  expected_columns <- c("datetimestamp", "temp", "spcond", "sal", "do_pct", "do_mgl", 
                        "depth", "cdepth", "level", "clevel", "ph", "turb", "chlfluor", 
                        "atemp", "rh", "bp", "wspd", "maxwspd", "wdir", "sdwdir", "totpar", 
                        "totprcp", "cumprcp", "totsorad")
  expect_equal(names(result), expected_columns) 
  
})

test_that("Warning if qaqc present in comb", {
  
  expect_warning(comb(apacpwq, apacpnut, timestep = 'years'), 'QAQC columns present, removed from output')
  
})

test_that("Checking errors for comb.swmpr", {
  
  expect_error(comb(wq, met, method = 'asdf'), 'Method must be intersect, union, or station name')
  expect_error(comb(wq, wq), 'Unable to combine duplicated data types')
  expect_error(comb(as.list(wq, nut)))
  
  swmpr_in <- wq
  attributes(swmpr_in)$timezone <- 'asdf'
  expect_error(comb(swmpr_in, nut), 'Input data are from multiple timezones')
  
})

test_that("Checking errors for comb.default", {

  swmpr_in1 <- data.frame(wq)
  swmpr_in2 <- data.frame(nut)
  attr(swmpr_in1[, 'datetimestamp'], 'tzone') <- 'asdf'
  expect_error(comb(swmpr_in1, swmpr_in2, date_col = 'datetimestamp'), 'Input data are from multiple timezones')
  
  swmpr_in1 <- data.frame(wq)
  expect_error(comb(swmpr_in1, swmpr_in2, date_col = 'datetimestamp', method = 3), 'numeric value for method must specify an index for the input data')
  expect_error(comb(swmpr_in1, swmpr_in2, date_col = 'datetimestamp', method = 'asdf'), 'character value for method must be intersect or union')
  comb(swmpr_in1, swmpr_in2, date_col = 'datetimestamp', differ = 6)
})