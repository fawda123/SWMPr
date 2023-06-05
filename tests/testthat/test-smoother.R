# Define the test cases
test_that("smoother performs convolution smoothing", {

  # Call the function
  out <- smoother(wq, window = 3, sides = 2)
  
  # Check if the output has the correct number of rows
  expect_equal(nrow(out), 70176)
  
  # Check if the output has the correct column names
  expect_equal(colnames(out), c("datetimestamp", "temp", "spcond", "sal", "do_pct", "do_mgl", 
                                "depth", "cdepth", "level", "clevel", "ph", "turb", "chlfluor"
  ))
  
  # Check if the values are smoothed correctly
  expect_equal(out$temp[1:6], c(NA, 17.3, 17.3333333333333, 17.3666666666667, 17.4, 17.4), tolerance = 1e3)
})

test_that("smoother performs convolution smoothing for specific parameters", {
 
  # Call the function with specific parameters
  out <- smoother(wq, window = 50, params = 'do_mgl')
  
  # Check if the output has the correct number of rows
  expect_equal(ncol(out), 2)
  
  # Check if the output has the correct column names
  expect_equal(colnames(out), c("datetimestamp", "do_mgl"))
  
  # Check if the other parameter is not present in the output
  expect_false("temp" %in% colnames(out))
  
})

test_that("Check error if params argument is incorrect", {
  
  expect_error(smoother(wq, params = c('test')), 
               'Params argument must name input columns')
  
})

test_that("Check warning if qaqc columns present", {
  
  expect_warning(smoother(apacpwq), 
               'QAQC columns present, removed in output')
  
})

