# Test case 1: Decomposing a SWMP object with default arguments
test_that("decomp_cj.swmpr decomposes a SWMP object with default arguments", {

  # Specify the parameter to decompose
  parameter <- "do_mgl"  # Replace with the parameter to decompose
  
  # Call the decomp_cj.swmpr function
  result <- decomp_cj(wq, param = parameter)
  
  # Perform assertions to check the output
  expect_s3_class(result, "ggplot")  # Check if the result is of class "ggplot"
  
})

# Test case 2: Decomposing a SWMP object with specified arguments
test_that("decomp_cj.swmpr decomposes a SWMP object with specified arguments", {
  
  # Specify the parameter and other arguments
  parameter <- "do_mgl"  # Replace with the parameter to decompose
  vals_out <- TRUE  # Return the decomposition values instead of a plot
  
  # Call the decomp_cj.swmpr function with specified arguments
  result <- decomp_cj(wq, param = parameter, vals_out = vals_out)
  
  # Perform assertions to check the output
  expect_s3_class(result, "data.frame")  # Check if the result is of class "data.frame"
  
})

# check error
test_that("decomp_cj.swmpr returns incorrect parameters error", {
  
  # Perform assertions to check the output
  expect_error(decomp_cj(wq, 'asdf'), 'Selected parameter not in data')
  
})
