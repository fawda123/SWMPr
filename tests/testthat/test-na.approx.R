
test_that("na.approx.swmpr returns the expected output", {

  swmpr_data <- wq
  
  # Call the function
  output <- na.approx(swmpr_data, params = 'do_mgl', maxgap = 1)
  
  # Check if the output has the expected structure
  expect_true(is.data.frame(output))
  expect_true("datetimestamp" %in% names(output))
  expect_true('do_mgl' %in% names(output))
  
})

test_that("na.approx returns warning if qaqc present", {

  swmpr_data <- apadbwq
  
  # Call the function
  expect_warning(na.approx(swmpr_data, params = 'do_mgl', maxgap = 1))

})

test_that("na.approx returns error if param not present", {

  swmpr_data <- wq
  
  # Call the function
  expect_error(na.approx(swmpr_data, params = 'test', maxgap = 1), 'Params argument must name input columns')
  
})
