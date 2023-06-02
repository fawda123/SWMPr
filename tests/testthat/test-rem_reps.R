# Test cases for rem_reps function
test_that("rem_reps removes replicates", {

  # Call rem_reps function
  rem_reps_dat <- rem_reps(nut)
  
  # Check if the replicates are removed and averaged by day
  expect_equal(nrow(rem_reps_dat), 140)
  expect_s3_class(rem_reps_dat, 'swmpr')
  
})

# check rem_reps function
test_that("rem_reps removes replicates with user input function", {
  
  func <- function(x) max(x, na.rm = TRUE)
  rem_reps_dat <- rem_reps(nut, FUN = func)
  
  # Check if the replicates are removed
  expect_equal(nrow(rem_reps_dat), 140)
  expect_s3_class(rem_reps_dat, 'swmpr')
  
})


test_that("rem_reps returns error if not nutrients input", {
  
  # test error
  expect_error(rem_reps(wq), 'Input swmpr object must be nutrient data')
  
})

# rem_reps warning if not nutrient data
test_that("rem_reps returns warning if qaqc columns present", {
  
  # test warning
  expect_warning(rem_reps(apacpnut), 'QAQC columns present, removed from output')
  
})

