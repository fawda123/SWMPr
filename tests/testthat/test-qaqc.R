# Test cases for qaqc function
test_that("qaqc correctly removes flagged values", {

  # Call qaqc with default parameters
  qaqc_dat <- qaqc(apacpwq, trace = T)
  
  # Check if qaqc columns are removed
  expect_false("f_do_mgl" %in% colnames(qaqc_dat))
  expect_false("f_ph" %in% colnames(qaqc_dat))
  
})

test_that("qaqc correctly keeps specified flags", {
  
  # Call qaqc with specified flags to keep
  qaqc_dat <- qaqc(apacpwq, qaqc_keep = c("2", "<6>"))
  
  # Check if qaqc columns are removed
  expect_false("f_do_mgl" %in% colnames(qaqc_dat))
  expect_false("f_ph" %in% colnames(qaqc_dat))
  
})

test_that("qaqc returns warning if no qaqc flags", {
  
  expect_warning(qaqc(wq), 'No qaqc columns in input data')

})

test_that("qaqc adds censored columns", {
  
  qaqc_dat <- qaqc(apacpwq, cens_id = T)
  
  # Check if censored columns present
  expect_true("cdepth" %in% colnames(qaqc_dat))
  expect_true("clevel" %in% colnames(qaqc_dat))
  
})
