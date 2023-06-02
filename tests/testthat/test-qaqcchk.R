# Test cases for qaqcchk function
test_that("qaqcchk returns correct qaqc information", {

  # Call qaqcchk function
  qaqc_counts <- qaqcchk(apacpwq)
  
  # Check if the qaqc flag counts are correct
  expect_equal(qaqc_counts$f_cdepth[1], 98)
  expect_equal(qaqc_counts$f_level[2], 70176)
  expect_s3_class(qaqc_counts, 'data.frame')
  
})

# Test error if no qaqc columns
test_that("qaqcchk returns error if no qaqc columns", {
  
  expect_error(qaqcchk(wq), 'No qaqc columns in input data')
  
})
