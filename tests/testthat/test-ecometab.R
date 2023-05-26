
test_that("ecometab returns expected output", {
  
  dat_in <- comb(wq, met)
  
  # Call the ecometab function
  result <- ecometab(dat_in)
  
  # Check if the result is a data.frame
  expect_s3_class(result, "data.frame")
  
  expect_true("metabolism" %in% names(attributes(result)))
  
  expect_identical(attr(result, "metab_units"), "mmol")
  
  expect_true(all(c("date", "DOF_d", "D_d", "DOF_n", "D_n", "Pg", "Rt", "NEM") %in% names(attr(result, 'metabolism'))))
  
})
