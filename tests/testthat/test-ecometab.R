
test_that("ecometab returns expected output", {
  
  # Call the ecometab function
  result <- ecometab(dat)
  
  # Check if the result is a data.frame
  expect_s3_class(result, "data.frame")
  
  expect_true("metabolism" %in% names(attributes(result)))
  
  expect_identical(attr(result, "metab_units"), "mmol")
  
  expect_true(all(c("date", "DOF_d", "D_d", "DOF_n", "D_n", "Pg", "Rt", "NEM") %in% names(attr(result, 'metabolism'))))
  
})

test_that("ecometab returns expected output with non-default inputs", {
  
  # Call the ecometab function
  result <- ecometab(dat, trace = T, metab_units = 'grams', depth_val = 5)
  
  # Check if the result is a data.frame
  expect_s3_class(result, "data.frame")
  
  expect_true("metabolism" %in% names(attributes(result)))
  
  expect_identical(attr(result, "metab_units"), "grams")
  
  expect_true(all(c("date", "DOF_d", "D_d", "DOF_n", "D_n", "Pg", "Rt", "NEM") %in% names(attr(result, 'metabolism'))))
  
})

test_that("check ecometab errors", {
  
  expect_error(ecometab(dat, metab_units = 'asdf'), 'Units must be mmol or grams')
  expect_error(ecometab(wq), 'Requires water quality and weather data')
  
  comb_in <- subset(dat, select = c('do_mgl', 'atemp'))
  expect_error(ecometab(comb_in), 'Column names are incorrect, missing depth, sal, temp, wspd, bp')
  
  expect_error(ecometab(dat, depth_val = c(1, 5)), 'depth_val must have length equal to input data if not a constant') 
  expect_error(ecometab(dat, depth_val = sample(c(1, NA), size = nrow(dat), replace = T)), 'NA values for depth_val not permitted')
  
  expect_error(ecometab(dat[sample(1:nrow(dat), nrow(dat)), ]), 'datetimestamp is unsorted')
  
})
