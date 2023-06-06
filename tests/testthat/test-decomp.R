# Test decomp with valid inputs
test_that("decomp decomposes the input data correctly", {
  
  # Call the function
  result <- decomp(dcmdat, param = 'do_mgl', frequency = 'daily')
  
  # Check if the result is a decompose object
  expect_s3_class(result, "decomposed.ts")
  
  # use na.approx to interpolate missing data  
  datin <- na.approx(wq, params = 'do_mgl', maxgap = 1e6)

  # Call the function
  result <- decomp(datin, param = 'do_mgl', frequency = 'annual')
  
  # Check if the result is a decompose object
  expect_s3_class(result, "decomposed.ts")
  
})

# Test decomp with invalid parameter name
test_that("decomp throws an error for invalid parameter name", {

  # Call the function with an invalid parameter name
  expect_error(decomp(dcmdat, param = "param"), "Params argument must name input columns")

  # default method
  expect_error(decomp(data.frame(dcmdat), param = "param"), "Params argument must name input columns")
  
})

test_that("decomp throws an error with incorrect frequency or start argument", {
  
  expect_error(decomp(dcmdat, param = "do_mgl", frequency = 'asdf'), "Chr string input for frequency must be 'daily' or 'annual'")
  expect_error(decomp(dcmdat, param = "do_mgl", frequency = 96), "Start argument required if frequency is numeric")
  
})

# Test decomp with non-standardized time step
test_that("decomp throws an error for non-standardized time step", {
  
  # Call the function
  expect_error(decomp(apacpnut, param = "chla_n"), "The time step is not standardized, use setstep")
  
})


