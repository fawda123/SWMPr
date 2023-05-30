# Test oxySol function
test_that("oxySol calculates oxygen solubility correctly", {
  # Test case 1: No pressure specified
  t1 <- 25  # Temperature in degrees Celsius
  S1 <- 35  # Salinity
  
  # Expected result
  expected1 <- 6.772116
  
  # Call the oxySol function
  result1 <- oxySol(t1, S1)
  
  # Perform assertions
  expect_equal(result1, expected1, tolerance = 1e-5)
  
  
  # Test case 2: Pressure specified
  t2 <- 15  # Temperature in degrees Celsius
  S2 <- 25  # Salinity
  P2 <- 2   # Pressure in atm
  
  # Expected result (calculated using external reference)
  expected2 <- 17.43224
  
  # Call the oxySol function
  result2 <- oxySol(t2, S2, P2)
  
  # Perform assertions
  expect_equal(result2, expected2, tolerance = 1e-5)
  

})