# Unit tests
test_that("calckl calculates KL correctly", {
  temp <- 25
  sal <- 35
  atemp <- 25
  wspd <- 5
  bp <- 1013
  height <- 10
  
  expected_KL <- 1.177
  result <- round(calckl(temp, sal, atemp, wspd, bp, height), 3)
  
  # Check if the result is equal to the expected value
  expect_equal(result, expected_KL)
})
