# Test for timezone retrieval
test_that("time_vec returns the correct timezone based on station_code", {
  
  # Act
  result <- time_vec(chr_in =  "01/01/2022 12:00", station_code = 'ace', tz_only = TRUE)
  
  # Assert
  expect_equal(result, "America/Jamaica")
})

# Test for datetime conversion
test_that("time_vec converts character input to POSIXct with correct timezone", {
  # Arrange
  expected <- as.POSIXct("01/01/2022 12:00", tz = "America/Jamaica", format = "%m/%d/%Y %H:%M")
  
  # Act
  result <- time_vec(chr_in = "01/01/2022 12:00", station_code = 'ace')
  
  # Assert
  expect_identical(result, expected)
})