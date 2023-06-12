# Define the test cases
test_that("setstep sets the timestep correctly", {

  # Call the function
  out <- setstep(apadbwq, timestep = 120)
  
  # Check if the output has the correct number of rows
  expect_equal(nrow(out), 8773)
  
  # Check if the timestep is set correctly
  timestep <- as.numeric(diff(out$datetimestamp[1:2]))
  expect_equal(timestep, 2)
})

test_that("setstep converts character timestep correctly", {
  # Create sample data
  dat_in <- data.frame(
    datetimestamp = seq(
      as.POSIXct("2021-01-01 00:00:00"),
      as.POSIXct("2021-01-02 00:00:00"),
      by = "5 mins"
    ),
    value = rnorm(289)
  )
  
  # Call the function with character timestep
  out <- setstep.default(dat_in, date_col = "datetimestamp", timestep = "hours")
  
  # Check if the output has the correct number of rows
  expect_equal(nrow(out), 25)
  
  # Check if the timestep is set correctly
  timestep <- as.numeric(diff(out$datetimestamp[1:2]))
  expect_equal(timestep, 1)
})

test_that("Check error if differ arg is not less than or equal one half of timestep", {
  
  expect_error(setstep(apaebmet, timestep = 120, differ = 120), 
               'Value for differ must be less than or equal to one half of timestep')
  
})

test_that("Check error datetimestep is date class", {
  
  dat_in <- apacpnut
  dat_in$datetimestamp <- as.Date(dat_in$datetimestamp)
  
  expect_error(setstep(dat_in), 'Cannot use setstep with date class')
  
})

test_that("Check error timestep character input is incorrect", {
  
  expect_error(setstep(apaebmet, timestep = 'asfd'), 
               'Character input for timestep must be one of of the following: years, quarters, months, weeks, days, hours' )
  
})
