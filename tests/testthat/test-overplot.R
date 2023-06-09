test_that("Check overplot class", {

  # Perform assertions on the plot object
  expect_type(overplot(apacpwq), 'list')
  expect_type(overplot(wq, cols = 'red'))
  expect_type(overplot(wq, lwd = 2, lty = 2, pch = 2, type = 'l'), 'list')

})

test_that("Check overplot class with additional arguments", {

  # Perform assertions on the plot object
  expect_type(overplot(wq, select = c("do_mgl", "do_pct"), 
                       subset = c('2013-01-01 0:0', '2013-05-01 0:0'), 
                       ylabs = c('DO (mg/L)', 'DO (%)')), 'list')

})

test_that("Check overplot error if date_var class incorrect", {
  
  expect_error(overplot.default(wq, date_var = 'temp'), 'date_var must be POSIXct class')
  
})

test_that("Check errors for argument inputs to overplot", {
  
  expect_error(overplot(wq, lwd = c(1, 2, 3)), 'lwd must have length equal to 1 or variables to select')
  expect_error(overplot(wq, lty = c(1, 2, 3)), 'lty must have length equal to 1 or variables to select')
  expect_error(overplot(wq, pch = c(1, 2, 3)), 'pch must have length equal to 1 or variables to select')
  expect_error(overplot(wq, type = c(1, 2, 3)), 'type must have length equal to 1 or variables to select')
  
})
  