test_that("Check overplot class", {

  # Perform assertions on the plot object
  expect_type(overplot(wq), 'list')

})

test_that("Check overplot class with additional arguments", {

  # Perform assertions on the plot object
  expect_type(overplot(wq, select = c("do_mgl", "do_pct"), 
                       subset = c('2013-01-01 0:0', '2013-05-01 0:0'), 
                       ylabs = c('DO (mg/L)', 'DO (%)')), 'list')

})
