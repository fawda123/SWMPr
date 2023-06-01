swmpr_in <- subset(apadbwq, select = 'do_mgl',
  subset = c('2013-07-01 00:00', '2013-07-31 00:00'))

# Test if the plot function runs without errors
test_that("plot function runs without errors", {
  expect_null(plot(swmpr_in))
})

# Test if the lines function runs without errors
test_that("lines function runs without errors", {
  plot(swmpr_in)
  expect_null(lines(swmpr_in, col = 'red'))
})

# Test if the plot function throws an error when more than one parameter is present
test_that("plot function throws an error for multiple parameters", {
  expect_error(plot(wq))
})

# Test if the lines function throws an error when more than one parameter is present
test_that("lines function throws an error for multiple parameters", {
  expect_error(lines(wq))
})