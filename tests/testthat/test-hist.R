dat <- subset(apadbwq, select = 'do_mgl')

test_that("hist.swmpr plots histogram for single parameter", {
  expect_silent(hist(dat))
})

test_that("hist.swmpr raises an error for multiple parameters", {
  expect_error(hist(apadbwq), "Only one parameter can be plotted, use subset first")
})