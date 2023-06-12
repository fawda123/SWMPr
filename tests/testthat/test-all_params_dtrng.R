test_that("Error for all_params_dtrng no registration", {
  expect_error(all_params_dtrng('apaebwq', c('01/01/2013', '02/01/2013'),
                                param = 'do_mgl'))
})


