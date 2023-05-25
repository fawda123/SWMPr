# Create a mock swmpr object for testing
create_mock_swmpr <- function() {
  dat <- data.frame(
    datetimestamp = c("2023-01-01 00:00:00", "2023-01-02 00:00:00"),
    parameter1 = c(10, 15),
    f_parameter1 = c(" B", " A"),
    parameter2 = c(20, 25),
    f_parameter2 = c(" A", " B"),
    parameter3 = c(30, 35),
    f_parameter3 = c(" B", " B")
  )
  attr(dat, "qaqc_cols") <- TRUE
  attr(dat, "station") <- "Station1"
  attr(dat, "parameters") <- c("parameter1", "parameter2", "parameter3")
  swmpr(dat, 'apadbwq')
}

swmpr_obj <- create_mock_swmpr()

# Unit tests
test_that("cens_id.swmpr returns correct results for flag_type = 'both'", {

  flag_type <- "both"
  expected_output <- c('-1', '1')
  
  result <- cens_id.swmpr(swmpr_obj, flag_type)$c_parameter1
  
  expect_equal(result, expected_output)
})

test_that("cens_id.swmpr returns correct results for flag_type = 'above'", {

  flag_type <- "above"
  expected_output <- c(T, F)
  
  result <- cens_id.swmpr(swmpr_obj, flag_type)$c_parameter2
  
  expect_equal(result, expected_output)
})

test_that("cens_id.swmpr returns correct results for flag_type = 'below'", {

  flag_type <- "below"
  expected_output <- c(T, T)
  
  result <- cens_id.swmpr(swmpr_obj, flag_type)$c_parameter3
  
  expect_equal(result, expected_output)
})
