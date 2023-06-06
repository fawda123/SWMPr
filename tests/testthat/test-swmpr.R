# Test for valid input data.frame
test_that("swmpr throws an error if stat_in is not a data.frame", {
  # Arrange
  stat_in <- list(a = 1, b = 2)
  meta_in <- list(c = 3, d = 4)
  
  # Act & Assert
  expect_error(swmpr(stat_in, meta_in), "stat_in must be data.frame")
})

# Test for qaqc_cols attribute
test_that("swmpr sets qaqc_cols to TRUE if 'f_' columns exist", {

  # Act
  result <- swmpr(apadbwq, 'apadbwq')
  
  # Assert
  expect_true(attr(result, 'qaqc_cols'))
  
})

# Test for cens_cols attribute
test_that("swmpr sets cens_cols to FALSE if 'c_' columns exist", {

  # Act
  result <- swmpr(apacpnut, 'apacpnut')
  
  # Assert
  expect_false(attr(result, 'cens_cols'))
  
})

# Test for parameters attribute
test_that("swmpr correctly identifies parameters in stat_in", {
  
  # Act
  result <- swmpr(apadbwq, 'apadbwq')
  
  # Assert
  expect_equal(attr(result, 'parameters'), c("temp", "spcond", "sal", "do_pct", "do_mgl", "depth", "cdepth", 
                                    "level", "clevel", "ph", "turb", "chlfluor"))
})