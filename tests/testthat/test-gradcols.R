test_that("gradcols returns correct colors", {
  
  # Test 1: No color vector provided, should return default colors
  result <- gradcols()
  expect_equal(result, RColorBrewer::brewer.pal(11, 'Spectral'))
  
  # Test 2: Valid color vector provided, should return corresponding colors
  col_vec <- "Blues"
  result <- gradcols(col_vec)
  expect_equal(result, RColorBrewer::brewer.pal(9, "Blues"))
  
  # Test 3: Invalid color vector provided, should return the original color vector
  col_vec <- "InvalidColor"
  result <- gradcols(col_vec)
  expect_equal(result, col_vec)
  
})