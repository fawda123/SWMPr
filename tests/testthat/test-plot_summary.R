# Test for basic functionality
test_that("plot_summary function works correctly", {
  
  # Call the plot_summary function
  plot <- plot_summary(wq, param = "do_mgl")
  
  # Check if the plot object is of class "gtable"
  expect_s3_class(plot, 'gtable')
  
  # Call the plot_summary function with met
  plot <- plot_summary(met, param = "cumprcp")
  
  # Check if the plot object is of class "gtable"
  expect_s3_class(plot, 'gtable')
  
})

# Test for correct handling of missing values
test_that("plot_summary function handles missing values correctly", {

  # Call the plot_summary function
  plot <- plot_summary(wq, param = "do_mgl", fill = "monoclim")
  
  # Check if the plot object is of class "gtable"
  expect_s3_class(plot, 'gtable')
  
  # Call the plot_summary function
  plot <- plot_summary(wq, param = "do_mgl", fill = "interp")
  
  # Check if the plot object is of class "gtable"
  expect_s3_class(plot, 'gtable')
  
})

# Test for correct selection of years to plot
test_that("plot_summary function selects years correctly", {

  # Call the plot_summary function with specific years
  plot <- plot_summary(wq, param = "do_mgl", years = 2012)
  
  # Check if the plot object is of class "gtable"
  expect_s3_class(plot, 'gtable')
  
})

# Test for correct selection of years to plot
test_that("plot_summary function returns separate plots", {
  
  # Call the plot_summary function with specific years
  plot <- plot_summary(wq, param = "do_mgl", years = 2012, plt_sep = T)
  
  # Check if the plot object is of class "gtable"
  expect_type(plot, 'list')
  
})

# Test for correct selection of years to plot
test_that("plot_summary function returns summary output", {
  
  # Call the plot_summary function with specific years
  plot <- plot_summary(wq, param = "do_mgl", years = 2012, sum_out = T)
  
  # Check if the plot object is of class "gtable"
  expect_type(plot, 'list')
  
})

test_that("plot_summary returns errors for incorrect inputs", {
  
  expect_error(plot_summary(wq, param = "do_mgl", years = c(2011, 2012, 2013)), 'One or two element year vector is required.')
  expect_error(plot_summary(wq, param = 'asdf'), 'param must be included in the data')
  
})
