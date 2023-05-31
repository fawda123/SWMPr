
# Test case for valid swmpr object and default arguments
test_that("plot_quants returns a ggplot object for a valid swmpr object with default arguments", {

  plot <- plot_quants(wq, paramtoplot = "do_mgl", yr = 2013, yrstart = 2012, yrend = 2013)
  
  expect_s3_class(plot, "ggplot")
})

# Test case for valid swmpr object and custom arguments
test_that("plot_quants returns a ggplot object for a valid swmpr object with custom arguments", {
  
  plot <- plot_quants(wq, paramtoplot = "do_mgl", yr = 2013, yrstart = 2012, yrend = 2013, yaxislab = "mg/L", yrcolor = "blue", bgcolor1 = "lightblue", bgcolor2 = "lightcyan", maintitle = "Dissolved oxygen concentrations")
  
  expect_s3_class(plot, "ggplot")
})