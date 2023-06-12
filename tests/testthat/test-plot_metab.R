
# Test case for valid swmpr object and default arguments
test_that("plot_metab returns a ggplot object for a valid swmpr object with default arguments", {
  
  plot <- plot_metab(resmet)
  expect_s3_class(plot, "ggplot")
  
  plot <- plot_metab(resmetgrms)
  expect_s3_class(plot, "ggplot")  
  
})

# Test case for valid swmpr object and custom arguments
test_that("plot_metab returns a ggplot object for a valid swmpr object with custom arguments", {
  
  plot <- plot_metab(resmet, by = 'weeks', alpha = 0.01, width = 5, pretty = FALSE)
  expect_s3_class(plot, "ggplot")
  
  plot <- plot_metab(resmet, by = 100, width = 5, pretty = FALSE)
  expect_s3_class(plot, "ggplot")
  
})

# Test case for swmpr object without metabolism data
test_that("plot_metab throws an error for a swmpr object without metabolism data", {

  expect_error(plot_metab(wq), "No metabolism data, use the ecometab function")
  
})