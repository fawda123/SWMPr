key <- Sys.getenv('GGMAP_GOOGLE_API_KEY')
register_google(key = key, write = TRUE)

test_that("map_reserve returns a ggplot object", {
  # Call the function
  plot <- map_reserve("apa")
  
  # Check the class of the returned object
  expect_s3_class(plot, "ggplot")
})

test_that("map_reserve generates a plot with correct elements", {
  # Call the function
  plot <- map_reserve("apa")
  
  # Check if the plot contains the expected layers
  expect_true("ggplot" %in% class(plot))
  expect_true("GeomBlank" %in% class(plot$layers[[1]]$geom))
  expect_true("ggproto" %in% class(plot$coordinates))
})

test_that("map_reserve throws an error for invalid nerr_site_id", {
  # Expect an error to be thrown for invalid nerr_site_id
  expect_error(map_reserve("A"), "nerr_site_id must be three characters")
  expect_error(map_reserve("ABCD"), "nerr_site_id must be three characters")
})