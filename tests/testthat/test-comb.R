
# Test case 1: Combining two SWMP objects with default arguments
test_that("comb.swmpr combines two SWMP objects with default arguments", {

  # Call the comb.swmpr function
  result <- comb(wq, met)
  
  # Perform assertions to check the output
  expect_s3_class(result, "swmpr")  # Check if the result is of class "swmpr"
  
  expected_columns <- c("datetimestamp", "temp", "spcond", "sal", "do_pct", "do_mgl", 
                        "depth", "cdepth", "level", "clevel", "ph", "turb", "chlfluor", 
                        "atemp", "rh", "bp", "wspd", "maxwspd", "wdir", "sdwdir", "totpar", 
                        "totprcp", "cumprcp", "totsorad")
  expect_equal(names(result), expected_columns) 
  
})

# Test case 2: Combining three SWMP objects with specified timestep and method
test_that("comb.swmpr combines three SWMP objects with specified timestep and method", {
  
  # Call the comb function with specified arguments
  result <- comb(wq, met, timestep = 30, method = "intersect")
  
  # Perform assertions to check the output
  expect_s3_class(result, "swmpr")  # Check if the result is of class "swmpr"
  
  expected_columns <- c("datetimestamp", "temp", "spcond", "sal", "do_pct", "do_mgl", 
                        "depth", "cdepth", "level", "clevel", "ph", "turb", "chlfluor", 
                        "atemp", "rh", "bp", "wspd", "maxwspd", "wdir", "sdwdir", "totpar", 
                        "totprcp", "cumprcp", "totsorad")
  expect_equal(names(result), expected_columns) 
  
})
