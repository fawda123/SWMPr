# Test param_names function
test_that("param_names returns correct parameter names", {
  # Test case 1: Nutrient parameter type
  param_type1 <- "nut"
  
  # Expected result
  expected1 <- list(
    nut = c("po4f", "f_po4f", "chla_n", "f_chla_n", "no3f", "f_no3f", "no2f", 
      "f_no2f", "nh4f", "f_nh4f", "no23f", "f_no23f", "ke_n", "f_ke_n", 
      "urea", "f_urea")
  )
  
  # Call the param_names function
  result1 <- param_names(param_type1)
  
  # Perform assertions
  expect_equal(result1, expected1)
  
  # Test case 2: Water quality parameter type
  param_type2 <- "wq"
  
  # Expected result
  expected2 <- list(
    wq = c("temp", "f_temp", "spcond", "f_spcond", "sal", "f_sal", "do_pct", 
           "f_do_pct", "do_mgl", "f_do_mgl", "depth", "f_depth", "cdepth", 
           "f_cdepth", "level", "f_level", "clevel", "f_clevel", "ph", "f_ph", 
           "turb", "f_turb", "chlfluor", "f_chlfluor")
  )
  
  # Call the param_names function
  result2 <- param_names(param_type2)
  
  # Perform assertions
  expect_equal(result2, expected2)
  
  # Test case 3: Meteorological parameter type
  param_type3 <- "met"
  
  # Expected result
  expected3 <- list(met = c("atemp", "f_atemp", "rh", "f_rh", "bp", "f_bp", 
                            "wspd", "f_wspd", "maxwspd", "f_maxwspd", "wdir", "f_wdir", "sdwdir", 
                            "f_sdwdir", "totpar", "f_totpar", "totprcp", "f_totprcp", "cumprcp", 
                            "f_cumprcp", "totsorad", "f_totsorad")
  )
  
  # Call the param_names function
  result3 <- param_names(param_type3)
  
  # Perform assertions
  expect_equal(result3, expected3)
  
  # Test case 4: error if param_type argument is incorrect
  expect_error(param_names('abc'), 'param_type must chr string of nut, wq, or met')
  
})