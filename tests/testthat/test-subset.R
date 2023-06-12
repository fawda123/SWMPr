# Define the test cases
test_that("subset.swmpr subsets the swmpr object correctly", {

  subset_obj <- subset(wq, subset = '2013-01-01 0:00', operator = '>=')
  
  # Check if the subset object has the correct number of rows
  expect_equal(nrow(subset_obj), 35040)
  
  # Check if the subset object has the correct column names
  expect_equal(colnames(subset_obj), c("datetimestamp", "temp", "spcond", "sal", "do_pct", "do_mgl", 
                                       "depth", "cdepth", "level", "clevel", "ph", "turb", "chlfluor"
  ))
  
  # Check if the subset object has the correct timezone attribute
  expect_equal(attr(subset_obj, "timezone"), "America/Jamaica")
  
})

test_that("Check error in subset.swmpr if select is incorrect", {
  
  expect_error(subset.swmpr(apacpnut, select = 'test'), 'select argument is invalid: test')
  
})

test_that("Check error if no record match subset criteria", {
  
  expect_error(subset.swmpr(apacpnut, subset = '2015-01-01 0:00', operator = '>='), 'No records matching subset criteria')
  
})

test_that("Check error if no binary operator for one date", {
  
  expect_error(subset.swmpr(apacpnut, subset = '2013-01-01 0:00'), 'Binary operator must be included if only one subset value is provided')
  
})

test_that("Check error if subset input format inccorrect", {
  
  expect_error(subset.swmpr(apacpnut, subset = '2013-01-01'), 'subset must be of format %Y-%m-%d %H:%M')
  
})

test_that("Check rem_rows returns correct output", {
  
  subset_obj <- subset.swmpr(apacpnut, select = 'no2f', rem_rows = T)
  
  expect_equal(nrow(subset_obj), 107)
  
})

test_that("Check rem_rows returns error if no data", {
  
  swmpr_in <- nut
  swmpr_in$no2f <- NA
  
  expect_error(subset.swmpr(swmpr_in, select = 'no2f', rem_rows = T), 'All data removed, select different parameters')
  
})

test_that("Subset for datetimestamp as date", {
  
  swmpr_in <- apacpnut
  swmpr_in$datetimestamp <- as.Date(swmpr_in$datetimestamp)
  attr(swmpr_in, 'stamp_class') <- 'Date'
  subset_obj <- subset(swmpr_in, subset = '2013-01-01 0:00', operator = '>=')
  
  expect_equal(nrow(subset_obj), 16)
  
})

test_that("Check error if all data removed with rem_cols", {
  
  expect_error(subset(apacpwq, rem_cols = T, select = 'level'), 'All data removed, select different parameters')
 
})