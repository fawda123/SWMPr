test_that("sun_times returns expected structure", {

  dts <- seq(as.Date('2024-01-01'), as.Date('2024-01-10'), by = 'days')
  result <- sun_times(dts, lat = 29.67, long = -85.06, tz = 'America/Jamaica')

  expect_s3_class(result, 'data.frame')
  expect_equal(names(result), c('date', 'sunrise', 'sunset'))
  expect_equal(nrow(result), length(dts))
  expect_equal(result$date, dts)
  expect_s3_class(result$sunrise, 'POSIXct')
  expect_s3_class(result$sunset, 'POSIXct')
  expect_equal(attr(result$sunrise, 'tzone'), 'America/Jamaica')
  expect_equal(attr(result$sunset, 'tzone'), 'America/Jamaica')

})

test_that("sun_times matches reference values", {

  # St. Petersburg, FL, reference values from suncalc::getSunlightTimes, within one minute of NOAA solar calculator
  dts <- as.Date(c('2024-03-20', '2024-06-21', '2024-12-21'))
  result <- sun_times(dts, lat = 27.76, long = -82.54, tz = 'America/New_York')

  expect_equal(
    format(result$sunrise, '%Y-%m-%d %H:%M:%S'),
    c('2024-03-20 07:34:51', '2024-06-21 06:36:19', '2024-12-21 07:18:19')
  )
  expect_equal(
    format(result$sunset, '%Y-%m-%d %H:%M:%S'),
    c('2024-03-20 19:42:41', '2024-06-21 20:30:17', '2024-12-21 17:41:12')
  )

})

test_that("sun_times sunrise and sunset fall on the requested date", {

  dts <- seq(as.Date('2024-01-01'), as.Date('2024-12-31'), by = 'days')

  # eastern and western hemisphere, local timezones
  west <- sun_times(dts, lat = 47.6, long = -122.3, tz = 'America/Los_Angeles')
  east <- sun_times(dts, lat = -33.9, long = 151.2, tz = 'Australia/Sydney')

  for(res in list(west, east)){
    tz <- attr(res$sunrise, 'tzone')
    expect_equal(as.Date(res$sunrise, tz = tz), dts)
    expect_equal(as.Date(res$sunset, tz = tz), dts)
    expect_true(all(res$sunrise < res$sunset))
  }

})

test_that("sun_times day length is longer in summer than winter", {

  dts <- as.Date(c('2024-06-21', '2024-12-21'))

  north <- sun_times(dts, lat = 40, long = -75)
  north_hrs <- as.numeric(difftime(north$sunset, north$sunrise, units = 'hours'))
  expect_gt(north_hrs[1], north_hrs[2])

  south <- sun_times(dts, lat = -40, long = 145)
  south_hrs <- as.numeric(difftime(south$sunset, south$sunrise, units = 'hours'))
  expect_lt(south_hrs[1], south_hrs[2])

  # near twelve hours at the equator
  equator <- sun_times(as.Date('2024-03-20'), lat = 0, long = 0)
  equator_hrs <- as.numeric(difftime(equator$sunset, equator$sunrise, units = 'hours'))
  expect_equal(equator_hrs, 12, tolerance = 0.02)

})

test_that("sun_times returns NA for polar day and night", {

  result <- sun_times(as.Date(c('2024-06-21', '2024-12-21', '2024-03-20')), lat = 75, long = 20)

  expect_true(all(is.na(result$sunrise[1:2])))
  expect_true(all(is.na(result$sunset[1:2])))
  expect_false(is.na(result$sunrise[3]))
  expect_false(is.na(result$sunset[3]))

})

test_that("sun_times accepts character dates", {

  result_chr <- sun_times('2024-06-21', lat = 29.67, long = -85.06)
  result_dt <- sun_times(as.Date('2024-06-21'), lat = 29.67, long = -85.06)

  expect_equal(result_chr, result_dt)

})

test_that("sun_times checks inputs", {

  dt <- as.Date('2024-06-21')

  expect_error(sun_times(dt, lat = 95, long = -85), 'lat must be a single numeric value between -90 and 90')
  expect_error(sun_times(dt, lat = c(29, 30), long = -85), 'lat must be a single numeric value between -90 and 90')
  expect_error(sun_times(dt, lat = '29', long = -85), 'lat must be a single numeric value between -90 and 90')
  expect_error(sun_times(dt, lat = 29, long = -185), 'long must be a single numeric value between -180 and 180')
  expect_error(sun_times(dt, lat = 29, long = NA_real_), 'long must be a single numeric value between -180 and 180')

})
