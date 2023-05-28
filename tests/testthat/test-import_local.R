# Create a temporary directory for testing
test_dir <- tempdir()

# Generate a sample CSV file
sample_data <- data.frame(
  datetimestamp = seq.POSIXt(as.POSIXct('2022-01-01'), as.POSIXct('2022-01-10'), by = 'hour'),
  temp = runif(217, 20, 30),
  do_mgl = runif(217, 0, 10)
)
sample_data$datetimestamp <- strftime(sample_data$datetimestamp, format = '%m/%d/%Y %H:%M')
sample_file <- file.path(test_dir, "apadbwq.csv")
write.csv(sample_data, file = sample_file, row.names = FALSE)

zip(paste0(test_dir, 'apadwq.zip'), sample_file)

# Run the unit tests
test_that("import_local imports data correctly", {
  # Test 1: Importing data from a valid path and valid station_code
  result <- import_local(dirname(sample_file), 'apadbwq', trace = T)
  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), 217)
  expect_s3_class(result, 'swmpr')
})

test_that("import_local raises an error for invalid path", {
  # Test 2: Importing data from an invalid path
  expect_error(import_local("invalid_path.csv", "apadbwq"), "Path does not exist")
})

test_that("import_local raises an error for invalid station_code", {
  # Test 3: Importing data with an invalid station_code
  expect_error(import_local(dirname(sample_file), "invalid_station"), "station_code must include wq, met, or nut")
})

test_that("import_local imports zip data correctly", {
  # Test 1: Importing data from a valid path and valid station_code
  result <- import_local(paste0(test_dir, 'apadwq.zip'), 'apadbwq', trace = T)
  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), 217)
  expect_s3_class(result, 'swmpr')
})

# Cleanup: Remove the temporary directory
unlink(test_dir, recursive = TRUE)
