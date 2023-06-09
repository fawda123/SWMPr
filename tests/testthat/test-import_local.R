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

# generate sample met data
sample_metdata <- data.frame(
  datetimestamp = seq.POSIXt(as.POSIXct('2022-01-01'), as.POSIXct('2022-01-10'), by = 'hour'),
  atemp = runif(217, 20, 30),
  rh = runif(217, 0, 10),
  frequency = 60
)
sample_metdata <- rbind(sample_metdata, sample_metdata[nrow(sample_metdata), ])
sample_metdata$datetimestamp <- strftime(sample_metdata$datetimestamp, format = '%m/%d/%Y %H:%M')
sample_metfile <- file.path(test_dir, "apaebmet.csv")
write.csv(sample_metdata, file = sample_metfile, row.names = FALSE)

# generate sample nut data, w/ and w/o collmethd
sample_nutdata <- data.frame(
  datetimestamp = seq.POSIXt(as.POSIXct('2022-01-01'), as.POSIXct('2023-01-10'), by = 'month'),
  po4f = runif(13, 20, 30),
  nh4f = runif(13, 0, 10)
)
sample_nutdata$datetimestamp <- strftime(sample_nutdata$datetimestamp, format = '%m/%d/%Y %H:%M')
sample_nutdata2 <- sample_nutdata 
sample_nutdata2$collmethd <- sample(c(1, 2), 13, replace = T)
sample_nutfile <- file.path(test_dir, "apacpnut.csv")
sample_nutfile2 <- file.path(test_dir, "apacp2nut.csv")
write.csv(sample_nutdata, file = sample_nutfile, row.names = FALSE)
write.csv(sample_nutdata2, file = sample_nutfile2, row.names = FALSE)

# Run the unit tests
test_that("import_local imports data correctly", {
  result <- import_local(dirname(sample_file), 'apadbwq', trace = T, keep_qaqcstatus = T)
  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), 217)
  expect_s3_class(result, 'swmpr')
})

test_that("import_local imports met data correctly", {
  result <- import_local(dirname(sample_metfile), 'apaebmet', trace = T)
  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), 217)
  expect_s3_class(result, 'swmpr')
})

test_that("import_local imports nut data correctly", {
  
  # w/o collmethd
  result <- suppressWarnings(import_local(dirname(sample_nutfile), 'apacpnut', trace = T))
  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), 13)
  expect_s3_class(result, 'swmpr')
  
  # w/ collmethd
  result <- import_local(dirname(sample_nutfile2), 'apacp2nut', trace = T)
  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), 13)
  expect_s3_class(result, 'swmpr')
  
})

test_that("import_local raises an error for invalid path", {
  expect_error(import_local("invalid_path.csv", "apadbwq"), "Path does not exist")
})

test_that("import_local raises an error for invalid station_code", {
  expect_error(import_local(dirname(sample_file), "invalid_station"), "station_code must include wq, met, or nut")
})

test_that("import_local imports zip data correctly", {
  result <- import_local(paste0(test_dir, 'apadwq'), 'apadbwq', trace = T)
  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), 217)
  expect_s3_class(result, 'swmpr')
})

test_that("import_local returns error if files not found", {
  expect_error(import_local(paste0(test_dir, 'apadwq'), 'asdfwq', trace = T))
  expect_error(import_local(dirname(sample_file), 'asdfwq', trace = T))
})

# Cleanup: Remove the temporary directory
unlink(test_dir, recursive = TRUE)
