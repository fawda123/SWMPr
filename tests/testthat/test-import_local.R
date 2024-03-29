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

# generate sample met data
sample_metdata <- data.frame(
  datetimestamp = seq.POSIXt(as.POSIXct('2022-01-01'), as.POSIXct('2022-01-10'), by = 'hour'),
  atemp = runif(217, 20, 30),
  rh = runif(217, 0, 10),
  frequency = 60
)
sample_metdata$datetimestamp <- strftime(sample_metdata$datetimestamp, format = '%m/%d/%Y %H:%M')
sample_metdata <- rbind(sample_metdata, sample_metdata[nrow(sample_metdata), ])
sample_metfile <- file.path(test_dir, "apaebmet.csv")
write.csv(sample_metdata, file = sample_metfile, row.names = FALSE)

# generate sample nut data, w/ and w/o collmethd
sample_nutdata <- data.frame(
  datetimestamp = seq.POSIXt(as.POSIXct('2022-01-01'), as.POSIXct('2023-01-10'), by = 'month'),
  po4f = runif(13, 20, 30),
  nh4f = runif(13, 0, 10)
)
sample_nutdata$datetimestamp <- strftime(sample_nutdata$datetimestamp, format = '%m/%d/%Y %H:%M')
sample_nutdata <- rbind(sample_nutdata, sample_nutdata[nrow(sample_nutdata), ])
sample_nutdata2 <- sample_nutdata 
sample_nutdata2$collmethd <- sample(c(1, 2), 14, replace = T)
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
  skip_on_cran()
  zip(paste0(test_dir, '\\apadwq.zip'), sample_file)
  result <- import_local(paste0(test_dir, '\\apadwq'), 'apadbwq', trace = T)
  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), 217)
  expect_s3_class(result, 'swmpr')
})

test_that("import_local returns error if files not found", {
  expect_error(import_local(paste0(test_dir, '\\apadwq'), 'asdfwq', trace = T))
  expect_error(import_local(dirname(sample_file), 'asdfwq', trace = T))
})

test_that("import_local works with downstream functions when keep_qaqcstatus = T",{
  
  nut_in <- import_local('importtest/', 'gndblnut', keep_qaqcstatus = T)
  wq_in <- import_local('importtest/', 'gndblwq', keep_qaqcstatus = T)
  
  result <- suppressWarnings(aggreswmp(nut_in, by = 'years'))
  result <- ncol(result)
  expect_equal(result, 7)
  
  result <- cens_id(nut_in, select = 'nh4f')
  result <- names(result)
  expect_equal(result, c("datetimestamp", "historical", "provisionalplus", "nh4f", "f_nh4f", 
                         "c_nh4f"))
  
  result <- cens_id(nut_in)
  result <- qaqc(result, qaqc_keep = NULL)
  result <- sum(!is.na(result$nh4f))
  expect_equal(result, 170)
  
  result <- cens_id(nut_in)
  result <- qaqc(result, qaqc_keep = '0')
  result <- sum(!is.na(result$chla_n))
  expect_equal(result, 226)
  
  result <- suppressWarnings(comb(nut_in, wq_in, method = 'gndblwq'))
  result <- ncol(result)
  expect_equal(result, 23)
  
  result <- suppressWarnings(decomp_cj(nut_in, param = 'chla_n'))
  expect_s3_class(result, 'ggplot')
  
  datin <- suppressWarnings(na.approx(wq_in, params = 'do_mgl', maxgap = 1e6))
  result <- decomp(datin, param = 'do_mgl', frequency = 'daily')
  expect_s3_class(result, "decomposed.ts")
  
  expect_type(overplot(wq_in, select = c('depth', 'do_mgl')), 'list')
  
  result <- suppressWarnings(plot_quants(nut_in, 'chla_n', yr = 2021, yrstart = 2020, yrend = 2021))
  expect_s3_class(result, "ggplot")
  
  result <- suppressWarnings(plot_summary(nut_in, 'chla_n'))
  expect_s3_class(result, 'gtable')
  
  result <- qaqc(nut_in, qaqc_keep = NULL)
  result <- ncol(result)
  expect_equal(result, 9)
  
  result <- qaqc(nut_in)
  result <- ncol(result)
  expect_equal(result, 9)
  
  result <- qaqcchk(wq_in)
  expect_s3_class(result, 'data.frame')
  
  result <- suppressWarnings(rem_reps(nut_in))
  result <- nrow(result)
  expect_equal(result, 43)
 
  result <- setstep(nut_in, timestep = 'years')
  result <- nrow(result)
  expect_equal(result, 3)
  
  result <- suppressWarnings(smoother(wq_in, window = 50, params = 'do_mgl'))
  expect_equal(nrow(result), nrow(wq_in))
               
  result <- subset(wq_in, select = 'do_mgl')
  result <- names(result)
  expect_equal(result, c('datetimestamp', 'do_mgl', 'f_do_mgl'))
  
  result <- subset(nut_in, select = 'chla_n')
  result <- names(result)
  expect_equal(result, c('datetimestamp', 'chla_n', 'f_chla_n'))
  
})

# Cleanup: Remove the temporary directory
unlink(test_dir, recursive = TRUE)
