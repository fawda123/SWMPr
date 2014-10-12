#######
# simple test of data analysis functions

##
# test aggregate.swmpr

# local data
# get nuts, wq, and met data as separate objects for the same station
# note that most sites usually have one weather station
swmp1 <- import_local('data/zip_ex', 'apacpwq')
swmp2 <- import_local('data/zip_ex', 'apaebmet')

# combine, qaqc, remove empty columns
dat <- comb(swmp1, swmp2, method = 'union')
dat <- qaqc(dat)
swmpr_in <- subset(dat, rem_cols = T)

# test function
fun_in <- function(x) mean(x, na.rm = T)
test <- aggregate(swmpr_in, FUN = fun_in, 'hours')
test <- aggregate(swmpr_in, FUN = fun_in, 'quarters', 
  params = c('do_mgl', 'atemp'))

##
# test smoother.swmpr

# get nuts, wq, and met data as separate objects for the same station
swmp1 <- import_local('data/zip_ex', 'apadbwq')

# combine, qaqc, remove empty columns
dat <- qaqc(swmp1)
dat <- subset(dat, subset = c('2012-07-09 00:00', '2012-07-24 00:00'))

#filter
param <- c('depth', 'spcond')
plo <- 2
test <- smoother(dat, window = 50, params = param)

# will sort out a better plotting method....
plot(dat$station_data[, 'datetimestamp'], dat$station_data[, param[plo]], 
  type = 'l', xlab = 'Date', ylab = param[plo])
lines(test$station_data[, 'datetimestamp'], test$station_data[, param[plo]], type = 'l', col = 'red',
  lwd = 2)

##
# test na.approx.swmpr

# get data
swmp1 <- import_local('data/zip_ex', 'apadbwq')

# qaqc, subset
dat <- qaqc(swmp1)
dat <- subset(swmp1, subset = c('2013-01-22 00:00', '2013-01-26 00:00'))

# interpolate, maxgap of default 8 records
test <- na.approx(dat, params = 'do_mgl')

# interpolate maxgap of 30 records
test2 <- na.approx(dat, params = 'do_mgl', maxgap = 30)

# plot for comparison
par(mfrow = c(3, 1))
plot(do_mgl ~ datetimestamp, data = dat[[1]], type = 'l')
plot(do_mgl ~ datetimestamp, data = test[[1]], type = 'l', col = 'red')
lines(dat$station_data[, 'datetimestamp'], dat$station_data[, 'do_mgl'], type = 'l')
plot(do_mgl ~ datetimestamp, data = test2[[1]], type = 'l', col = 'red')
lines(dat$station_data[, 'datetimestamp'], dat$station_data[, 'do_mgl'], type = 'l')
