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
test <- smoother(dat, window = 50, params = param)

# plot
plot(dat, select = 'depth')
lines(test, select = 'depth', col = 'red', lwd = 2)

##
# test na.approx.swmpr

# get data
swmp1 <- import_local('data/zip_ex', 'apadbwq')

# qaqc, subset
dat <- qaqc(swmp1)
dat <- subset(dat, subset = c('2013-01-22 00:00', '2013-01-26 00:00'))

# interpolate, maxgap of default 10 records
test <- na.approx(dat, params = 'do_mgl', maxgap = 10)

# interpolate maxgap of 30 records
test2 <- na.approx(dat, params = 'do_mgl', maxgap = 30)

# plot for comparison
par(mfrow = c(3, 1))
plot(dat, select = 'do_mgl', main = 'Raw')
plot(test, select = 'do_mgl', col = 'red', main = 'Inteprolation - maximum gap of 10 records')
lines(dat, select = 'do_mgl')
plot(test2, select = 'do_mgl', col = 'red', main = 'Inteprolation - maximum gap of 30 records')
lines(dat, select = 'do_mgl')
