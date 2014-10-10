#######
# simple test of data analysis functions

##
# test aggregate.swmpr

# local data
# get nuts, wq, and met data as separate objects for the same station
# note that most sites usually have one weather station
swmp1 <- import_local('zip_ex', 'apacpwq')
swmp2 <- import_local('zip_ex', 'apaebmet')

# combine, qaqc, remove empty columns
dat <- comb(swmp1, swmp2, method = 'union')
dat <- qaqc(dat)
swmpr_in <- subset(dat, rem_cols = T)

# test function
fun_in <- function(x) var(x, na.rm = T)
test <- aggregate(swmpr_in, FUN = fun_in, 'hours')
test <- aggregate(swmpr_in, FUN = fun_in, 'quarters', 
  params = c('do_mgl'))

##
# test smoother.swmpr

# get nuts, wq, and met data as separate objects for the same station
swmp1 <- import_local('zip_ex', 'apadbwq')

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
