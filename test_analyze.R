#######
# simple test of data analysis functions

##
# test aggregate

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
test <- aggregate(swmpr_in, 'week')
test <- aggregate(swmpr_in, '%d', params = c('do_mgl', 'atemp'))
