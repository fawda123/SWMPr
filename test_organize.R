#######
# simple test of data organize functions

##
# test qaqc

# local data

# import data from a folder with csv files from CDMO
path <- 'C:/Users/mbeck/Desktop/zip_download3'
station_code <- 'owcdrnut'

dat <- import_local(path, station_code, T)
tmp <- qaqc(dat, qaqc_keep = NULL, trace = F)

# remote data

# remote data, all_params
dat <- all_params('sfbfmwq')
tmp <- qaqc(dat)

# remote data, all_params_dtrng
# returns swmpr object
dat <- all_params_dtrng('hudscwq', c('09/10/2012', '02/8/2013'))
tmp <- qaqc(dat)

# remote data, all_params_dtrng
# returns swmpr object
dat <- all_params_dtrng('elkvmwq', c('09/10/2012', '02/8/2013'), 'do_mgl')
tmp <- qaqc(dat)

# remote data, single_param
dat <- single_param('tjrtlmet', 100, 'wspd')
tmp <- qaqc(dat)

##
# test subset.swmpr

# remote data, single_param
dat <- single_param('tjrtlmet', 100, 'wspd')
dat <- subset(dat, subset = c('2014-09-29 22:40', '2014-09-30 10:42'))

# import data
dat <- import_local('zip_ex', 'apaebmet', trace = F) 

# select two parameters
subset(dat, select = c('rh', 'bp'))

# subset records greater than a date
subset(dat, subset = '2013-01-01 0:00', operator = '>=')

# subset records within a date range
subset(dat, subset = c('2012-07-01 6:00', '2012-08-01 18:15'))

# subset records within a date range, two parametrs
subset(dat, subset = c('2012-07-01 6:00', '2012-08-01 18:15'),
  select = c('atemp', 'totsorad'))

# subset records less than a date, one parametr
subset(dat, subset = c('2013-12-4 13:00'),
  select = c('totpar'), operator = '>')

# subset records that contain data
dat <- import_local('zip_ex', 'apacpnut')
dat <- setstep(dat, timestep = 60)
dat <- qaqc(dat)
dat <- subset(dat, rem_empty = T)

##
# test setstep.swmpr

dir('zip_ex')

swmp1 <- import_local('zip_ex', 'apacpnut')
swmp2 <- import_local('zip_ex', 'apacpwq')
swmp3 <- import_local('zip_ex', 'apaebmet')

test1 <- setstep(swmp1, timestep = 60, differ = 30)
test2 <- setstep(swmp2)
test3 <- setstep(swmp3)

##
# test comb.swmpr

dir('zip_ex')

swmp1 <- import_local('zip_ex', 'apacpnut')
swmp2 <- import_local('zip_ex', 'apacpwq')
swmp3 <- import_local('zip_ex', 'apaebmet')

test1 <- comb(swmp1, swmp2, method = 'union')
test2 <- comb(swmp1, swmp2, method = 'intersect')
test3 <- comb(swmp1, swmp2, swmp3, timestep = 120, method = 'apacpnut')
