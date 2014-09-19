#######
# simple test of data retrieval functions

# get metadata all sites
meta <- site_codes()
 
# get metadata for single site
tmp <- site_codes_ind('tjr')

# get all parameters for number of records back from current date
# current max is 100
# returns swmpr object
tmp <- all_params('apadbnut')

# get all records within a date range, current max is 1000
# returns swmpr object
tmp <- all_params_dtrng('marabwq', c('09/10/2009', '02/8/2010'))#, 
  param = 'DO_mgl')

# get single parameter for number of records back from current date
# returns swmpr object
tmp <- single_param('marabwq', 100, 'DO_mgl')

######
# comprehensive test of data retrieval functions

##
# test 'site_codes'
meta <- site_codes()

##
# test 'site_codes_ind'

# random sample to test
samps <- sample(unique(meta$nerr_site_id), 10)

# test
test <- list()
for(site in samps) test[[site]] <- site_codes_ind(site)

# check results
lapply(test, head)

##
# test 'all_params'

# random sample to test
samps <- sample(unique(meta$station_code), 10)

test <- list()
for(stat in samps){
  cat(stat, '\t')
  tmp <- all_params(stat)[[1]]
  test[[stat]] <- tmp
  }

# check results
ldply(test, dim)

##
# testing 'all_params_dtrng'
test_dates <- c('10/15/2007', '12/20/2008')

test <- list()
test_rows <- sample(unique(meta$station_code), 10)
for(i in test_rows){
  
  cat(i, '\n')
  
  # station name and parameters reported
  stat_meta <- meta[grep(i, meta$station_code), ]
  stat <- stat_meta$station_code
  params <- strsplit(stat_meta$params_reported, ',')[[1]]
  
  # return default string if no parameters reported
  if(length(params) == 0){
    test[[stat]] <- 'no parameters reported'
    next
    }
  
  # get date range output for a station using a random parameter
  # returns active dates if error
  param <- sample(params, 1)
  tmp <- try(all_params_dtrng(stat, test_dates, param = param))
  if(class(tmp) == 'try-error')  tmp <- stat_meta$active_dates
  
  # append to list for all stations
  test[[stat]] <- tmp
  
  }

# check results
lapply(test, attributes)
lapply(test, function(x) head(x[[1]]))

##
# testing 'single_param'
test <- list()
test_rows <- sample(unique(meta$station_code), 10)
for(i in test_rows){
  
  cat(i, '\n')
  
  # station name and parameters reported
  stat_meta <- meta[grep(i, meta$station_code), ]
  stat <- stat_meta$station_code
  params <- strsplit(stat_meta$params_reported, ',')[[1]]
  
  # return default string if no parameters reported
  if(length(params) == 0){
    test[[stat]] <- 'no parameters reported'
    next
    }
  
  # list of single parameter output for a single station
  param <- sample(params, 1)
  tmp <- single_param(stat, param = param)
  
  # append to list for all stations
  test[[stat]] <- tmp
  
  }

# check results
lapply(test, attributes)
lapply(test, function(x) head(x[[1]]))
