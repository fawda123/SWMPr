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
tmp <- all_params_dtrng('apaesnut', c('01/01/2013', '08/10/2013'), param = 'CHLA_N')

# get single parameter for number of records back from current date
# returns swmpr object
tmp <- single_param('marabwq', 100, 'DO_mgl')

######
# comprehensive test of data retrieval functions

##
# test 'site_codes'
meta <- site_codes()

##
# test 'site_single'

# random sample to test
samps <- sample(unique(meta$nerr_site_id), 10)

# test, should print w/o errors
for(site in samps) print(site_codes_ind(site))

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
ldply(test, dim)

##
# testing 'all_params_dtrng'
test_dates <- c('04/01/2010', '09/22/2011')

test <- list()
test_rows <- sample(unique(meta$station_code), 10)
for(i in test_rows){
  
  cat(i, 'of', nrow(meta), '\n')
  
  # station name and parameters reported
  stat_meta <- meta[i, ]
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
lapply(test, head)

##
# testing 'single_param'
test <- list()
for(i in 1:nrow(meta)){
  
  cat(i, 'of', nrow(meta), '\n')
  
  # station name and parameters reported
  stat_meta <- meta[i, ]
  stat <- stat_meta$station_code
  params <- strsplit(stat_meta$params_reported, ',')[[1]]
  
  # return default string if no parameters reported
  if(length(params) == 0){
    test[[stat]] <- 'no parameters reported'
    next
    }
  
  # list of single parameter output for a single station
  tmp <- list()
  for(param in params) tmp[[param]] <- single_param(stat, param = param)
  
  # append to list for all stations
  test[[stat]] <- tmp
  
  }
