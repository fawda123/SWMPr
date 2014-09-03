#######
# simple test of data retrieval functions

# get metadata all stations
meta <- stat_codes()
 
# get metadata for single station
tmp <- stat_code('acebbnut')

# get all parameters for number of records back from current date
# current max is 100
tmp <- all_params('apadbnut')

# get all records within a date range, current max is 1000
tmp <- all_params_dtrng('apaesnut', c('01/01/2013', '08/10/2013'))

# get single parameter for number of records back from current date
tmp <- single_param('acebbnut', 100, 'CHLA_N')

######
# comprehensive test of data retrieval functions

meta <- stat_codes()

# testing metadata for individual stations
for(stat in meta$station_code) print(stat_code(stat))
  
# testing metadata for individual stations
test <- list()
for(stat in meta$station_code){
  cat(stat, '\t')
  test[[stat]] <- all_params(stat)
  }
ldply(test, dim)

# testing date range

# testing single param
test <- list()
for(i in 1:nrow(meta)){
  
  cat(i, 'of', nrow(meta), '\n')
  
  # station name and parameters reported
  stat_meta <- meta[i, ]
  stat <- stat_meta$station_code
  params <- strsplit(stat_meta$params_reported, ',')[[1]]
  
  # return string if no parameters reported
  if(length(params) == 0){
    test[[stat]] <- 'no parameters reported'
    next
    }
  
  # list of single parameter output for a single station
  tmp <- list()
  for(param in params) tmp[[param]] <- single_param(stat, param = param)
  
  # append to list for all staitons
  test[[stat]] <- tmp
  
  }
