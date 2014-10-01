# comb dat function

dir('zip_ex')

swmp1 <- import_local('zip_ex', 'apacpnut')
swmp2 <- import_local('zip_ex', 'apacpwq')

# 'timestep' numeric value of time step to use in minutes
# 'differ' is buffer for merging time stamps to standardized time series
# 'comb' is chr string indicating method of combining multiple stations
#   values are 'union' - all dates as continuous time series, 
#   'intersect' - areas of overlap, or 'station' name for given station
# NULL if one station
comb_dat <- function(...) UseMethod('comb_dat')
comb_dat.swmpr <- function(..., timestep = 30, differ= 5, comb = NULL){

  library(plyr)
  library(data.table) 
  
  # swmp objects list
  all_dat <- list(...)
  
  ##
  # sanity checks
  if(is.null(comb) & length(all_dat) > 1)
    stop('Value for comb is required for multiple swmpr object')
  if(timestep/2 <= differ) 
    stop('Value for differ must be less than one half of timestep')
  
  ##
  # attributes of input swmpr objects
  attrs <- llply(all_dat, attributes)
  dats <- llply(all_dat, .fun = function(x) x[['station_data']])
  
  # standardize time scale for one swmpr object
  if(length(all_dat) == 1){
    
    ##
    # create continous time vector at given time step and time range
    
    # get attributes
    timezone <- attrs[[1]]$timezone
    dts_std <- attrs[[1]]$date_rng
    
    # round to nearest timestep
    dts_std <- as.POSIXct(
      round(as.double(dts_std)/(timestep * 60)) * (timestep * 60),
      origin = '1970-01-01',
      tz = timezone
      )
    
    # create continuous vector
    dts_std <- seq(dts_std[1], dts_std[2], by = timestep * 60)
    dts_std <- data.frame(datetimestamp = dts_std)
    
    # convert swmpr data and standardized vector to data.table for merge
    # time_dum is vector of original times for removing outside of differ
    mrg_dat <- dats[[1]]
    mrg_dat$time_dum <- mrg_dat$datetimestamp
    mrg_dat <- data.table(mrg_dat, key = 'datetimestamp')
    mrg_std <- data.table(dts_std, key = 'datetimestamp')
    
    # merge all the data  using  mrg_std as master
    mrg <- mrg_dat[mrg_std, roll = 'nearest']
    mrg <- data.frame(mrg)
    
    # set values outside of differ to NA
    time.diff <- abs(difftime(mrg$datetimestamp, mrg$time_dum, units='secs'))
    time.diff <- time.diff >= 60 * differ
    mrg[time.diff, !names(mrg) %in% c('datetimestamp', 'time_dum')] <- NA
    
    out <- data.frame(mrg) # need to remove time_dum after testing
    
  # standardize time scales and combine multiple swmpr objects
  } else {
    
    return(NULL)
    
    }
   
  # convert to swmpr class
  # station can be multiple if combined
  station <- unlist(llply(attrs, .fun = function(x) x[['station']]))
  out <- swmpr(out, station)
  
  return(out)
  
  }


test <- comb_dat(swmp2, timestep = 10, differ = 4)

