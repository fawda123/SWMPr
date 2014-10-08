
filter.swmpr <- function(swmpr_in, filter, sides, ...){
  
  
}


# get nuts, wq, and met data as separate objects for the same station
# note that most sites usually have one weather station
swmp1 <- import_local('zip_ex', 'apacpnut')
swmp2 <- import_local('zip_ex', 'apadbwq')

# combine nuts and wq data by union
dat <- comb(swmp1, swmp2, method = 'intersect')
dat <- qaqc(dat)

filts <- rep(1, 5)
sids <- 2
params <- NULL
##
# start function here

# attributes
parameters <- attr(swmpr_in, 'parameters')
station <- attr(swmpr_in, 'station')

if(!is.null(params)) parameters <- params

to_filt <- dat$station_data[1:100, c('datetimestamp', parameters)]
datetimestamp <- to_filt$datetimestamp
to_filt$datetimestamp <- NULL

out <- filter(to_filt, filter = rep(1, 5), sides = sids)
out <- data.frame(datetimestamp, out)
names(out) <- names('datetimetamp', parameters)

out <- swmpr(out, station)

