#' Ecosystem metabolism
#' 
#' Estimate ecosystem metabolism using the Odum open-water method.  Estimates of daily integrated gross production, total respiration, and net ecosystem metabolism are returned.
#' 
#' @param dat_in Input data object, see details for required time series
#' @param tz chr string for timezone, e.g., 'America/Chicago'
#' @param lat numeric for latitude
#' @param long numeric for longitude (negative west of prime meridian)
#' @param depth_val numeric value for station depth (m) if time series is not available
#' @param metab_units chr indicating desired units of output for oxygen, either as mmol or grams
#' @param trace logical indicating if progress is shown in the console
#' @param ... arguments passed to other methods
#' 
#' @return A \code{data.frame} of daily integrated metabolism estimates is returned. If a \code{\link{swmpr}} object is passed to the function, this \code{data.frame} is added to the \code{metab} attribute and the original object is returned.  See the examples for retrieval from a \code{swmpr} object.  The metabolism \code{data.frame} contains the following:  
#' \describe{
#'  \item{\code{date}}{The metabolic day, defined as the 24 hour period starting at sunrise (calculated using \code{\link{metab_day}})}
#'  \item{\code{DOF_d}}{Mean DO flux during day hours, mmol m-2 hr-1. Day hours are calculated using the \code{\link{metab_day}} function.}
#'  \item{\code{D_d}}{Mean air-sea gas exchange of DO during day hours, mmol m-2 hr-1}
#'  \item{\code{DOF_n}}{Mean DO flux during night hours, mmol m-2 hr-1}
#'  \item{\code{D_n}}{Mean air-sea gas exchange of DO during night hours, mmol m-2 hr-1}
#'  \item{\code{Pg}}{Gross production, mmol m-2 d-1, calculated as ((DOF_d - D_d) - (DOF_n - D_n)) * day hours}
#'  \item{\code{Rt}}{Total respiration, mmol m-2 d-1, calculated as (DOF_n - D_n) * 24}
#'  \item{\code{NEM}}{Net ecosytem metabolism, mmol m-2 d-1, calculated as Pg + Rt}
#' }
#' 
#' @import oce
#' 
#' @importFrom stats aggregate
#' 
#' @concept analyze
#'
#' @export
#'
#' @details 
#' Input data include both water quality and weather time series, which are typically collected with independent instrument systems.  For SWMP data, this requires merging water quality and meteorology \code{swmpr} data objects using the \code{\link{comb}} function (see examples).  For the default method not using SWMP data, the input \code{data.frame} must have columns named \code{datetimestamp} (date/time column, as \code{\link[base]{POSIXct}} object), \code{do_mgl} (dissolved oxygen, mg/L), \code{depth} (depth, m), \code{atemp} (air temperature, C), \code{sal} (salinity, psu), \code{temp} (water temperature, C), \code{wspd} (wind speed, m/s), and \code{bp} (barometric pressure, mb). 
#' 
#' The open-water method is a common approach to quantify net ecosystem metabolism using a mass balance equation that describes the change in dissolved oxygen over time from the balance between photosynthetic and respiration processes, corrected using an empirically constrained air-sea gas diffusion model (see Ro and Hunt 2006, Thebault et al. 2008).  The diffusion-corrected DO flux estimates are averaged separately over each day and night of the time series. The nighttime average DO flux is used to estimate respiration rates, while the daytime DO flux is used to estimate net primary production. To generate daily integrated rates, respiration rates are assumed constant such that hourly night time DO flux rates are multiplied by 24. Similarly, the daytime DO flux rates are multiplied by the number of daylight hours, which varies with location and time of year, to yield net daytime primary production. Respiration rates are subtracted from daily net production estimates to yield gross production rates.  The metabolic day is considered the 24 hour period between sunsets on two adjacent calendar days.
#' 
#' Areal rates for gross production and total respiration are based on volumetric rates normalized to the depth of the water column at the sampling location, which is assumed to be well-mixed, such that the DO sensor is reflecting the integrated processes in the entire water column (including the benthos).  Water column depth is calculated as the mean value of the depth variable across the time series in the \code{\link{swmpr}} object.  Depth values are floored at one meter for very shallow stations and 0.5 meters is also added to reflect the practice of placing sensors slightly off of the bottom. A user-supplied depth value can also be passed to the \code{depth_val} argument, either as a single value that is repeated or as a vector equal in length to the number of rows in the input data.  An accurate depth value should be used as this acts as a direct scalar on metabolism estimates.
#' 
#' The air-sea gas exchange model is calibrated with wind data either collected at, or adjusted to, wind speed at 10 m above the surface. The metadata should be consulted for exact height.  The value can be changed manually using a \code{height} argument, which is passed to \code{\link{calckl}}.
#' 
#' A minimum of three records are required for both day and night periods to calculate daily metabolism estimates.  Occasional missing values for air temperature, barometric pressure, and wind speed are replaced with the climatological means (hourly means by month) for the period of record using adjacent data within the same month as the missing data.
#' 
#' All DO calculations within the function are done using molar units (e.g., mmol O2 m-3).  The output can be returned as mass units by setting \code{metab_units = 'grams'} (i.e., 1mol = 32 g O2, which multiplies all estimates by 32 g mol-1/1000 mg/g).  Input data must be in standard mass units for DO (mg L-1).
#' 
#' The specific approach for estimating metabolism with the open-water method is described in Caffrey et al. 2013 and references cited therein.
#' 
#' @references 
#' Caffrey JM, Murrell MC, Amacker KS, Harper J, Phipps S, Woodrey M. 2013. Seasonal and inter-annual patterns in primary production, respiration and net ecosystem metabolism in 3 estuaries in the northeast Gulf of Mexico. Estuaries and Coasts. 37(1):222-241.
#' 
#' Odum HT. 1956. Primary production in flowing waters. Limnology and Oceanography. 1(2):102-117.
#' 
#' Ro KS, Hunt PG. 2006. A new unified equation for wind-driven surficial oxygen transfer into stationary water bodies. Transactions of the American Society of Agricultural and Biological Engineers. 49(5):1615-1622.
#' 
#' Thebault J, Schraga TS, Cloern JE, Dunlavey EG. 2008. Primary production and carrying capacity of former salt ponds after reconnection to San Francisco Bay. Wetlands. 28(3):841-851.
#' 
#' @seealso 
#' \code{\link{calckl}} for estimating the oxygen mass transfer coefficient used with the air-sea gas exchange model, \code{\link{comb}} for combining \code{swmpr} objects, \code{\link{metab_day}} for identifying the metabolic day for each observation in the time series, \code{\link{plot_metab}} for plotting the results, and \code{\link{aggremetab}} for aggregating the metabolism attribute.
#' 
#' @examples
#' \dontrun{
#' ## import water quality and weather data, qaqc
#' data(apadbwq)
#' data(apaebmet)
#' wq <- qaqc(apadbwq)
#' met <- qaqc(apaebmet)
#' 
#' ## combine
#' dat <- comb(wq, met)
#' 
#' ## output units in grams of oxygen
#' res <- ecometab(dat, metab_units = 'grams')
#' attr(res, 'metabolism')
#' 
#' ## manual input of integration depth
#' ## NA values must be filled
#' dat_fill <- na.approx(dat, params = 'depth', maxgap = 1e6)
#' depth <- dat_fill$depth
#' res <- ecometab(dat, metab_units = 'grams', depth_val = depth)
#' attr(res, 'metabolism')
#' 
#' ## use the default method for ecometab with a generic data frame
#' ## first recreate a generic object from the sample data
#' cols <- c('datetimestamp', 'do_mgl', 'depth', 'atemp', 'sal', 'temp', 'wspd', 'bp')
#' dat <- data.frame(dat)
#' dat <- dat[, cols]
#' res <- ecometab(dat, tz = 'America/Jamaica', lat = 29.67, long = -85.06)
#' res
#' }
ecometab <- function(dat_in, ...) UseMethod('ecometab')

#' @rdname ecometab
#' 
#' @export
#' 
#' @method ecometab swmpr
ecometab.swmpr <- function(dat_in, depth_val = NULL, metab_units = 'mmol', trace = FALSE, ...){
  
  stat <- attr(dat_in, 'station')
  
  # stop if units not mmol or grams
  if(any(!(grepl('mmol|grams', metab_units))))
    stop('Units must be mmol or grams')
  
  # stop if input data does not include wq and met
  if(sum(grepl('wq$|met$', stat)) != 2)
    stop('Requires water quality and weather data')
  stat <- grep('wq$', stat, value = T)
  
  # station locations
  dat_locs <- get('stat_locs')
  stat_meta <- dat_locs[grep(gsub('wq$', '', stat), dat_locs$station_code),]
  
  # all times are standard - no DST!
  gmt_tab <- data.frame(
    gmt_off=c(-4, -5, -6, -8, -9),
    tz = c('America/Virgin', 'America/Jamaica', 'America/Regina',
           'Pacific/Pitcairn', 'Pacific/Gambier'),
    stringsAsFactors = F
  )
  
  # lat, long, tz from stat_meta
  lat <- stat_meta$latitude
  long <- stat_meta$longitude
  gmt_off <- stat_meta$gmt_off
  tz <- gmt_tab[gmt_tab$gmt_off == gmt_off, 'tz']
  
  # pass args to default method
  out <- ecometab.default(dat_in, tz = tz, lat = lat, long = long, depth_val = depth_val, 
                          metab_units = metab_units, trace = trace, ...)
  
  # append to metabolism attribute
  attr(dat_in, 'metabolism') <- out
  attr(dat_in, 'metab_units') <- metab_units
  
  return(dat_in)
  
}

#' @rdname ecometab
#'
#' @export
#' 
#' @method ecometab default
ecometab.default <- function(dat_in, tz, lat, long, depth_val = NULL, metab_units = 'mmol', trace = FALSE, ...){
  
  # start timer
  if(trace){
    tictoc::tic()
    cat(paste0('Estimating ecosystem metabolism...\n\n'))
  }
  
  # check for correct columns
  to_keep <- c('datetimestamp', 'do_mgl', 'depth', 'atemp', 'sal', 'temp', 
               'wspd', 'bp')
  nm_chk <- to_keep %in% names(dat_in)
  if(sum(nm_chk) != length(to_keep))
    stop('Column names are incorrect, missing ', paste(to_keep[!nm_chk], collapse = ', ')) 
  
  # if 'depth_val' is provided, replace depth with the constant or supplied vector
  if(!is.null(depth_val)){
    
    # checks if vector supplied
    chkln <- length(depth_val) > 1
    if(any(is.na(depth_val)) & chkln)  
      stop('NA values for depth_val not permitted')
    if(length(depth_val) != nrow(dat_in) & chkln) 
      stop('depth_val must have length equal to input data if not a constant')
    
    dat_in$depth <- depth_val   
    
  }
  
  # columns to keep
  dat <- data.frame(dat_in)
  dat <- dat[,names(dat) %in% to_keep]
  
  # all vals as numeric
  dat[, 2:ncol(dat)] <- apply(
    dat[, 2:ncol(dat), drop = FALSE],
    2,
    function(x) suppressWarnings(as.numeric(x))
  )
  
  #convert do from mg/L to mmol/m3
  dat$do <- dat[, 'do_mgl'] / 32 * 1000
  
  # get change in do per hour, as mmol m^-3 hr^-1
  # difference between time steps, divided by time (in mins) between time steps, multiplied by 60
  ddo <- with(dat, diff(do) / as.double(diff(datetimestamp), units = 'hours'))
  
  # take diff of each column, divide by 2, add original value
  datetimestamp <- diff(dat$datetimestamp)/2 + dat$datetimestamp[-c(nrow(dat))]
  dat <- apply(
    dat[,2:ncol(dat), drop = FALSE],
    2,
    function(x) diff(x)/2 + x[1:(length(x) -1)]
  )
  dat <- data.frame(datetimestamp, dat)
  do <- dat$do
  
  ##
  # replace missing wx values with climatological means
  # only atemp, wspd, and bp
  
  # monthly and hourly averages
  months <- as.character(format(dat$datetimestamp, '%m'))
  hours <- as.character(format(dat$datetimestamp, '%H'))
  clim_means <-dplyr:: mutate(dat, months = months, hours = hours) 
  clim_means <- clim_means[, c('months', 'hours', 'atemp', 'wspd', 'bp')]
  clim_means <- reshape2::melt(clim_means, 
                               measure.vars = c('atemp', 'wspd', 'bp')
  )
  clim_means <- dplyr::group_by(clim_means, 'months', 'hours', 'variable')
  clim_means <- aggregate(value ~ months + hours + variable, clim_means, 
                          FUN = mean, na.rm = T)
  clim_means <- tidyr::spread(clim_means, 'variable', 'value')
  
  # merge with original data
  to_join <- data.frame(datetimestamp = dat$datetimestamp, months, 
                        hours, stringsAsFactors = FALSE) 
  clim_means <- dplyr::left_join(
    to_join,
    clim_means, by = c('months','hours')
  )
  clim_means <- clim_means[order(clim_means$datetimestamp),]
  
  # datetimestamp order in dat must be ascending to match
  if(is.unsorted(dat$datetimestamp))
    stop('datetimestamp is unsorted')
  
  # reassign empty values to means, objects are removed later
  atemp_mix <- dat$atemp
  wspd_mix <- dat$wspd
  bp_mix <- dat$bp
  atemp_mix[is.na(atemp_mix)] <- clim_means$atemp[is.na(atemp_mix)]
  wspd_mix[is.na(wspd_mix)] <- clim_means$wspd[is.na(wspd_mix)]
  bp_mix[is.na(bp_mix)] <- clim_means$bp[is.na(bp_mix)]
  
  ##
  # dosat is do at saturation given temp (C), salinity (st. unit), and press (atm, mb * 1/1013.25)
  # dosat as proportion
  # used to get loss of O2 from diffusion
  dosat <- with(dat, do_mgl/oxySol(temp, sal, bp * 1/1013.25))
  
  # station depth, defaults to mean depth value if not provided, floored at 1 in case not on bottom
  # uses 'depth_val' if provided, either as a repeated single value or supplied vector
  if(is.null(depth_val)){
    
    H <- rep(0.5 + mean(pmax(1, dat$depth), na.rm = T), nrow(dat))
    
  } else {  
    
    H <- dat$depth
    
  }
  
  #use metab_day to add columns indicating light/day, date, and hours of sunlight
  dat <- metab_day(dat, tz = tz, lat = lat, long = long)
  
  #get air sea gas-exchange using wx data with climate means
  KL <- with(dat, calckl(temp, sal, atemp_mix, wspd_mix, bp_mix, ...))
  rm(list = c('atemp_mix', 'wspd_mix', 'bp_mix'))
  
  #get volumetric reaeration coefficient from KL 
  Ka <- KL / 24 / H
  
  #get exchange at air water interface
  D = Ka * (do / dosat - do)
  
  #combine all data for processing
  proc_dat <- dat[, names(dat) %in% c('metab_date', 'solar_period', 'day_hrs')]
  proc_dat <- data.frame(proc_dat, ddo, H, D)
  
  #get daily/nightly flux estimates for Pg, Rt, NEM estimates
  out <- lapply(
    split(proc_dat, proc_dat$metab_date),
    function(x){
      
      #filter for minimum no. of records 
      if(length(with(x[x$solar_period == 'sunrise', ], na.omit(ddo))) < 3 |
         length(with(x[x$solar_period == 'sunset', ], na.omit(ddo))) < 3 ){
        DOF_d <- NA; D_d <- NA; DOF_n <- NA; D_n <- NA
        
      } else {
        #day
        DOF_d <- mean(with(x[x$solar_period == 'sunrise', ], ddo * H), na.rm = T)
        D_d <- mean(with(x[x$solar_period == 'sunrise', ], D * H), na.rm = T)
        
        #night
        DOF_n <- mean(with(x[x$solar_period == 'sunset', ], ddo * H), na.rm = T)
        D_n <- mean(with(x[x$solar_period == 'sunset', ], D * H), na.rm = T)
        
      }
      
      # metabolism
      # account for air-sea exchange if surface station
      # else do not
      Pg <- ((DOF_d - D_d) - (DOF_n - D_n)) * unique(x$day_hrs)
      Rt <- (DOF_n - D_n) * 24
      NEM <- Pg + Rt
      Pg_vol <- Pg / mean(x$H, na.rm = T)
      Rt_vol <- Rt / mean(x$H, na.rm = T)
      
      # output
      data.frame(date = unique(x$metab_date), 
                 DOF_d, D_d, DOF_n, D_n, Pg, Rt, NEM
      )
      
    }
  )
  
  out <- do.call('rbind',out)
  row.names(out) <- 1:nrow(out)
  
  # change units to grams
  if('grams' %in% metab_units){
    
    # convert metab data to g m^-2 d^-1
    # 1mmolO2 = 32 mg O2, 1000mg = 1g, multiply by 32/1000
    as_grams <- apply(out[, -1], 2, function(x) x * 0.032)
    out <- data.frame(date = out[, 'date'], as_grams)
    
  }
  
  if(trace) tictoc::toc()
  
  return(out)
  
}