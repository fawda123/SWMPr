######
#' QAQC filtering for SWMP data
#' 
#' QAQC filtering for SWMP data obtained from retrieval functions, local and remote
#'
#' @param swmpr_in input swmpr object
#' @param ... arguments passed to or from other methods
#' 
#' @export qaqc
#' 
#' @return Returns a swmpr object with \code{NA} values for records that did not match \code{qaqc_keep}.  QAQC columns are also removed.
#' 
#' @seealso \code{\link{qaqcchk}}
#' 
#' @details
#' The qaqc function is a simple screen to retain values from the data with specified QAQC flags, described online: \url{http://cdmo.baruch.sc.edu/data/qaqc.cfm}. Each parameter in the swmpr data typically has a corresponding QAQC column of the same name with the added prefix 'f_'. Values in the QAQC column specify a flag from -5 to 5. Generally, only data with the '0' QAQC flag should be used, which is the default option for the function. Data that do not satisfy QAQC criteria are converted to \code{NA} values. Additionally, simple filters are used to remove obviously bad values, e.g., wind speed values less than zero or pH values greater than 12. Erroneous data entered as -99 are also removed. Processed data will have QAQC columns removed, in addition to removal of values in the actual parameter columns that do not meet the criteria.
#' 
#' @examples
#' ## get data
#' data(apadbwq)
#' dat <- apadbwq
#' 
#' ## qaqc screen for a swmpr object, retain only '0'
#' qaqc(dat)
#'
#' ## retain all data regardless of flag
#' qaqc(dat, qaqc_keep = NULL)
#' 
#' ## retain only '0' and '-1' flags
#' qaqc(dat, qaqc_keep = c(0, -1))
#' 
qaqc <- function(swmpr_in, ...) UseMethod('qaqc')

#' @rdname qaqc
#' 
#' @param qaqc_keep numeric vector of qaqc flags to keep, default \code{0}
#' @param trace logical for progress output on console, default \code{FALSE}
#' 
#' @export qaqc.swmpr
#' 
#' @method qaqc swmpr
qaqc.swmpr <- function(swmpr_in, 
  qaqc_keep = 0,
  trace = FALSE, ...){
  
  ##
  # sanity checks
  if(!class(qaqc_keep) %in% c('numeric', 'integer', 'NULL'))
    stop('qaqc_keep argument must be numeric or NULL')
  
  ##
  # swmpr data and attributes
  dat <- swmpr_in
  qaqc_cols <- attr(swmpr_in, 'qaqc_cols')
  station <- attr(swmpr_in, 'station')
  parameters <- attr(swmpr_in, 'parameters')
  
  # exit function if no qaqc columns
  if(!qaqc_cols){
    warning('No qaqc columns in input data')
    return(swmpr_in)
    }
  
  ##
  #remove values flagged by QA/QC, see cdmo website for flag numbers

  if(trace) cat('Processing QAQC columns...')
  
  #names of qaqc columns
  qaqc_sel <- grep('f_', names(dat), value = TRUE)
  
  qaqc_rm <- as.numeric(seq(-5,  5))
  qaqc_rm <- qaqc_rm[!qaqc_rm %in% qaqc_keep]
  if(length(qaqc_rm) == 0) qaqc_keep <- NULL
  
  # keep all if qaqc_in is NULL, otherwise process qaqc
  if(is.null(qaqc_keep)){ 
    
    rm_col <- c('datetimestamp', qaqc_sel)
    qaqc <- dat[, !names(dat) %in% rm_col]

  } else {
      
    #matrix of TF values for those that don't pass qaqc
    qaqc_vec <- dat[, names(dat) %in% qaqc_sel, drop = FALSE]
    qaqc_vec <- apply(qaqc_vec, 2, 
      function(x) grepl(paste(qaqc_rm, collapse = '|'), x)
      )
    #replace T values with NA
    #qaqc is corrected
    qaqc_sel <- gsub('f_', '', qaqc_sel)
    qaqc <- dat[, names(dat) %in% qaqc_sel, drop = FALSE]
    qaqc <- data.frame(sapply(
      names(qaqc),
      function(x){
        out <- qaqc[, x]
        out[qaqc_vec[, paste0('f_',x)]] <- NA
        out
        },
      USE.NAMES = TRUE
      ), stringsAsFactors = FALSE)
    
    }
   
  ##
  # addl misc processing
  
  # combine with datetimestamp and append to output list
  out <- data.frame(datetimestamp = dat[,1], qaqc)
    
	# convert columns to numeric, missing converted to NA
	# NA values from qaqc still included as NA
  datetimestamp <- out[, 1]
  nr <- nrow(out)
  nc <- ncol(out) -1
  out <- c(as.matrix(out[, -1]))
  out[is.nan(out)] <- NA
  out[out %in%  c(-Inf, Inf, -99)] <- NA
  out <- matrix(out, nrow = nr, ncol = nc) 
  out <- data.frame(
    datetimestamp = datetimestamp,
    out
    )
  names(out) <- c('datetimestamp', parameters)

  # remove obviously bad values
  out <- within(out, {
    
    try({SpCond[SpCond < 0] <- NA}, silent = T)
    try({Sal[Sal < 0] <- NA}, silent = T)
    try({DO_mgl[DO_mgl < 0] <- NA}, silent = T) 
    try({pH[pH < 0 | pH > 12] <- NA}, silent = T) 
    try({Turb[Turb < 0] <- NA}, silent = T) 
    try({ChlFluor[ChlFluor < 0] <- NA}, silent = T) 
    try({RH[RH < 0] <- NA}, silent = T) 
    try({BP[BP < 0] <- NA}, silent = T) 
    try({WSpd[WSpd < 0] <- NA}, silent = T) 
    try({Wdir[Wdir < 0 | Wdir > 360] <- NA}, silent = T)
    try({SDWDir[SDWDir < 0] <- NA}, silent = T) 
    try({TotPAR[TotPAR < 0] <- NA}, silent = T) 
    try({TotPrcp[TotPrcp < 0] <- NA}, silent = T) 
    try({CumPrcp[CumPrcp < 0] <- NA}, silent = T) 
    try({TotSoRad[TotSoRad < 0] <- NA}, silent = T) 
    try({PO4H[PO4H < 0] <- NA}, silent = T) 
    try({NH4F[NH4F < 0] <- NA}, silent = T) 
    try({NO2F[NO2F < 0] <- NA}, silent = T) 
    try({NO3F[NO3F < 0] <- NA}, silent = T) 
    try({NO23F[NO23F < 0] <- NA}, silent = T) 
    try({CHLA_N[CHLA_N < 0] <- NA}, silent = T)
    
    })

  # create swmpr class
  out <- swmpr(out, station)
  
  # return output
  if(trace) cat('\n\nQAQC processed...')
  return(out)

}

######
#' Summary of QAQC flags in SWMP data
#' 
#' Summary of the number of observations with a given QAQC flag for each parameter column of a swmpr object
#'
#' @param swmpr_in input swmpr object
#' 
#' @export
#' 
#' @import reshape2
#' 
#' @seealso \code{\link{qaqc}}
#' 
#' @return Returns a \code{\link[base]{data.frame}} with columns for swmpr parameters and row counts indicating the number of observations in each parameter assigned to a flag value.
#' 
#' @details
#' Viewing the number of observations for each parameter that are assigned to a QAQC flag may be useful for deciding how to process the data qith qaqc. The \code{qaqcchk} function can be used to view this information. Consult the online documentation for a description of each QAQC flag: \url{http://cdmo.baruch.sc.edu/data/qaqc.cfm}
#' 
#' @examples
#' ## get data
#' data(apadbwq)
#' dat <- apadbwq
#' 
#' ## view the number observations in each QAQC flag
#' qaqcchk(dat)
#' 
qaqcchk <- function(swmpr_in) UseMethod('qaqcchk')

#' @rdname qaqcchk
#' 
#' @export qaqcchk.swmpr
#' 
#' @method qaqcchk swmpr
qaqcchk.swmpr <- function(swmpr_in){
  
  ##
  # sanity checks
  qaqc_cols <- attr(swmpr_in, 'qaqc_cols')

  # exit function if no qaqc columns
  if(!qaqc_cols) stop('No qaqc columns in input data')

  # qaqc flag columns
  qaqc_ind <- grep('^f_', names(swmpr_in))
  qaqc <- swmpr_in[, qaqc_ind]

  # summarize number of qaqc flags by column
  out <- lapply(c(qaqc), table)
  
  # format output as data.frame
  out <- melt(out)
  names(out) <- c('flag', 'count', 'variable')
  out <- tidyr::spread(out, 'variable', 'count')
  out[is.na(out)] <- 0
  
  # return output
  return(out)

}

######
#' Remove replicates in nutrient data
#' 
#' Remove replicates in SWMP nutrient data to keep approximate monthly time step
#' 
#' @param swmpr_in input swmpr object
#' @param FUN function to combine values, defaults to mean
#' @param ... arguments passed to other methods
#' 
#' @export
#' 
#' @return Returns a swmpr object for nutrient data with no replicates.
#' 
#' @seealso \code{\link{qaqc}}
#' 
#' @details
#' Raw nutrient data obtained from the CDMO will usually include replicate samples that are taken within a few minutes of each other.  This function combines nutrient data that occur on the same day.  The \code{datetimestamp} column will always be averaged for replicates, but the actual observations will be combined based on the user-supplied function which defauls to the mean.  Other suggested functions include the \code{\link[stats]{median}}, \code{\link[base]{min}}, or \code{\link[base]{max}}.  The entire function call including treatment of \code{NA} values should be passed to the \code{FUN} argument (see the examples).  The function is meant to be used after \code{\link{qaqc}} processing, although it works with a warning if QAQC columns are present.
#' 
#' @examples
#' ## get nutrient data
#' data(apacpnut)
#' swmp1 <- apacpnut
#' 
#' # remove replicate nutrient data
#' rem_reps(swmp1)
#' 
#' # use different function to aggregate replicates
#' func <- function(x) max(x, na.rm = TRUE)
#' rem_reps(swmp1, FUN = func)
rem_reps <- function(swmpr_in, ...) UseMethod('rem_reps')

#' @rdname rem_reps
#' 
#' @export rem_reps.swmpr
#' 
#' @method rem_reps swmpr
rem_reps.swmpr <- function(swmpr_in, FUN = function(x) mean(x, na.rm = TRUE), ...){
  
  dat <- swmpr_in
  
  ##
  # sanity checks
  station <- attr(dat, 'station')
  qaqc_cols <- attr(dat, 'qaqc_cols')
  timezone <- attr(swmpr_in, 'timezone')
  
  # stop if not nutrients
  if(!any(grepl('nut$', station))) stop('Input swmpr object must be nutrient data')
  
  # remove QAQC if present
  if(qaqc_cols){
    warning('QAQC columns present, removed from output')
    dat <- qaqc(dat, qaqc_keep = NULL)
  }
  
  ##
  # remove reps
  
  # add day column
  dat$day <- as.Date(dat$datetimestamp, tz = timezone)
  
  # average datetimestamp by day
  datetimestamp <- aggregate(datetimestamp ~ day, dat, 
    FUN = function(x) mean(x, na.rm = TRUE))
  
  # aggregate reps
  obs <- suppressWarnings(aggregate(. ~ day, dat[, -1], FUN = FUN, na.action = na.pass))
  obs[is.na(obs)] <- NA
  obs[obs == -Inf] <- NA
  
  # merge and create output
  out <- merge(datetimestamp, obs, by = 'day')
  out <- out[, !names(out) %in% 'day']
  out <- swmpr(out, station)
  
  # return output
  return(out)

}

######
#' Subset a swmpr object
#' 
#' Subset a swmpr object by a date range, parameters, or non-empty values
#'
#' @param x input swmpr object
#' @param subset chr string of form 'YYYY-mm-dd HH:MM' to subset a date range.  Input can be one (requires \code{operator} or two values (a range).
#' @param select chr string of parameters to keep
#' @param operator chr string specifiying binary operator (e.g., \code{'>'}, \code{'<='}) if subset is one date value
#' @param rem_rows logical indicating if rows with no data are removed, default \code{FALSE}
#' @param rem_cols is logical indicating if cols with no data are removed, default \code{FALSE}
#' @param ... arguments passed to other methods
#' 
#' @export subset.swmpr
#' 
#' @method subset swmpr
#' 
#' @return Returns a swmpr object as a subset of the input.  The original object will be returned if no arguments are specified.
#' 
#' @seealso \code{\link[base]{subset}}
#'
#' @details
#' This function is used to subset swmpr data by date and/or a selected parameter. The date can be a single value or as two dates to select records within the range. The former case requires a binary operator input as a character string passed to the argument, such as \code{>} or \code{<}. The subset argument for the date(s) must also be a character string of the format YYYY-mm-dd HH:MM for each element (i.e., `%Y-%m%-%d %H:%M' in POSIX standards). Be aware that an error may be returned using this function if the subset argument is in the correct format but the calendar date does not exist, e.g. \code{'2012-11-31 12:00'}.  The function can also be used to remove rows and columns that do not contain data, which may be useful after processing with other functions.
#' 
#' @examples
#' ## get data
#' data(apaebmet)
#' dat <- apaebmet
#' 
#' ## select two parameters from dat
#' subset(dat, select = c('rh', 'bp'))
#'
#' ## subset records greater than or equal to a date
#' subset(dat, subset = '2013-01-01 0:00', operator = '>=')
#'
#' ## subset records within a date range
#' subset(dat, subset = c('2012-07-01 6:00', '2012-08-01 18:15'))
#'
#' ## subset records within a date range, select two parameters
#' subset(dat, subset = c('2012-07-01 6:00', '2012-08-01 18:15'),
#'    select = c('atemp', 'totsorad'))
#'
#' ## remove rows/columns that do not contain data
#' subset(dat, rem_rows = TRUE, rem_cols = TRUE)
#' 
subset.swmpr <- function(x, subset = NULL, select = NULL, 
  operator = NULL, rem_rows = FALSE, rem_cols = FALSE, ...){
  
  ##
  # swmpr data and attributes
  swmpr_in <- x
  dat <- swmpr_in
  station <- attr(swmpr_in, 'station')
  timezone <- attr(swmpr_in, 'timezone')
  parameters <- attr(swmpr_in, 'parameters')
  qaqc_cols <- attr(swmpr_in, 'qaqc_cols')
  stamp_class <- attr(swmpr_in, 'stamp_class')
    
  ##
  # subset
  
  # create posix object from subset input
  date_sel <- rep(TRUE, nrow(dat)) # created based on subset arg
  if(!is.null(subset)){
    
    subset <- as.POSIXct(subset, format = '%Y-%m-%d %H:%M', tz = timezone)
    
    # convert subset to date if input datetimestamp is date
    if('Date' %in% stamp_class)
      subset <- base::as.Date(subset, tz = timezone)
    
    # exit function of subset input is incorrect format
    if(any(is.na(subset))) 
      stop('subset must be of format %Y-%m-%d %H:%M')
    
    # exit function if operator not provided for one subset value
    if(length(subset) == 1){
      
      if(is.null(operator))
        stop('Binary operator must be included if only one subset value is provided')
      
      date_sel <- outer(dat$datetimestamp, subset, operator)[, 1]
     
    # for two datetime values...
    } else {
      
      date_sel <- with(dat, datetimestamp >= subset[1] & datetimestamp <= subset[2])
      
      }

    }
  
  # exit function if date_sel has no matches
  if(sum(date_sel) == 0)
    stop('No records matching subset criteria')
  
  # columns to select, includes qaqc cols if present
  # all if null
  if(is.null(select)) select <- names(dat)
  else{
    # stop if select not in parameters
    if(any(!select %in% parameters)) stop('select argument is invalid')
    select <- names(dat)[names(dat) %in% c('datetimestamp', select, paste0('f_', select))]
  }
  
  # subset data
  out <- base::subset(data.frame(dat), date_sel, select, ...)
  out <- data.frame(out, row.names = 1:nrow(out))
  
  ##
  # remove empty rows
  if(rem_rows){
    
    # get vector of rows that are empty
    col_sel <- grepl(paste(parameters, collapse = '|'), names(out))
    check_empty <- t(apply(as.matrix(out[, col_sel, drop = FALSE]), 1, is.na))
    if(nrow(check_empty) == 1) check_empty <- matrix(check_empty)
    check_empty <- rowSums(check_empty) == ncol(check_empty)
    
    # remove empty rows
    out <- out[!check_empty, , drop = FALSE]
    
    if(nrow(out) == 0) stop('All data removed, select different parameters')
    
    out <- data.frame(out, row.names = 1:nrow(out))
    
    }
  
  ##
  # remove empty columns
  if(rem_cols){

    # get vector of empty columns
    check_empty <- colSums(apply(out, 2, is.na)) == nrow(out)

    # make sure qaqc columns match parameter columns if present
    if(qaqc_cols){
      
      rem_vals <- check_empty[names(check_empty) %in% parameters]
      check_empty[names(check_empty) %in% paste0('f_', parameters)] <- rem_vals
      
      }
    
    # subset output
    out <- out[, !check_empty, drop = FALSE]
    
    if(ncol(out) == 1) stop('All data removed, select different parameters')
    
    }
  
  # create swmpr class
  out <- swmpr(out, station)
  
  # return output
  return(out)
  
}

######
#' Format a swmpr time vctor
#'
#' Create a continuous time vector at set time step for a swmpr object
#' 
#' @param swmpr_in input swmpr object
#' @param timestep numeric value of time step to use in minutes
#' @param differ numeric value defining buffer for merging time stamps to standardized time series
#' @param ... arguments passed to or from other methods
#' 
#' @export
#' 
#' @import data.table
#' 
#' @return Returns a swmpr object for the specified time step
#' 
#' @seealso \code{\link{comb}}
#' 
#' @details
#' The setstep function formats a swmpr object to a continuous time series at a given time step. This function is not necessary for most stations but can be useful for combining data or converting an existing time series to a set interval.  The first argument of the function, \code{timestep}, specifies the desired time step in minutes starting from the nearest hour of the first observation. The second argument, \code{differ}, specifies the allowable tolerance in minutes for matching existing observations to user-defined time steps in cases where the two are dissimilar. Values for \code{differ} that are greater than one half the value of timestep are not allowed to prevent duplication of existing data. Likewise, the default value for differ is one half the time step. Rows that do not match any existing data within the limits of the differ argument are not discarded. Output from the function can be used with \code{subset} and to create a time series at a set interval with empty data removed.
#' 
#' @examples
#' ## import data
#' data(apaebmet)
#' dat <- apaebmet
#' 
#' ## convert time series to two hour invervals
#' ## tolerance of +/- 30 minutes for matching existing data
#' setstep(dat, timestep = 120, differ = 30)
#' 
#' ## convert a nutrient time series to a continuous time series
#' ## then remove empty rows and columns
#' data(apacpnut)
#' dat_nut <- apacpnut
#' dat_nut <- setstep(dat_nut, timestep = 60)
#' subset(dat_nut, rem_rows = TRUE, rem_cols = TRUE)
setstep <- function(swmpr_in, ...) UseMethod('setstep')

#' @rdname setstep
#' 
#' @export
#' 
#' @method setstep swmpr
setstep.swmpr <- function(swmpr_in, timestep = 15, differ= timestep/2, ...){ 
  
  # swmpr data and attributes
  dat <- swmpr_in
  attrs <- attributes(swmpr_in)
  
  # sanity check
  if(timestep/2 < differ) 
    stop('Value for differ must be less than one half of timestep')
  if('Date' %in% attrs$stamp_class) 
    stop('Cannot use setstep with date class')
  
  # round to nearest timestep
  dts_std <- as.POSIXct(
    round(as.double(attrs$date_rng)/(timestep * 60)) * (timestep * 60),
    origin = '1970-01-01',
    tz = attrs$timezone
    )
    
  # create continuous vector
  dts_std <- seq(dts_std[1], dts_std[2], by = timestep * 60)
  dts_std <- data.frame(datetimestamp = dts_std)
  
  # convert swmpr data and standardized vector to data.table for merge
  # time_dum is vector of original times for removing outside of differ
  mrg_dat <- dat
  mrg_dat$time_dum <- mrg_dat$datetimestamp
  mrg_dat <- data.table::data.table(mrg_dat, key = 'datetimestamp')
  mrg_std <- data.table::data.table(dts_std, key = 'datetimestamp')
  
  # merge all the data  using  mrg_std as master
  mrg <- mrg_dat[mrg_std, roll = 'nearest']
  mrg <- data.frame(mrg)
  
  # set values outside of differ to NA
  time_diff <- abs(difftime(mrg$datetimestamp, mrg$time_dum, units='secs'))
  time_diff <- time_diff >= 60 * differ
  mrg[time_diff, !names(mrg) %in% c('datetimestamp', 'time_dum')] <- NA
  
  # output, back to swmpr object from data.table
  out <- data.frame(mrg)
  out <- out[, !names(out) %in% 'time_dum']
  out <- swmpr(out, attrs$station)
  
  return(out)

} 

######
#' Combine swmpr data
#' 
#' Combine swmpr data types for a station by common time series
#' 
#' @param ... swmpr object input from one to many
#' @param timestep numeric value of time step to use in minutes, passed to \code{setstep}
#' @param differ numeric value defining buffer for merging time stamps to standardized time series, passed to \code{setstep}
#' @param method chr string indicating method of combining (\code{'union'} for all dates as continuous time series, \code{'intersect'} for areas of overlap, or \code{'station'} for date ranges of a given station)
#' 
#' @import data.table
#' 
#' @export 
#' 
#' @return Returns a combined swmpr object
#' 
#' @seealso \code{\link{setstep}}
#' 
#' @details
#' The \code{comb} function is used to combine multiple swmpr objects into a single object with a continuous time series at a given step. The \code{timestep} function is used internally such that \code{timestep} and \code{differ} are accepted arguments for \code{comb}. 
#' 
#' The function requires one or more swmpr objects as input as separate, undefined arguments. The remaining arguments must be called explicitly since an arbitrary number of objects can be used as input. In general, the function combines data by creating a master time series that is used to iteratively merge all swmpr objects. The time series for merging depends on the value passed to the \code{method} argument. Passing \code{'union'} to \code{method} will create a time series that is continuous starting from the earliest date and the latest date for all input objects. Passing \code{'intersect'} to \code{method} will create a time series that is continuous from the set of dates that are shared between all input objects. Finally, a seven or eight character station name passed to \code{method} will merge all input objects based on a continuous time series for the given station. The specified station must be present in the input data. Currently, combining data types from different stations is not possible, excluding weather data which are typically at a single, dedicated station.
#' 
#' @examples
#' 
#' ## get nuts, wq, and met data as separate objects for the same station
#' ## note that most sites usually have one weather station
#' data(apacpnut)
#' data(apacpwq)
#' data(apaebmet)
#' swmp1 <- apacpnut
#' swmp2 <- apacpwq
#' swmp3 <- apaebmet
#' 
#' ## combine nuts and wq data by union
#' comb(swmp1, swmp2, method = 'union')
#' 
#' ## combine nuts and wq data by intersect
#' comb(swmp1, swmp3, method = 'intersect')
#' 
#' ## combine nuts, wq, and met data by nuts time series, two hour time step
#' comb(swmp1, swmp2, swmp3, timestep = 120, method = 'apacpnut')
comb <- function(...) UseMethod('comb')

#' @rdname comb
#' 
#' @export
#' 
#' @method comb swmpr
comb.swmpr <- function(..., timestep = 15, differ= timestep/2, method = 'union'){
  
  # swmp objects list and attributes
  all_dat <- list(...)
  attrs <- lapply(all_dat, attributes)
  
  ##
  # sanity checks
  if(length(all_dat) == 1)
    stop('Input data must include more than one swmpr object')
  
  # stop if from more than one timezone
  timezone <- unique(unlist(lapply(attrs, function(x) x$timezone)))
    
  if(length(timezone) > 1)
    stop('Input data are from multiple timezones')
  
  # stop of method is invalid
  stations <- unlist(lapply(attrs, function(x) x$station))
  
  if(!method %in% c('intersect', 'union', stations))
    stop('Method must be intersect, union, or station name')

  # stop if more than one data type
  types <- unlist(lapply(attrs, function(x) substring(x$station, 6)))
  
  if(any(duplicated(types))) 
    stop('Unable to combine duplicated data types')
  
  ##
  # setstep applied to data before combining
  all_dat <- lapply(all_dat, function(x) setstep(x, timestep, differ))
  
  ##
  # dates
  date_vecs <- lapply(all_dat, function(x) x$datetimestamp)
  
  ## 
  # date vector for combining
  # for union, intersect
  if(method %in% c('union', 'intersect')){
    
    date_vec <- Reduce(method, date_vecs)
    date_vec <- as.POSIXct(date_vec, origin = '1970-01-01', tz = timezone)
    
  # for a station
  } else {
    
    sel <- unlist(lapply(attrs, function(x) x$station == method))
    
    date_vec <- date_vecs[sel][[1]]
    
  }
    
  ##
  # merge stations by date_vec
  out <- data.table::data.table(datetimestamp = date_vec, key = 'datetimestamp')
  
  for(dat in all_dat){
    
    # set dummy time variable and parameter id for differ check
    dat$time_dum <- dat$datetimestamp
    dat_parms <- attr(dat, 'parameters')

    # merge
    dat <- data.table::data.table(dat, key = 'datetimestamp')
    out <- dat[out, roll = 'nearest']
 
    # set values outside of differ to NA
    time_diff <- abs(difftime(out$datetimestamp, out$time_dum, units='secs'))
    time_diff <- time_diff >= 60 * differ
    out <- data.frame(out)
    out[time_diff, names(out) %in% dat_parms] <- NA
    out$time_dum <- NULL
    out <- data.table::data.table(out, key = 'datetimestamp')
      
  }

  # format as swmpr object, return
  out <- data.frame(out)
  out <- swmpr(out, stations)
  
  return(out)
  
}