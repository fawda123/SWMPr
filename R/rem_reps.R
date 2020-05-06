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
#' @importFrom stats aggregate na.pass
#' 
#' @concept organize
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
#' @export
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