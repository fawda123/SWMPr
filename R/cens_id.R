#' Flag observations above/below detection limits
#' 
#' Flag observations above/below detection limits
#'
#' @param swmpr_in input swmpr object
#' @param flag_type chr string indicating the flag type to return, must be one of \code{'below'}, \code{'above'}, or \code{'both'}, see details
#' @param select chr string of parameters to keep, defaults to all, \code{'datetimestamp'} will always be kept
#' @param ... optional arguments passed to or from other methods
#' 
#' @details 
#' Censored observations are identified in swmpr objects using the CDMO flags -4 or -5, indicating outside the low or high sensor range, respectively.  Additional codes are identified including A (-2007) or SUL (2007-) for above and B (-2007), SBL (2007-), SCB (2007-, calculated) for below detection limits.   The QAQC columns are searched for all parameters and replaced with the appropriate value indicating the detection limit as defined by \code{flag_type}.  The default argument \code{flag_type = 'both'} will recode the QAQC columns as -1, 0, or 1 indicating below, within, or above the detection limit.  Setting \code{flag_type = 'below'} or \code{'above'} will convert the columns to \code{TRUE}/\code{FALSE} values indicating observations beyond the detection limit (either above or below, \code{TRUE}) or within the normal detection range \code{FALSE}. 
#' The output includes additional columns similar to those for QAQC flags, such that the column names for censored flags include a \code{c_} prefix before the parameter name.  Note that the function will of course not work if already processed with \code{\link{qaqc}}.  QAQC columns are retaine for additional processing.
#' 
#' The user should refer to the metadata or visually examine the observed data to identify the actual limit, which may change over time.  
#'
#' @concept organize
#' 
#' @export
#' 
#' @seealso \code{\link{qaqc}}
#' 
#' @return Returns a swmpr object with additional columns for censored flag values and the appropriate flag type based on the input arguments.  Censored flag columns are named with a \code{c_} prefix.
#' 
#' @examples
#' ## get data
#' data(apacpnut)
#' dat <- apacpnut
#' 
#' ## convert all qaqc columns to censored flags, -1 below, 0 within, 1 above
#' cens_id(dat)
#' 
#' ## T/F for above or within, note that none are above
#' cens_id(dat, flag_type = 'above')
#' 
#' ## T/F for below or within
#' cens_id(dat, flag_type = 'below')
cens_id <- function(swmpr_in, ...) UseMethod('cens_id')

#' @rdname cens_id
#' 
#' @export
#' 
#' @method cens_id swmpr
cens_id.swmpr <- function(swmpr_in, flag_type = 'both', select = NULL, ...){ 
  
  # attributes
  qaqc_cols <- attr(swmpr_in, 'qaqc_cols')
  station <- attr(swmpr_in, 'station')
  parameters <- attr(swmpr_in, 'parameters')
  
  dat <- swmpr_in
  
  # sanity checks
  # exit function if no qaqc columns
  if(!qaqc_cols) stop('No qaqc columns in input data')
  if(!flag_type %in% c('both', 'above', 'below'))
    stop('flag_type must be one of both, above, or below')
  
  #names of qaqc columns
  qaqc_sel <- grepl('f_', names(dat))
  
  # get matrix of -1, 0, or 1 for below, within, or above detect limit
  cens_dat <- dat[, qaqc_sel, drop = FALSE]
  cens_dat <- apply(cens_dat, 2, 
                    function(x){
                      
                      x <- as.character(x)
                      
                      # within
                      out <- rep(0, length(x))
                      
                      # above
                      out[grepl('<-5>|SUL|\\sA\\s|\\sA$', x)] <- '1'
                      
                      # below
                      out[grepl('<-4>|SBL|SCB|\\sB\\s|\\sB$', x)] <- '-1'
                      
                      # NA vals
                      out[is.na(x)] <- NA
                      out
                      
                    }
  )
  
  # change flag type 
  if(flag_type == 'above')
    cens_dat <- cens_dat == '1'
  
  if(flag_type == 'below')
    cens_dat <- cens_dat == '-1'
  
  # change names for cens_dat
  cens_dat <- as.data.frame(cens_dat, stringsAsFactors = FALSE)
  names(cens_dat) <- gsub('^f_', 'c_', names(cens_dat))
  
  # sort output column as in dat
  out <- data.frame(dat, cens_dat)
  inds <- seq(2, ncol(out), by = 3)
  out[, inds] <- dat[, parameters]
  out[, inds + 1] <- dat[, qaqc_sel]
  out[, inds + 2] <- cens_dat
  
  # change names to correct order
  names(out)[inds] <- parameters
  names(out)[inds + 1] <- names(dat)[qaqc_sel]
  names(out)[inds + 2] <- names(cens_dat)
  
  # select all if NULL
  if(is.null(select)) 
    select <- parameters
  
  # subset columns
  select <- c(select, paste0('f_', select), paste0('c_', select))
  out <- out[, names(out) %in% c('datetimestamp', select)]
  
  # create swmpr class
  out <- swmpr(out, station)
  
  return(out)
  
}