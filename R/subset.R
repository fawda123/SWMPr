#' Subset a swmpr object
#' 
#' Subset a swmpr object by a date range, parameters, or non-empty values
#'
#' @param x input swmpr object
#' @param subset chr string of form 'YYYY-mm-dd HH:MM' to subset a date range.  Input can be one (requires \code{operator} or two values (a range).
#' @param select chr string of parameters to keep, \code{'datetimestamp'} will always be kept
#' @param operator chr string specifiying binary operator (e.g., \code{'>'}, \code{'<='}) if subset is one date value
#' @param rem_rows logical indicating if rows with no data are removed, default \code{FALSE}
#' @param rem_cols is logical indicating if cols with no data are removed, default \code{FALSE}
#' @param ... arguments passed to other methods
#' 
#' @export
#' 
#' @concept organize
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
#' ## subset records greater than or equal to a date
#' subset(dat, subset = '2013-01-01 0:00', operator = '>=')
#' 
#' ## subset records within a date range, select two parameters
#' subset(dat, subset = c('2012-07-01 6:00', '2012-08-01 18:15'),
#'    select = c('atemp', 'totsorad'))
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
    chks <- select %in% c('datetimestamp', parameters, paste0('f_', parameters))
    if(any(!chks)){
      
      nonmtch <- which(!chks)
      nonmtch <- paste(select[nonmtch], collapse = ', ')
      stop(paste('select argument is invalid:', nonmtch))
      
    }
    
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