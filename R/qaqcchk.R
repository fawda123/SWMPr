#' Summary of QAQC flags in SWMP data
#' 
#' Summary of the number of observations with a given QAQC flag for each parameter column of a swmpr object
#'
#' @param swmpr_in input swmpr object
#' 
#' @export
#' 
#' @concept organize
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
#' @export
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
  out <- reshape2::melt(out)
  names(out) <- c('flag', 'count', 'variable')
  out <- tidyr::spread(out, 'variable', 'count')
  out[is.na(out)] <- 0
  
  # return output
  return(out)
  
}