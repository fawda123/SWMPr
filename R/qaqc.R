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
#' @concept organize
#' 
#' @details
#' The qaqc function is a simple screen to retain values from the data with specified QAQC flags, described online: \url{https://cdmo.baruch.sc.edu/data/qaqc.cfm}. Each parameter in the swmpr data typically has a corresponding QAQC column of the same name with the added prefix 'f_'. Values in the QAQC column specify a flag from -5 to 5. Generally, only data with the '0' QAQC flag should be used, which is the default option for the function. Data that do not satisfy QAQC criteria are converted to \code{NA} values. Additionally, simple filters are used to remove obviously bad values, e.g., wind speed values less than zero or pH values greater than 12. Erroneous data entered as -99 are also removed. Processed data will have QAQC columns removed, in addition to removal of values in the actual parameter columns that do not meet the criteria.
#' 
#' The data are filtered by matching the flag columns with the character string provided by \code{qaqc_keep}.  A single combined string is created by pasting each element together using the '|' operator, then using partial string matching with \code{\link[base]{grepl}} to compare the actual flags in the QAQC columns.  Values that can be passed to the function are those described online: \url{https://cdmo.baruch.sc.edu/data/qaqc.cfm}.
#' 
#' @examples
#' \dontrun{
#' ## get data
#' data(apadbwq)
#' dat <- apadbwq
#' 
#' ## retain only '0' and '-1' flags
#' qaqc(dat, qaqc_keep = c('0', '-1'))
#' 
#' ## retain observations with the 'CSM' error code
#' qaqc(dat, qaqc_keep = 'CSM')
#' }
qaqc <- function(swmpr_in, ...) UseMethod('qaqc')

#' @rdname qaqc
#' 
#' @param qaqc_keep character string of qaqc flags to keep, default \code{'0'}, any number of flag codes can be supplied including three character error codes (see examples)
#' @param trace logical for progress output on console, default \code{FALSE}
#' 
#' @export
#' 
#' @method qaqc swmpr
qaqc.swmpr <- function(swmpr_in, 
                       qaqc_keep = '0',
                       trace = FALSE, ...){
  
  ##
  # swmpr data and attributes
  dat <- swmpr_in
  qaqc_cols <- attr(swmpr_in, 'qaqc_cols')
  cens_cols <- attr(swmpr_in, 'cens_cols')
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
  
  # surround integers with brackets
  # otherwise both positive and negative flags will be kept
  topaste <- suppressWarnings(as.numeric(qaqc_keep))
  topaste <- !is.na(topaste)
  qaqc_keep[topaste] <- paste0('<', qaqc_keep[topaste], '>')
  
  #names of qaqc columns
  qaqc_sel <- grep('f_', names(dat), value = TRUE)

  # keep all if qaqc_in is NULL, otherwise process qaqc
  if(length(qaqc_keep) == 0){ 
    
    qaqc <- dat[, names(dat) %in% gsub('f_', '', qaqc_sel)]
    
  } else {
    
    # qaqc columns
    qaqc_vec <- dat[, names(dat) %in% qaqc_sel, drop = FALSE]
    
    # add angle brackets if not present
    qaqc_vec <- apply(qaqc_vec, 2, function(x){

      x <- gsub('<|>', '', x)
      x <- gsub('^(.*\\d)', '<\\1', x)
      x <- gsub('^(.*\\d)', '\\1>', x)

      return(x)

    })

    #matrix of TF values for those that don't pass qaqc
    qaqc_vec <- apply(qaqc_vec, 2, 
                      function(x) !grepl(paste(qaqc_keep, collapse = '|'), x)
    )
    
    #replace T values with NA
    #qaqc is corrected
    qaqc <- dat[, names(dat) %in% gsub('f_', '', qaqc_sel), drop = FALSE]
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
  
  # convert columns to numeric, missing converted to NA
  # NA values from qaqc still included as NA
  front <- dat[, !names(dat) %in% c(parameters, qaqc_sel), drop = FALSE]
  nr <- nrow(dat)
  nc <- length(parameters)
  out <- c(as.matrix(qaqc))
  out[is.nan(out)] <- NA
  out[out %in%  c(-Inf, Inf, -99)] <- NA
  out <- matrix(out, nrow = nr, ncol = nc) 
  out <- data.frame(
    front,
    out
  )
  names(out) <- c(names(front), parameters)
  
  # add back censored columns if present, in correct order
  if(cens_cols){
    
    cens_nms <- paste0('c_', parameters)
    frnt_nms <- names(front)[!names(front) %in% cens_nms]
    nms <- c(frnt_nms, c(rbind(parameters, cens_nms)))
    
    out <- out[, nms]
    
  }
  
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
