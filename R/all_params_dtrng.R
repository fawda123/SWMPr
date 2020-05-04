#' Get CDMO records within a date range
#' 
#' Get station records from the CDMO within a date range
#' 
#' @param station_code chr string of station, 7 or 8 characters
#' @param dtrng two element chr string, each of format MM/DD/YYYY
#' @param param chr string for a single parameter to return, defaults to all parameters for a station type.
#' @param trace logical indicating if import progress is printed in console
#' @param Max numeric indicating maximum number of records to return
#' 
#' @export
#' 
#' @concept retrieve
#' 
#' @return Returns a swmpr object, all parameters for a station type (nutrients, water quality, or meteorological) or a single parameter if specified.  QAQC columns are not provided for single parameters.
#' 
#' @details 
#' This function retrieves data from the CDMO through the web services URL.  The computer making the request must have a registered IP address.  Visit the CDMO web services page for more information: \url{http://cdmo.baruch.sc.edu/webservices.cfm}.  This function is the CDMO equivalent of \code{exportAllParamsDateRangeXMLNew}.
#' Download time may be excessive for large requests.
#' 
#' @seealso \code{\link{all_params}}, \code{\link{single_param}}
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' ## get all parameters within a date range
#' all_params_dtrng('apaebwq', c('01/01/2013', '02/01/2013'))
#' 
#' ## get single parameter within a date range
#' all_params_dtrng('apaebwq', c('01/01/2013', '02/01/2013'), 
#'    param = 'do_mgl')
#' 
#' }
all_params_dtrng <- function(station_code, dtrng, param = NULL, trace = TRUE, Max = NULL){
  
  ##
  # access CDMO web services
  
  # timer
  tictoc::tic()
  
  # url
  serv <- "http://cdmo.baruch.sc.edu/webservices2/requests.cfc?wsdl"
  
  # for initializing while loop
  tz <- time_vec(station_code = station_code, tz_only = TRUE)
  obsmin <- as.POSIXct(dtrng[1], format = '%m/%d/%Y', tz = tz)
  end_obs <- obsmin + 1
  
  # object to fill
  out_all <- NULL
  
  if(trace) cat('Importing...\n\n')
  
  # start loop
  while(end_obs > obsmin){
    
    # arguments to pass to function on server
    web_args = list(
      method = 'exportAllParamsDateRangeXMLNew',
      station_code = station_code,
      mindate = dtrng[1],
      maxdate = dtrng[2]
    )
    
    # add a parameter argument if provided
    if(!is.null(param)) web_args$param <- param
    
    # request data
    dat <- try({
      httr::GET(
        serv,
        query = web_args
      )
    }, silent = TRUE)
    
    # stop if retrieval error
    if('try-error' %in% class(dat))
      stop('Error retrieving data, check metadata for station availability.')
    
    # parse reply from server 
    out <- parser(dat)
    
    # sometimes data request is good, but empty data frame returned
    if(nrow(out) == 0)
      stop('Empty data frame, check metadata for station availability')
    
    # type of parameter requested - wq, nut, or met, NOT the param argument
    parm <- substring(station_code, 6)
    nms <- param_names(parm)[[parm]]
    
    # format datetimestamp, sort, get relevant columns as data frame
    out[, 'datetimestamp'] <- time_vec(out[, 'datetimestamp'], station_code)
    out <- out[order(out$datetimestamp), ]
    out <- data.frame(
      datetimestamp = out$datetimestamp,
      out[, tolower(names(out)) %in% nms, drop = FALSE],
      row.names = 1:nrow(out)
    )
    names(out) <- tolower(names(out))
    
    # get new loop 
    end_obs <- min(out$datetimestamp)
    max_obs <- max(out$datetimestamp)
    
    # progress
    if(trace) cat('\t', as.character(as.Date(max_obs) - 1), 'to', as.character(as.Date(end_obs)), '\n')
    
    # exit if no new data
    if(!is.null(out_all)){
      if(end_obs == min(out_all$datetimestamp)) break
    }
    
    # append to output
    out_all <- rbind(out_all, out)
    out_all <- unique(out_all[order(out_all$datetimestamp), ])
    
    if(!is.null(Max)){
      if(nrow(out_all) >= Max){  
        out_all <- out_all[(1 + nrow(out_all) - Max):nrow(out_all), ]
        break
      }
    }
    
    # new date ranges
    dtrng[2] <- as.character(as.Date(end_obs))
    dtrng[2] <- paste0(substr(dtrng[2], 6, nchar(dtrng[2])), '/', substr(dtrng[2], 1, 4))
    dtrng[2] <- gsub('-', '/', dtrng[2])
    
  }
  
  # sort by date, then remove duplicates (there will be overlaps)
  # data columns as numeric
  parms <- nms[!grepl('^f_', nms)]
  out <- out_all
  out[, names(out) %in% parms] <- apply(out[, names(out) %in% parms, drop = FALSE], 2, as.numeric)
  row.names(out) <- 1:nrow(out)
  
  # convert to swmpr class
  out <- swmpr(out, station_code)
  
  if(trace){
    cat('\n')
    cat(nrow(out), 'records, ')
    tictoc::toc()
  }
  
  # return output
  return(out)
  
}