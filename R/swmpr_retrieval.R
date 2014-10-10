######
#' Import station records from the CDMO starting with the most current date, CDMO equivalent of \code{exportAllParamsXMLNew}
#' 
#' @param  station_code chr string of station, 7 or 8 characters
#' @param  Max numeric value for number of records to obtain from the current date, maximum of 100
#' 
#' @export
#' @return  Returns a swmpr object, all available parameters including QAQC columns
all_params <- function(station_code, Max = 100){
  
  # sanity check
  if(Max > 100) warning('Maximum of 100 records')
  
  # install SSOAP if not available
  check_soap <- require('SSOAP')
  if(!check_soap)
    install.packages("SSOAP", repos="http://www.omegahat.org/R", 
      dependencies = T,  type =  "source")
  
  # access CDMO web services
  serv <- SOAPServer(
    "http://cdmo.baruch.sc.edu/webservices2/requests.cfc?wsdl"
    )
  
  # get from current date
  dat <- try({
    .SOAP(
      serv,
      method = 'exportAllParamsXMLNew',
      station_code = station_code,
      tbl = Max,
      action="",
      .convert = F
      )
    }, silent = T)
  
  # stop if retrieval error
  if('try-error' %in% class(dat))
    stop('Error retrieving data, check metadata for station availability.')
  
  # parse reply from server 
  out <- parser(dat)
  
  # type of parameter requested - wq, nut, or met
  parm <- substring(station_code, 6)
  nms <- param_names(parm)[[parm]]
  
  # format datetimetamp if output is not empty
  if(ncol(out) != 0 & nrow(out) != 0){
    
    # format datetimestamp and sort
    out[, 'datetimestamp'] <- time_vec(out[, 'datetimestamp'], station_code)
    out <- out[order(out$datetimestamp), ]
    out <- data.frame(
      datetimestamp = out$datetimestamp,
      out[, tolower(names(out)) %in% nms, drop = F], 
      row.names = 1:nrow(out)
      )
    names(out) <- tolower(names(out))
    
    }

  # convert to swmpr class
  out <- swmpr(out, station_code)
  
  # return output
  return(out)
  
}

######
#' Get station records from the CDMO for a date range, CDMO equivalent of \code{exportAllParamsDateRangeXMLNew}
#' 
#' @param  station_code chr string of station, 7 or 8 characters
#' @param  dtrng two element chr string, each of format MM/DD/YYYY
#' @param  param chr string for a single parameter to return, defaults to all parameters for a station type.
#' 
#' @export
#' @return Returns a swmpr object, all parameters for a station type (nutrients, water quality, or meteorological) or a single parameter if specified.  QAQC columns are not provided for single parameters.  Up to 1000 records can be obtained.
all_params_dtrng <- function(station_code, dtrng, param = NULL){
  
  # install SSOAP if not available
  check_soap <- require('SSOAP')
  if(!check_soap)
    install.packages("SSOAP", repos="http://www.omegahat.org/R", 
      dependencies = T,  type =  "source")
  
  # access CDMO web services
  serv <- SOAPServer(
    "http://cdmo.baruch.sc.edu/webservices2/requests.cfc?wsdl"
    )
  
  # arguments to pass to function on server
  soap_args = list(
      station_code = station_code,
      mindate = dtrng[1],
      maxdate = dtrng[2]
      )
  
  # add a parameter argument if provided
  if(!is.null(param)) soap_args$fieldlist <- param
  
  # request data
  dat <- try({
    .SOAP(
      serv,
      method = 'exportAllParamsDateRangeXMLNew',
      .soapArgs = soap_args, 
      action = '',
      .convert = F
      )}, silent = T)
  
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
    out[, tolower(names(out)) %in% nms, drop = F],
    row.names = 1:nrow(out)
    )
  names(out) <- tolower(names(out))
  
  # convert to swmpr class
  out <- swmpr(out, station_code)
  
  # return output
  return(out)
  
}

######
#' Get stations records from the CDMO for a single parameter starting with the most current date, CDMO equivalent of \code{exportSingleParamXML} max 100 records
#' 
#' @param  station_code chr string of station, 7 or 8 characters
#' @param  Max numeric value for number of records to obtain from the current date, maximum of 100
#' @param  param chr string for a single parameter to return.
#' 
#' @import XML plyr
#' 
#' @export
#' @return Returns a swmpr object with one parameter.  QAQC columns are not provided.
single_param <- function(station_code, Max = 100, param){
  
  # sanity check
  if(Max > 100) warning('Maximum of 100 records')
  
  # install SSOAP if not available
  check_soap <- require('SSOAP')
  if(!check_soap)
    install.packages("SSOAP", repos="http://www.omegahat.org/R", 
      dependencies = T,  type =  "source")
  
  # access CDMO web services
  serv <- SOAPServer(
    "http://cdmo.baruch.sc.edu/webservices2/requests.cfc?wsdl"
    )
  
  # request data
  dat <- try({
    .SOAP(
      serv,
      method = 'exportSingleParamXML',
      .soapArgs = list(
        station_code = station_code,
        recs = Max,
        param = param
        ),
      action = '',
      .convert = F
      )}, silent = T)
    
  
  # stop if retrieval error
  if('try-error' %in% class(dat))
    stop('Error retrieving data, check metadata for station availability.')
  
  # convert to XMLDocumentContent for parsing
  raw <- htmlTreeParse(dat$content, useInternalNodes = T)

  # get node attributes for c after parsing
  attrs <- xpathSApply(
    raw,
    '//data//r//c',
    fun = xmlAttrs
  )
  
  # arrange as data frame
  out <- matrix(attrs, ncol = 2, byrow = T)
  colnames(out) <- tolower(out[1, ])
  out <- data.frame(out[-1,], stringsAsFactors = F)
  out[, param] <- as.numeric(out[, param])
  
  # type of parameter requested - wq, nut, or met, NOT the param argument
  parm <- substring(station_code, 6)
  
  # format datetimestamp and sort
  out[, 'datetimestamp'] <- time_vec(out[, 'datetimestamp'], station_code)
  out <- out[order(out$datetimestamp), ]
  out <- data.frame(
    datetimestamp = out$datetimestamp,
    out[, !tolower(names(out)) %in% 'datetimestamp', drop = F], 
    row.names = 1:nrow(out)
    )
  names(out) <- tolower(names(out))

  # convert to swmpr class
  out <- swmpr(out, station_code)
  
  # return output
  return(out)
  
}

######
#' Import local data that were obtained from the CDMO through a zip request
#' 
#' @param  path chr string of full path to .csv files with raw data
#' @param  station_code chr string of station to import, 7 or 8 characters
#' @param  trace logical indicating if progress is sent to console, default \code{F}
#' 
#' @export
#' @return Returns a swmpr object with all parameters and QAQC columns for the station.  The full date range in the raw data are also imported.
import_local <- function(path, station_code, trace = F){
  
  # sanity check
  if(!nchar(station_code) %in% c(7, 8)) stop('station_code invalid.')
  
  ##
  # find station files in path
  file_nms <- dir(path)
  expr <- paste0('^', station_code, '.*', '\\.csv$')
  files_in <- grep(expr, file_nms, value = T)
  
  if(length(files_in) == 0) stop('File(s) not found.')
  
  # import all data files for a station
  dat <- vector('list', length(files_in))
  names(dat) <- gsub('.csv', '', files_in)
  
  if(trace) cat('Loading files...\n\n')
  
  for(file_in in files_in){
    
    if(trace) cat(file_in, '\t')
    
    # import file
    tmp <- read.csv(file.path(path, file_in), stringsAsFactors = F)
    names(tmp) <- tolower(names(tmp))
    
    # convert date time to posix
    names(tmp)[grep('datetimestamp', names(tmp), ignore.case = T)] <- 'datetimestamp'
    tmp$datetimestamp <- time_vec(tmp$datetimestamp, station_code)
    
    # append to output list
    nm <-  gsub('.csv', '', file_in)
    dat[[nm]] <- tmp
    
    }
  
  ##
  # column names for each parameter type, used to subset combined data
  # kept as upper case here because improted data will match, changed to lower below

  # names to use
  parm <- substring(station_code, 6)
  nms <- param_names(parm)[[parm]]
  
  ##
  # convert output from 'import_local' to data frame and appropriate columns
  
  if(trace) cat('\n\nCombining data...')
  
  out <- do.call('rbind', dat)
  out <- data.frame(
    datetimestamp = out$datetimestamp,
    out[, names(out) %in% nms], 
    row.names = seq(1, nrow(out))
    )
  
  # names as lower case
  names(out) <- tolower(names(out))
  
  # convert to swmpr class
  out <- swmpr(out, station_code)
  
  if(trace) cat('\n\nData imported...')
  
  # return data frame
  return(out)
    
}