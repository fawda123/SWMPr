######
#' Import current station records from the CDMO
#' 
#' Import current station records from the CDMO starting with the most current date
#' 
#' @param  station_code chr string of station, 7 or 8 characters
#' @param  Max numeric value for number of records to obtain from the current date, maximum of 100
#' 
#' @export
#' 
#' @seealso \code{\link{all_params_dtrng}}, \code{\link{single_param}}
#' 
#' @return  Returns a swmpr object, all available parameters including QAQC columns
#' 
#' @details 
#' This function uses the SSOAP package to retrieve data from the CDMO through a SOAP client interface.  The computer making the request must have a registered IP address.  Visit the CDMO web services page for more information: \url{http://cdmo.baruch.sc.edu/webservices.cfm}.  Function is the CDMO equivalent of \code{exportAllParamsXMLNew}.
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' ## all parameters for a station, most recent
#' all_params('hudscwq')
#' 
#' }
all_params <- function(station_code, Max = 100){
  
  # sanity check
  if(Max > 100) warning('Maximum of 100 records')
  
  # install SSOAP if not available
  check_soap <- require('SSOAP')
  if(!check_soap)
    install.packages("SSOAP", repos="http://www.omegahat.org/R", 
      dependencies = TRUE,  type =  "source")
  
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
      .convert = FALSE
      )
    }, silent = TRUE)
  
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
      out[, tolower(names(out)) %in% nms, drop = FALSE], 
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
#' Get CDMO records within a date range
#' 
#' Get station records from the CDMO within a date range
#' 
#' @param  station_code chr string of station, 7 or 8 characters
#' @param  dtrng two element chr string, each of format MM/DD/YYYY
#' @param  param chr string for a single parameter to return, defaults to all parameters for a station type.
#' 
#' @export
#' 
#' @return Returns a swmpr object, all parameters for a station type (nutrients, water quality, or meteorological) or a single parameter if specified.  QAQC columns are not provided for single parameters.  Up to 1000 records can be obtained.
#' 
#' @details 
#' This function uses the SSOAP package to retrieve data from the CDMO through a SOAP client interface.  The computer making the request must have a registered IP address.  Visit the CDMO web services page for more information: \url{http://cdmo.baruch.sc.edu/webservices.cfm}.  This function is the CDMO equivalent of \code{exportAllParamsDateRangeXMLNew}.
#' 
#' @seealso \code{\link{all_params}}, \code{\link{single_param}}
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' ## get all parameters within a date range
#' all_params_dtrng('hudscwq', c('09/10/2012', '02/8/2013'))
#' 
#' ## get single parameter within a date range
#' all_params_dtrng('hudscwq', c('09/10/2012', '02/8/2013'), 
#'    param = 'do_mgl')
#' 
#' }
all_params_dtrng <- function(station_code, dtrng, param = NULL){
  
  # install SSOAP if not available
  check_soap <- require('SSOAP')
  if(!check_soap)
    install.packages("SSOAP", repos="http://www.omegahat.org/R", 
      dependencies = TRUE,  type =  "source")
  
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
      .convert = FALSE
      )}, silent = TRUE)
  
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
  
  # convert to swmpr class
  out <- swmpr(out, station_code)
  
  # return output
  return(out)
  
}

######
#' Get CDMO records for a single parameter
#' 
#' Get stations records from the CDMO for a single parameter starting with the most current date
#' 
#' @param  station_code chr string of station, 7 or 8 characters
#' @param  Max numeric value for number of records to obtain from the current date, maximum of 100
#' @param  param chr string for a single parameter to return.
#' 
#' @import XML plyr
#' 
#' @export
#' 
#' @return Returns a swmpr object with one parameter.  QAQC columns are not provided.
#' 
#' @details 
#' This function uses the SSOAP package to retrieve data from the CDMO through a SOAP client interface.  The computer making the request must have a registered IP address.  Visit the CDMO web services page for more information: \url{http://cdmo.baruch.sc.edu/webservices.cfm}.  This function is the CDMO equivalent of \code{exportSingleParamXML}.
#' 
#' @seealso \code{\link{all_params}}, \code{\link{all_params_dtrng}}
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' ## single parameter for a station, most recent
#' single_param('hudscwq', 'do_mgl')
#' 
#' }
single_param <- function(station_code, param, Max = 100){
  
  # sanity check
  if(Max > 100) warning('Maximum of 100 records')
  
  # install SSOAP if not available
  check_soap <- require('SSOAP')
  if(!check_soap)
    install.packages("SSOAP", repos="http://www.omegahat.org/R", 
      dependencies = TRUE,  type =  "source")
  
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
      .convert = FALSE
      )}, silent = TRUE)
    
  
  # stop if retrieval error
  if('try-error' %in% class(dat))
    stop('Error retrieving data, check metadata for station availability.')
  
  # convert to XMLDocumentContent for parsing
  raw <- htmlTreeParse(dat$content, useInternalNodes = TRUE)

  # get node attributes for c after parsing
  attrs <- xpathSApply(
    raw,
    '//data//r//c',
    fun = xmlAttrs
  )
  
  # arrange as data frame
  out <- matrix(attrs, ncol = 2, byrow = TRUE)
  colnames(out) <- tolower(out[1, ])
  out <- data.frame(out[-1,], stringsAsFactors = FALSE)
  out[, param] <- as.numeric(out[, param])
  
  # type of parameter requested - wq, nut, or met, NOT the param argument
  parm <- substring(station_code, 6)
  
  # format datetimestamp and sort
  out[, 'datetimestamp'] <- time_vec(out[, 'datetimestamp'], station_code)
  out <- out[order(out$datetimestamp), ]
  out <- data.frame(
    datetimestamp = out$datetimestamp,
    out[, !tolower(names(out)) %in% 'datetimestamp', drop = FALSE], 
    row.names = 1:nrow(out)
    )
  names(out) <- tolower(names(out))

  # convert to swmpr class
  out <- swmpr(out, station_code)
  
  # return output
  return(out)
  
}

######
#' Import local CDMO data
#' 
#' Import local data that were obtained from the CDMO through the zip downloads feature
#' 
#' @param  path chr string of full path to .csv files with raw data
#' @param  station_code chr string of station to import, typically 7 or 8 characters but may include full name with year, excluding file extension
#' @param  trace logical indicating if progress is sent to console, default \code{FALSE}
#' 
#' @export
#' 
#' @return Returns a swmpr object with all parameters and QAQC columns for the station.  The full date range in the raw data are also imported.
#' 
#' @details 
#' The function is designed to import local data that were downloaded from the CDMO outside of R. This approach works best for larger data requests.  The function is designed for data from the zip downloads feature in the advanced query section of the CDMO. The function may also work using data from the data export system, but this feature has not been extensively tested (expect bugs). The downloaded data will be in a compressed folder that includes multiple .csv files by year for a given data type (e.g., apacpwq2002.csv, apacpwq2003.csv, apacpnut2002.csv, etc.). The import_local function can be used after the folder is decompressed.
#' 
#' Zip download request through CDMO: \url{http://cdmo.baruch.sc.edu/aqs/zips.cfm}
#' 
#' @seealso \code{\link{all_params}}, \code{\link{all_params_dtrng}}, \code{\link{single_param}}
#' 
#' @examples
#' 
#' ## this is the path for csv example files
#' path <- system.file('zip_ex', package = 'SWMPr')
#'
#' ## import, do not include file extension
#' import_local(path, 'apaebmet') 
#' 
import_local <- function(path, station_code, trace = FALSE){
  
  # sanity check
  chk_file <- paste0('^', station_code, '.*\\.csv$')
  if(!any(grepl(chk_file, dir(path)))) 
    stop('station_code not found in directory')
  
  ##
  # find station files in path
  file_nms <- dir(path)
  expr <- paste0('^', station_code, '.*', '\\.csv$')
  files_in <- grep(expr, file_nms, value = TRUE)

  if(length(files_in) == 0) stop('File(s) not found.')

  station_code <- tolower(station_code)
  
  # import all data files for a station
  dat <- vector('list', length(files_in))
  names(dat) <- gsub('.csv', '', files_in)
  
  if(trace) cat('Loading files...\n\n')
  
  for(file_in in files_in){
    
    if(trace) cat(file_in, '\t')
    
    # import file, try using read.csv, else readlines
    tmp <- try({
      read.csv(file.path(path, file_in), stringsAsFactors = FALSE)
    }, silent = TRUE)
    
    if('try-error' %in% class(tmp)){
      raw <- readLines(file.path(path, file_in))
      keep_lines <- grep(paste0('^', station_code), raw)
      tmp <- raw[keep_lines]
      tmp <- strsplit(tmp, ',')
      tmp <- do.call('rbind', tmp)
      tmp <- data.frame(tmp, stringsAsFactors = FALSE)
      names(tmp)  <- strsplit(
        gsub('["\\"]', '', raw[keep_lines[1] - 1]),
        ',')[[1]] 
    }
      
    names(tmp) <- tolower(names(tmp))
    
    # convert date time to posix
    names(tmp)[grep('datetimestamp', names(tmp), ignore.case = TRUE)] <- 'datetimestamp'
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
  parm <- gsub('[0-9.*]', '', parm)
  nms <- param_names(parm)[[parm]]
  
  ##
  # convert output from 'import_local' to data frame and appropriate columns
  
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