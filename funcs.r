#######################
# functions for SWMPr #
# Sep 2014            #
#######################

########################
# misc functions
########################

######
# function for creating swmpr class
# used to create output in data retrieval functions - 'all_params', 'all_params_dtrng', 'single_param'
# generic methods defined for swmpr below
# 'stat_in' is data frame of swmp data
# 'meta_in' is station code, 7 or 8 characters
swmpr <- function(stat_in, meta_in){
    
  if(!is.data.frame(stat_in)) 
    stop('stat_in must be data.frame')
  
  # qaqc attribute
  qaqc_cols <- F
  if(any(grepl('^f_', names(stat_in)))) qaqc_cols <- T
  
  # parameters attribute
  parameters <- grep('datetimestamp|^f_', names(stat_in), invert = T, value = T)
  
  # get stations, param_types attribtues
  param_types <- param_names()
  param_types <- unlist(lapply(param_types, function(x) any(x %in% parameters)))
  param_types <- names(param_names())[param_types]
  station <- grep(paste0(param_types, collapse = '|'), meta_in, value = T)
  
  # timezone using time_vec function
  timezone <- time_vec(station_code = station, tz_only = T)

  # create class, with multiple attributes
  structure(
    list(station_data = stat_in), 
    class = 'swmpr', 
    station = station,
    parameters = parameters, 
    qaqc_cols = qaqc_cols,
    date_rng = range(stat_in$datetimestamp),
    timezone = timezone, 
    stamp_class = class(stat_in$datetimestamp)
    )
  
}

######
# generic parsing function for objects returned from SOAP server
# called internally from other functions
# 'soap_in' is soap object returned from server
# 'parent_in' is a text string of parent nodes to parse
parser <- function(soap_in, parent_in = 'data'){
  
  # sanity check
  if(!'SOAPHTTPReply' %in% class(soap_in))
    stop('Input must be of class SOAPHTTPReply')
  
  library(XML)
  library(plyr)
  
  # convert to XMLDocumentContent for parsing
  raw <- htmlTreeParse(soap_in$content, useInternalNodes = T)

  # get parent data nodes
  parents <- xpathSApply(
    raw,
    paste0('//', parent_in)
  )
  
  # get children nodes from data parents
  out <- ldply(parents, 
    .fun = function(x) getChildrenStrings(x)
    )

  # return output
  return(out)
  
}

######
# convert datetimestamp string to POSIXct, using correct tz (no daylight saving)
# uses hardcoded timezone data for each reserve
# 'chr_in' is datetimestamp vector
# 'station_code' is character string for station (three or more characters)
# 'tz_only' is logical that returns only the timezone
# otherwise output is POSIX vector
time_vec <- function(chr_in = NULL, station_code, tz_only = F){
  
  # lookup table for time zones based on gmt offset - no DST!
  gmt_tab <- data.frame(
    gmt_off=c(-4,-5,-6,-8,-9),
    tz = c('America/Virgin', 'America/Jamaica', 'America/Regina',
      'Pacific/Pitcairn', 'Pacific/Gambier'),
    stringsAsFactors = F
    )
  
  # hard-coded gmt offset for each site, from metadata direct from CDMO
  sites <- c('ace', 'apa', 'cbm', 'cbv', 'del', 'elk', 
    'gnd', 'grb', 'gtm', 'hud', 'jac', 'job', 'kac', 
    'lks', 'mar', 'nar', 'niw', 'noc', 'owc', 'pdb', 
    'rkb', 'sap', 'sfb', 'sos', 'tjr', 'wel', 'wkb',
    'wqb')
  gmt_offsets <- c(-5, -5, -5, -5, -5, -8, -6, -5, -5, -5, -5, -4, 
    -9, -6, -6, -5, -5, -5, -5, -8, -5, -5, -8, -8, -8,
    -5, -6, -5)  
  
  # get timezone from above information
  gmt_offset <- gmt_offsets[which(sites %in% substr(station_code, 1, 3))]
  tzone <- gmt_tab[gmt_tab$gmt_off %in% gmt_offset, 'tz']

  # timezone only if T
  if(tz_only) return(tzone)
  
  # format datetimestamp
  out <- as.POSIXct(chr_in, tz = tzone, format = '%m/%d/%Y %H:%M')
  
  # return output
  return(out)
  
}

######
# data frame of station metadata
# CDMO equivalent of exportStationCodesXML
site_codes <- function(){

  library(SSOAP)
  
  # access CDMO web services
  serv <- SOAPServer(
    "http://cdmo.baruch.sc.edu/webservices2/requests.cfc?wsdl"
    )

  # get all station codes
  reply <- .SOAP(
    serv,
    method = 'exportStationCodesXMLNew',
    action="", 
    .convert = F
    )

  # parse reply from server
  out <- parser(reply)
  out$params_reported <- tolower(out$params_reported)
  
  # return output
  return(out)
  
}

######
# get metadata for single site
# 'nerr_site_id' is text string of site, three letters
site_codes_ind <- function(nerr_site_id){
  
  library(SSOAP)
  
  # access CDMO web services
  serv <- SOAPServer(
    "http://cdmo.baruch.sc.edu/webservices2/requests.cfc?wsdl"
    )

  # get all station codes
  reply <- .SOAP(
      serv,
      method = 'NERRFilterStationCodesXMLNew',
      NERRFilter = nerr_site_id,
      action="", 
      .convert = F
      )

  # parse reply from server
  out <- parser(reply)
  out$params_reported <- tolower(out$params_reported)
  
  # return output
  return(out)
  
}

######
# returns parameter columns names for each parameter type - nut, wq, met
# names are actual parameter and corresponding qaqc column name ('f_' prefix)
# output also used to check data type after subsetsfor attributes
# used in 'import_local', 'all_params', 'all_params_dtrng' (single parameter only), 'swmpr'
# 'param_type' is character string specifying 'nut', 'wq', or met'
param_names <- function(param_type = c('nut', 'wq', 'met')){
  
  # sanity check
  if(any(!param_type %in% c('nut', 'wq', 'met')))
    stop('param_type must chr string of nut, wq, or met')
  
  nut_nms <- c('po4f', 'chla_n', 'no3f', 'no2f', 'nh4f', 'no23f', 'ke_n',
    'urea')
  nut_nms <- paste0(c('', 'f_'), rep(nut_nms, each = 2))
  
  wq_nms <- c('temp', 'spcond', 'sal', 'do_pct', 'do_mgl', 'depth', 
    'cdepth', 'level', 'clevel', 'ph', 'turb', 'chlfluor')
  wq_nms <- paste0(c('', 'f_'), rep(wq_nms, each = 2))
  
  met_nms <- c('atemp', 'rh', 'bp', 'wspd', 'maxwspd', 'wdir', 'sdwdir',
    'totpar', 'totprcp', 'cumprcp', 'totsorad')
  met_nms <- paste0(c('', 'f_'), rep(met_nms, each = 2))
  
  # get names for a given parameter type
  out <- sapply(param_type, function(x) get(paste0(x, '_nms')), simplify = F)
  
  return(out)
  
}

########################
# retrieval functions
########################

######
# get data for a station back to x number of records
# 'station_code' is text string of station
# 'Max' is numeric of no. of records
# output is swmpr class object
all_params <- function(station_code, Max = 100){
  
  library(SSOAP)
  
  # sanity check
  if(Max > 100) warning('Maximum of 100 records')
  
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
# get records from date range, max of 1000 records
# 'station_code' is text string of station
# 'dtrng' is two element character vector, each of format MM/DD/YYYY
# 'param' is character vector of a single parameter to return, see results from 'stat_codes' function for availability, default is all specific to station type
all_params_dtrng <- function(station_code, dtrng, param = NULL){
  
  library(SSOAP)
  
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
# get records for a single parameter, max 100 records
# 'station_code' is text string of station
# 'Max' is numeric of no. of records
# 'param' is text string of parameter to return
# does not use parser as above, slight mod 
single_param <- function(station_code, Max = 100, param){
  
  library(SSOAP)
  library(XML)
  library(plyr)
  
  # sanity check
  if(Max > 100) warning('Maximum of 100 records')
  
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
# import local data from CDMO
# data are .csv files for a station, all parameters and years, from zip downloads
# 'path' is chr string of full path with .csv files
# 'station_code' is 7 or 8 chr string of station to load, single parameter
# 'trace' is logical indicating if progress is sent to console
import_local <- function(path, station_code, trace = T){
  
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

########################
# organize functions
########################

######
# qaqc filtering for data obtained from retrieval functions, local and remote
# qaqc is generic, qaqc.swmpr is method
# 'swmpr_in' is data frame returned from any of the above retrieval functions
# 'qaqc_keep' is numeric vector of qaqc flags to keep, default 'zero'
# 'trace' is logical for progress
qaqc <- function(x, ...) UseMethod('qaqc')
qaqc.swmpr <- function(swmpr_in, 
  qaqc_keep = 0,
  trace = F){
  
  ##
  # sanity checks
  if(!class(qaqc_keep) %in% c('numeric', 'NULL'))
    stop('qaqc_in argument must be numeric or NULL')
  
  ##
  # swmpr data and attributes
  dat <- swmpr_in$station_data
  qaqc_cols <- attr(swmpr_in, 'qaqc_cols')
  station <- attr(swmpr_in, 'station')
  
  # exit function if no qaqc columns
  if(!qaqc_cols){
    warning('No qaqc columns in input data')
    return(swmpr_in)
    }
  
  ##
  #remove values flagged by QA/QC, see cdmo website for flag numbers

  if(trace) cat('Processing QAQC columns...')
  
  #names of qaqc columns
  qaqc_sel <- grep('f_', names(dat), value = T)
  
  qaqc_rm <- as.numeric(seq(-5,  5))
  qaqc_rm <- qaqc_rm[!qaqc_rm %in% qaqc_keep]
  
  # keep all if qaqc_in is NULL, otherwise process qaqc
  if(is.null(qaqc_keep)){ 
    
    rm_col <- c('datetimestamp', 'statparam', qaqc_sel)
    qaqc <- dat[, !names(dat) %in% rm_col]

  } else {
      
    #matrix of TF values for those that don't pass qaqc
    qaqc_vec <- dat[, names(dat) %in% qaqc_sel, drop = F]
    qaqc_vec <- apply(qaqc_vec, 2, 
      function(x) grepl(paste(qaqc_rm, collapse = '|'), x)
      )
    #replace T values with NA
    #qaqc is corrected
    qaqc_sel <- gsub('f_', '', qaqc_sel)
    qaqc <- dat[, names(dat) %in% qaqc_sel, drop = F]
    qaqc <- data.frame(sapply(
      names(qaqc),
      function(x){
        out <- qaqc[, x]
        out[qaqc_vec[, paste0('f_',x)]] <- NA
        out
        },
      USE.NAMES = T
      ), stringsAsFactors = F)
    
    }
   
  ##
  # addl misc processing
  
  #combine with datetimestamp and append to output list
  out <- data.frame(datetimestamp = dat[,1], qaqc)
  
  #remove duplicate time stamps (some minutely), do not use aggregate
	out<-out[!duplicated(out$datetimestamp),]  
    
	#convert columns to numeric, missing converted to NA
	#NA values from qaqc still included as NA
	out <- data.frame(
    datetimestamp = out[,1],
    apply(out[, -1, drop = F], 2 , as.numeric)
    )

  # create swmpr class
  out <- swmpr(out, station)
  
  # return output
  if(trace) cat('\n\nQAQC processed...')
  return(out)

}

######
# subset a swmpr data object by a date range or parameter
# 'subset' is chr string of form 'YYYY-mm-dd HH:MM', or using POSIX terms '%Y-%m-%d %H:%M'
# 'select' is chr string of parameters to keep
# 'operator' is chr string specifiying binary operator (e.g., >, <=) if 'subset' is one date value
# 'rem_rows' is logical indicating if rows w/ no data are removed, default F
# 'rem_cols' is logical indicating if cols w/ no data are removed, default = F
subset.swmpr <- function(swmpr_in, subset = NULL, select = NULL, 
  operator = NULL, rem_rows = F, rem_cols = F){
  
  ##
  # swmpr data and attributes
  dat <- swmpr_in$station_data
  station <- attr(swmpr_in, 'station')
  timezone <- attr(swmpr_in, 'timezone')
  parameters <- attr(swmpr_in, 'parameters')
  qaqc_cols <- attr(swmpr_in, 'qaqc_cols')
  stamp_class <- attr(swmpr_in, 'stamp_class')
  
  ##
  # subset
  
  # create posix object from subset input
  date_sel <- rep(T, nrow(dat)) # created based on subset arg
  if(!is.null(subset)){

    subset <- as.POSIXct(subset, format = '%Y-%m-%d %H:%M', tz = timezone)
    
    # convert subset to date if input datetimestamp is date
    if('Date' %in% stamp_class)
      subset <- as.Date(subset, tz = timezone)
    
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
  else select <- names(dat)[names(dat) %in% c('datetimestamp', select, paste0('f_', select))]
  
  # subset data
  out <- subset(dat, date_sel, select)
  out <- data.frame(out, row.names = 1:nrow(out))
  
  ##
  # remove empty rows
  if(rem_rows){
    
    # get vector of rows that are empty
    col_sel <- grepl(paste(parameters, collapse = '|'), names(out))
    check_empty <- t(apply(as.matrix(out[, col_sel, drop = F]), 1, is.na))
    if(nrow(check_empty) == 1) check_empty <- matrix(check_empty)
    check_empty <- rowSums(check_empty) == ncol(check_empty)
    
    # remove empty rows
    out <- out[!check_empty, , drop = F]
    
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
    out <- out[, !check_empty, drop = F]
    
    if(ncol(out) == 1) stop('All data removed, select different parameters')
    
    }
  
  # create swmpr class
  out <- swmpr(out, station)
  
  # return output
  return(out)
  
}

######
# create a continous time vector at set time step for a swmpr object
# 'swmpr_in' is swmpr object
# 'timestep' numeric value of time step to use in minutes
# 'differ' is buffer for merging time stamps to standardized time series
setstep <- function(x, ...) UseMethod('setstep')
setstep.swmpr <- function(swmpr_in, timestep = 30, differ= timestep/2){ 

  library(data.table)
  library(plyr)  
  
  # swmpr data and attributes
  dat <- swmpr_in$station_data
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
  mrg_dat <- data.table(mrg_dat, key = 'datetimestamp')
  mrg_std <- data.table(dts_std, key = 'datetimestamp')
  
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
# combine data types for a station by common time series
# 'timestep' numeric value of time step to use in minutes, passed to setstep
# 'differ' is buffer for merging time stamps to standardized time series, passed to setstep
# 'method' is chr string indicating method of combining
#   * 'union' - all dates as continuous time series
#   * 'intersect' - areas of overlap
#   * 'station' - given station
comb <- function(...) UseMethod('comb')
comb.swmpr <- function(..., timestep = 30, differ= 5, method = 'union'){

  library(plyr)
  library(data.table) 
  
  # swmp objects list and attributes
  all_dat <- list(...)
  attrs <- llply(all_dat, attributes)
  
  ##
  # sanity checks
  if(length(all_dat) == 1)
    stop('Input data must include more than one swmpr object')
  
  # stop if from more than one timezone
  timezone <- unique(unlist(llply(attrs, function(x) x$timezone)))
    
  if(length(timezone) > 1)
    stop('Input data are from multiple timezones')
  
  # stop of method is invalid
  stations <- unlist(llply(attrs, function(x) x$station))
  
  if(!method %in% c('intersect', 'union', stations))
    stop('Method must be intersect, union, or station name')

  # stop if more than one data type
  types <- unlist(llply(attrs, function(x) x$param_types))
  
  if(any(duplicated(types))) 
    stop('Unable to combine duplicated data types')
  
  ##
  # setstep applied to data before combining
  all_dat <- llply(all_dat, setstep, timestep, differ)
  
  ##
  # dates
  date_vecs <- llply(all_dat, function(x) x[[1]]$datetimestamp)
  
  ## 
  # date vector for combining
  # for union, intersect
  if(method %in% c('union', 'intersect')){
    
    date_vec <- Reduce(method, date_vecs)
    date_vec <- as.POSIXct(date_vec, origin = '1970-01-01', tz = timezone)
    
  # for a station
  } else {
    
    sel <- unlist(llply(attrs, function(x) x$station == method))
    
    date_vec <- date_vecs[sel][[1]]
    
  }
    
  ##
  # merge stations by date_vec
  out <- data.table(datetimestamp = date_vec, key = 'datetimestamp')
  
  for(dat in all_dat){
    
    dat <- data.table(dat$station_data, key = 'datetimestamp')
    
    # merge
    out <- dat[out, roll = 'nearest']
 
    }

  # format as swmpr object, return
  out <- data.frame(out)
  out <- swmpr(out, stations)
  
  return(out)
  
}

########################
# evaluate functions
########################

######
# aggregate swmpr data by specified time period and method
# 'swmpr_in' is input swmpr object returned from any of the above functions
# 'by' is chr string of time period for aggregation 
#   * 'years'
#   * 'quarters'
#   * 'months' 
#   * 'weeks'
#   * 'days'
#   * 'hours' 
# 'FUN' is aggregation function, default mean
# 'params' are names of parameters to aggregate, default all
# 'na.action' is function for treating missing data, default na.pass
aggregate.swmpr <- function(swmpr_in, by, FUN = mean, params = NULL, 
  na.action = na.pass){

  library(data.table)
  
  # data
  to_agg <- swmpr_in$station_data

  # attributes
  timezone <- attr(swmpr_in, 'timezone')
  parameters <- attr(swmpr_in, 'parameters')
  station  <- attr(swmpr_in, 'station')
  
  # sanity checks
  if(any(!params %in% parameters))
    stop('Aggregation parameters must be present in data')
  if(attr(swmpr_in, 'qaqc_cols'))
    warning('QAQC columns present, removed in output')
  if(!by %in% c('years', 'quarters', 'months', 'weeks', 'days', 'hours'))
    stop('Unknown value for by, see help documentation')
    
  # create agg values from datetimestamp
  # as posix if hours, as date if other
  if(by == 'hours'){
    
    to_agg$datetimestamp <- as.POSIXct(
      strftime(to_agg$datetimestamp, '%Y-%m-%d %H', 
        tz = timezone), format = '%Y-%m-%d %H',
      tz = timezone)

  } else {
    
    if(by == 'days'){
      
      to_agg$datetimestamp <- as.Date(to_agg$datetimestamp,
        tz = timezone)
      
    } else {
      
      to_agg$datetimestamp <- round(
        as.IDate(to_agg$datetimestamp, tz = timezone),
        digits = by
      )
      
      to_agg$datetimestamp <- as.Date(to_agg$datetimestamp, tz = timezone)
      
    }
   
  }
  
  # subset by parameters
  if(!is.null(params)){ 
    to_agg <- to_agg[, names(to_agg) %in% c('datetimestamp', params)]
  } else {
    to_agg <- to_agg[, names(to_agg) %in% c('datetimestamp', parameters)]
  }

  # aggregate
  form_in <- formula(. ~ datetimestamp)
  out <- aggregate(form_in, to_agg, FUN = FUN, na.action = na.action,
    simplify = F)

  # format output as swmpr object
  out <- swmpr(out, station)
  
  # return output
  return(out)
  
}

######
# filter swmpr data
# 'swmpr_in' is input swmpr object returned from any of the above functions
filter.swmpr <- function(swmpr_in, filter, sides, ...){
  
  
}
