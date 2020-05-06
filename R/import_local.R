#' Import local CDMO data
#' 
#' Import local data that were obtained from the CDMO through the zip downloads feature
#' 
#' @param  path chr string of full path to .csv files with raw data, can be a zipped or unzipped directory where the former must include the .zip extension
#' @param  station_code chr string of station to import, typically 7 or 8 characters including wq, nut, or met extensions, may include full name with year, excluding file extension
#' @param  trace logical indicating if progress is sent to console, default \code{FALSE}
#' @param collMethd chr string of nutrient data to subset. 1 indicates monthly, 2 indicates diel. Default is both diel and monthly data.
#' 
#' @concept retrieve
#' 
#' @export
#' 
#' @importFrom utils read.csv unzip
#' 
#' @return Returns a swmpr object with all parameters and QAQC columns for the station.  The full date range in the raw data are also imported.
#' 
#' @details 
#' The function is designed to import local data that were downloaded from the CDMO outside of R. This approach works best for larger data requests, specifically those from the zip downloads feature in the advanced query section of the CDMO. The function may also work using data from the data export system, but this feature has not been extensively tested. The downloaded data will be in a compressed folder that includes multiple .csv files by year for a given data type (e.g., apacpwq2002.csv, apacpwq2003.csv, apacpnut2002.csv, etc.). The import_local function can be used to import files directly from the compressed folder or after the folder is decompressed.  In the former case, the requested files are extracted to a temporary directory and then deleted after they are loaded into the current session.  An example dataset is available online to illustrate the format of the data provided through the zip downloads feature.  See the link below to access these data.  All example datasets included with the package were derived from these raw data.
#' 
#' Occasionally, duplicate time stamps are present in the raw data.  The function handles duplicate entries differently depending on the data type (water quality,  weather, or nutrients).  For water quality and nutrient data, duplicate time stamps are simply removed.  Note that nutrient data often contain replicate samples with similar but not duplicated time stamps within a few minutes of each other.  Replicates with unique time stamps are not removed but can be further processed using \code{\link{rem_reps}}.  Weather data prior to 2007 may contain duplicate time stamps at frequencies for 60 (hourly) and 144 (daily) averages, in addition to 15 minute frequencies.  Duplicate values that correspond to the smallest value in the frequency column (15 minutes) are retained.  
#' 
#' Zip download request through CDMO: \url{http://cdmo.baruch.sc.edu/aqs/zips.cfm}
#' 
#' Example dataset: \url{https://s3.amazonaws.com/swmpexdata/zip_ex.zip}
#' 
#' @seealso \code{\link{all_params}}, \code{\link{all_params_dtrng}}, \code{\link{rem_reps}}, \code{\link{single_param}}
#' 
#' @examples
#' 
#' \dontrun{
#' ## this is the path for csv example files, decompressed
#' path <- 'C:/this/is/my/data/path'
#'
#' ## import, do not include file extension
#' import_local(path, 'apaebmet') 
#' 
#' ## this is the path for csv example files, zipped folder
#' path <- 'C:/this/is/my/data/path.zip'
#'
#' ## import, do not include file extension
#' import_local(path, 'apaebmet') 
#' }
import_local <- function(path, station_code, trace = FALSE, collMethd = c('1', '2')){
  
  # add .zip if not present
  if(file.exists(paste0(path, '.zip'))){
    path <- paste0(path, '.zip')
  }
  
  # check if file exists 
  if(!file.exists(path)){
    stop('Path does not exist')
  }
  
  # check if qualifiers are present in station_code
  if(!grepl('wq|met|nut', station_code))
    stop('station_code must include wq, met, or nut')
  
  # check if path is zipped
  zips <- grepl('\\.zip$', path)
  
  # remove file extension if present, lower case
  station_code <- tolower(gsub('\\.csv$', '', station_code))
  
  ##
  # find station files in path
  
  # for zipped
  if(zips){
    
    # get the file names in the zipped folder
    # check if the requested files exist
    file_nms <- unzip(path, list = TRUE)$Name
    expr <- paste0(station_code, '.*', '\\.csv$')
    files_in <- grep(expr, file_nms, value = TRUE, ignore.case = TRUE)
    if(length(files_in) == 0) stop('File(s) not found.')
    
    # extract to temporary file
    tmp_fl <- tempfile()
    unzip(path, files = files_in, exdir = tmp_fl)
    files_in <- dir(tmp_fl, recursive = TRUE)
    
    # reassign path to temporary file
    path <- tmp_fl
    
    # for unzipped    
  } else {
    
    file_nms <- dir(path)
    expr <- paste0('^', station_code, '.*', '\\.csv$')
    
  }
  
  files_in <- grep(expr, file_nms, value = TRUE, ignore.case = TRUE)
  if(length(files_in) == 0) stop('File(s) not found.')
  
  station_code <- tolower(station_code)
  
  # import all data files for a station
  dat <- vector('list', length(files_in))
  names(dat) <- gsub('.csv', '', files_in)
  
  if(trace) cat('Loading files...\n\n')
  
  for(file_in in files_in){
    
    if(trace) cat(file_in, '\t')
    
    ##
    # import
    
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
    
    # remove stationcode, isswmp columns
    tmp <- tmp[, !names(tmp) %in% c('stationcode', 'isswmp')]
    
    # convert date time to posix
    names(tmp)[grep('datetimestamp', names(tmp), ignore.case = TRUE)] <- 'datetimestamp'
    tmp$datetimestamp <- time_vec(tmp$datetimestamp, station_code)
    
    # append to output list
    nm <-  gsub('.csv', '', file_in)
    dat[[nm]] <- tmp
    
  }
  
  # remove temporary files if zips
  if(zips) unlink(tmp_fl, recursive = TRUE)
  
  ##
  # column names for each parameter type, used to subset combined data
  # kept as upper case here because improted data will match, changed to lower below
  
  # names to use
  parm <- substring(station_code, 6)
  parm <- gsub('[0-9.*]', '', parm)
  nms <- param_names(parm)[[parm]]
  
  ##
  # deal with duplicate time stamps depending on data type
  
  out <- do.call('rbind', dat)
  
  # if duplicated timestamps and met, keep those with minimum value in frequency
  if('met' %in% parm & any(duplicated(out$datetimestamp)) & 'frequency' %in% names(out)){
    
    min_step <- as.character(min(as.numeric(unique(out$frequency))))
    out <- out[out$frequency %in% min_step, ]
    
    # sometimes duplicates still remain at same frequency
    out <- out[!duplicated(out$datetimestamp),]  
    
  }
  
  # remove duplicate time stamps from wq and nut data
  if(any(c('nut', 'wq') %in% parm) & any(duplicated(out$datetimestamp))){
    
    out <- out[!duplicated(out$datetimestamp),]  
    
  }
  
  # remove rows with no datetimestamp
  out <- out[!is.na(out$datetimestamp), ]
  
  # if nut, filter for relevant nutrient data
  if(parm == 'nut'){
    if(length(unique(out$collmethd)) == 2){
      out <- out[out$collmethd %in% collMethd, ]
    }else{
      warning('This station does not have diel sampling data. All data will be retained.', call. = FALSE)
      out <- out
    }
  }
  
  # convert output to data frame
  # retain only relevant columns
  out <- data.frame(
    datetimestamp = out$datetimestamp,
    out[, names(out) %in% nms], 
    row.names = seq(1, nrow(out))
  )
  
  # make sure relevant columns are numeric
  parameters <- grep('datetimestamp|^f_|^c_', names(out), invert = TRUE, value = TRUE)
  out[, parameters] <- suppressWarnings(
    lapply(out[, parameters], function(x) as.numeric(as.character(x)))
  )
  
  # names as lower case
  names(out) <- tolower(names(out))
  
  # remove date from station_code, convert to swmpr class
  station_code <- gsub('[0-9]*$', '', station_code)
  out <- swmpr(out, station_code)
  
  if(trace) cat('\n\nData imported...')
  
  # return data frame
  return(out)
  
}