# args
path <- 'C:/Users/mbeck/Desktop/zip_download2/' 
Station_Code <- 'apadb'
year <- '2012'

# start function

# sanity checks
if(nchar(Station_Code) < 5) stop('Station_Code invalid.')

# find station files in path
file_nms <- dir(path)

expr <- paste0('^', station, '.*', year, '\\.csv$')
files_in <- grep(expr, file_nms, value = T)
  
if(length(file_in) == 0) stop('File(s) not found.')

# import all data files for a station
dat <- vector('list', length(files_in))
names(dat) <- gsub('.csv', '', files_in)

for(file_in in files_in){
  
  # import file, conver time vector
  tmp <- read.csv(file.path(path, file_in), stringsAsFactors = F)
  tmp$DateTimeStamp <- time_vec(tmp$DateTimeStamp, Station_Code)
  
  # append to output list
  nm <-  gsub('.csv', '', file_in)
  dat[[nm]] <- tmp
  
  }

# find date ranges for files
tz <- time_vec('01/01/2014 0:00', Station_Code, T)
time_rng <- llply(dat, .fun = function(x) range(x$DateTimeStamp))
time_rng <- range(unlist(time_rng))
time_rng <- as.POSIXct(time_rng, origin="1970-01-01 00:00", tz = tz)

#create continuous time vector (30 min ints)
times <- seq(time_rng[1], time_rng[2], by = 30*60)
times <- data.frame(DateTimeStamp = times)
