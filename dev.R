##
# import, qaqc data from path

path <- 'C:/Users/mbeck/Desktop/zip_download2'
Station_Code <- 'apacpwq'

dat <- import_local(path, Station_Code, T)
tmp <- qaqc_local(dat, NULL)


