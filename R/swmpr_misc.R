#' Locations of NERRS sites
#'
#' Location of NERRS sites in decimal degress and time offset from Greenwich mean time.  Only active sites as of January 2015 are included.  Sites are identified by five letters indexing the reserve and site names.  The dataset is used to plot locations with the \code{\link{map_reserve}} function and to identify metabolic days with the \code{\link{ecometab}} function. 
#' 
#' @format A \code{\link[base]{data.frame}} object with 161 rows and 4 variables:
#' \describe{
#'   \item{\code{station_code}}{chr}
#'   \item{\code{latitude}}{numeric}
#'   \item{\code{longitude}}{numeric}
#'   \item{\code{gmt_off}}{int}
#' }
#' 
#' @seealso \code{\link{ecometab}}, \code{\link{map_reserve}}
"stat_locs"

######
#' Example nutrient data for Apalachicola Bay Cat Point station.
#'
#' An example nutrient dataset for Apalachicola Bay Cat Point station.  The data are a \code{\link{swmpr}} object that have been imported into R from csv files using the \code{\link{import_local}} function.  The raw data were obtained from the CDMO data portal but can also be accessed from a zip file created for this package.  See the source below.  The help file for the \code{\link{import_local}} function describes how the data can be imported from the zip file.  Attributes of the dataset include \code{names}, \code{row.names}, \code{class}, \code{station}, \code{parameters}, \code{qaqc_cols}, \code{date_rng}, \code{timezone}, and \code{stamp_class}. 
#'  
#' @format A \code{\link{swmpr}} object and \code{\link[base]{data.frame}} with 215 rows and 13 variables:
#' \describe{
#'   \item{\code{datetimestamp}}{POSIXct}
#'   \item{\code{po4f}}{num}
#'   \item{\code{f_po4f}}{chr}
#'   \item{\code{nh4f}}{num}
#'   \item{\code{f_nh4f}}{chr}
#'   \item{\code{no2f}}{num}
#'   \item{\code{f_no2f}}{chr}
#'   \item{\code{no3f}}{num}
#'   \item{\code{f_no3f}}{chr}
#'   \item{\code{no23f}}{num}
#'   \item{\code{f_no23f}}{chr}
#'   \item{\code{chla_n}}{num}
#'   \item{\code{f_chla_n}}{chr}
#' }
#' 
#' @source \url{https://s3.amazonaws.com/swmpexdata/zip_ex.zip}
#' 
#' @examples 
#' data(apacpnut)
"apacpnut"

######
#' Example water quality data for Apalachicola Bay Cat Point station.
#'
#' An example water quality dataset for Apalachicola Bay Cat Point station.  The data are a \code{\link{swmpr}} object that have been imported into R from csv files using the \code{\link{import_local}} function.  The raw data were obtained from the CDMO data portal but can also be accessed from a zip file created for this package.  See the source below.  The help file for the \code{\link{import_local}} function describes how the data can be imported from the zip file.  Attributes of the dataset include \code{names}, \code{row.names}, \code{class}, \code{station}, \code{parameters}, \code{qaqc_cols}, \code{date_rng}, \code{timezone}, and \code{stamp_class}. 
#'  
#' @format A \code{\link{swmpr}} object and \code{\link[base]{data.frame}} with 70176 rows and 25 variables:
#' \describe{
#'   \item{\code{datetimestamp}}{POSIXct}
#'   \item{\code{temp}}{num}
#'   \item{\code{f_temp}}{chr}
#'   \item{\code{spcond}}{num}
#'   \item{\code{f_spcond}}{chr}
#'   \item{\code{sal}}{num}
#'   \item{\code{f_sal}}{chr}
#'   \item{\code{do_pct}}{num}
#'   \item{\code{f_do_pct}}{chr}
#'   \item{\code{do_mgl}}{num}
#'   \item{\code{f_do_mgl}}{chr}
#'   \item{\code{depth}}{num}
#'   \item{\code{f_depth}}{chr}
#'   \item{\code{cdepth}}{num}
#'   \item{\code{f_cdepth}}{chr}
#'   \item{\code{level}}{num}
#'   \item{\code{f_level}}{chr}
#'   \item{\code{clevel}}{num}
#'   \item{\code{f_clevel}}{chr}
#'   \item{\code{ph}}{num}
#'   \item{\code{f_ph}}{chr}
#'   \item{\code{turb}}{num}
#'   \item{\code{f_turb}}{chr}
#'   \item{\code{chlfluor}}{num}
#'   \item{\code{f_chlfluor}}{chr}
#' }
#' 
#' @source \url{https://s3.amazonaws.com/swmpexdata/zip_ex.zip}
#'
#' @examples 
#' data(apacpwq)
"apacpwq"

######
#' Example water quality data for Apalachicola Bay Dry Bar station.
#'
#' An example water quality dataset for Apalachicola Bay Dry Bar station.  The data are a \code{\link{swmpr}} object that have been imported into R from csv files using the \code{\link{import_local}} function.  The raw data were obtained from the CDMO data portal but can also be accessed from a zip file created for this package.  See the source below.  The help file for the \code{\link{import_local}} function describes how the data can be imported from the zip file.  Attributes of the dataset include \code{names}, \code{row.names}, \code{class}, \code{station}, \code{parameters}, \code{qaqc_cols}, \code{date_rng}, \code{timezone}, and \code{stamp_class}. 
#'  
#' @format A \code{\link{swmpr}} object and \code{\link[base]{data.frame}} with 70176 rows and 25 variables:
#' \describe{
#'   \item{\code{datetimestamp}}{POSIXct}
#'   \item{\code{temp}}{num}
#'   \item{\code{f_temp}}{chr}
#'   \item{\code{spcond}}{num}
#'   \item{\code{f_spcond}}{chr}
#'   \item{\code{sal}}{num}
#'   \item{\code{f_sal}}{chr}
#'   \item{\code{do_pct}}{num}
#'   \item{\code{f_do_pct}}{chr}
#'   \item{\code{do_mgl}}{num}
#'   \item{\code{f_do_mgl}}{chr}
#'   \item{\code{depth}}{num}
#'   \item{\code{f_depth}}{chr}
#'   \item{\code{cdepth}}{num}
#'   \item{\code{f_cdepth}}{chr}
#'   \item{\code{level}}{num}
#'   \item{\code{f_level}}{chr}
#'   \item{\code{clevel}}{num}
#'   \item{\code{f_clevel}}{chr}
#'   \item{\code{ph}}{num}
#'   \item{\code{f_ph}}{chr}
#'   \item{\code{turb}}{num}
#'   \item{\code{f_turb}}{chr}
#'   \item{\code{chlfluor}}{num}
#'   \item{\code{f_chlfluor}}{chr}
#' }
#' 
#' @source \url{https://s3.amazonaws.com/swmpexdata/zip_ex.zip}
#' 
#' @examples 
#' data(apadbwq)
"apadbwq"

######
#' Example weather data for Apalachicola Bay East Bay station.
#'
#' An example weather dataset for Apalachicola Bay East Bay station.  The data are a \code{\link{swmpr}} object that have been imported into R from csv files using the \code{\link{import_local}} function.  The raw data were obtained from the CDMO data portal but can also be accessed from a zip file created for this package.  See the source below.  The help file for the \code{\link{import_local}} function describes how the data can be imported from the zip file.  Attributes of the dataset include \code{names}, \code{row.names}, \code{class}, \code{station}, \code{parameters}, \code{qaqc_cols}, \code{date_rng}, \code{timezone}, and \code{stamp_class}. 
#'  
#' @format A \code{\link{swmpr}} object and \code{\link[base]{data.frame}} with 70176 rows and 23 variables:
#' \describe{
#'   \item{\code{datetimestamp}}{POSIXct}
#'   \item{\code{atemp}}{num}
#'   \item{\code{f_atemp}}{chr}
#'   \item{\code{rh}}{num}
#'   \item{\code{f_rh}}{chr}
#'   \item{\code{bp}}{num}
#'   \item{\code{f_bp}}{chr}
#'   \item{\code{wspd}}{num}
#'   \item{\code{f_wspd}}{chr}
#'   \item{\code{maxwspd}}{num}
#'   \item{\code{f_maxwspd}}{chr}
#'   \item{\code{wdir}}{num}
#'   \item{\code{f_wdir}}{chr}
#'   \item{\code{sdwdir}}{num}
#'   \item{\code{f_sdwdir}}{chr}
#'   \item{\code{totpar}}{num}
#'   \item{\code{f_totpar}}{chr}
#'   \item{\code{totprcp}}{num}
#'   \item{\code{f_totprcp}}{chr}
#'   \item{\code{cumprcp}}{num}
#'   \item{\code{f_cumprcp}}{chr}
#'   \item{\code{totsorad}}{num}
#'   \item{\code{f_totsorad}}{chr}
#' }
#' 
#' @source \url{https://s3.amazonaws.com/swmpexdata/zip_ex.zip}
#'
#' @examples 
#' data(apaebmet)
"apaebmet"