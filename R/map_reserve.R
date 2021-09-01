#' Map a reserve
#' 
#' Create a map of all the stations in a reserve
#' 
#' @param nerr_site_id chr string of the reserve to map, first three characters used by NERRS or vector of stations to map using the first five characters
#' @param text_sz numeric value for text size of station names, passed to \code{\link[ggplot2]{geom_text}}
#' @param text_col chr string for text color of station names, passed to \code{\link[ggplot2]{geom_text}}
#' @param f numeric passed to \code{\link[ggmap]{make_bbox}} as expansion around the bounding box for stations at a NERR site, increase to expand map zoom chr 
#' 
#' @import ggplot2
#' 
#' @concept analyze
#' 
#' @export
#' 
#' @details This function is a simple wrapper to functions in the ggmap package which returns a map of all of the stations at a NERRS reserve.  The \code{f} argument may have to be chosen through trial and error depending on the spatial extent of the reserve.  A local data file included with the package is used to get the latitude and longitude values of each station.  The files includes only active stations as of January 2015.
#' 
#' @return A \code{\link[ggplot2]{ggplot}} object for plotting.
#' 
#' @seealso  \code{\link[ggmap]{get_map}}, \code{\link[ggmap]{ggmap}}, \code{\link[ggplot2]{ggplot}}
#' 
#' @examples
#' \dontrun{
#' map_reserve('jac')
#'}
map_reserve <- function(nerr_site_id, text_sz = 6, text_col = 'black', f = 0.2){
  
  # sanity check
  if(nchar(nerr_site_id) != 3)
    stop('nerr_site_id must be three characters')

  # subset stat_locs by reserve
  dat_locs <- get('stat_locs')
  stats <- paste(paste0('^', nerr_site_id), collapse = '|')
  stats <- dat_locs[grepl(stats, dat_locs$station_code), ]
  
  if(requireNamespace('ggmap', quietly = TRUE)){
    
    # base map
    mapImageData <- suppressMessages(
      ggmap::get_map(
        location = ggmap::make_bbox(lon = stats$longitude ,lat = stats$latitude, f = f),
        maptype = 'terrain'
      )
    )
    
    # plot
    p <- ggmap::ggmap(mapImageData,
                      extent = 'panel'
    ) + 
      geom_text(data = stats, aes_string(x = 'longitude', y = 'latitude', 
                                         label= 'station_code'), size = text_sz, colour = text_col
      ) +
      ylab('Latitude') +
      xlab('Longitude')
    
  }
  
  return(p)
  
}