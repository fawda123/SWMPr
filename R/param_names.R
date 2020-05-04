#' Get parameters of a given type
#'
#' Get parameter column names for each parameter type
#' 
#' @param  param_type chr string specifying \code{'nut'}, \code{'wq'}, or \code{'met'}.  Input can be one to three types.
#' 
#' @export
#' 
#' @return Returns a named list of parameters for the \code{param_type}.  The parameter names are lower-case strings of SWMP parameters and corresponding qaqc names (\code{'f_'} prefix)
#' 
#' @details
#' This function is used internally within several functions to return a list of the expected parameters for a given parameter type: nutrients, water quality, or meteorological.  It does not need to be called explicitly. 
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
  out <- sapply(param_type, function(x) get(paste0(x, '_nms')), simplify = FALSE)
  
  return(out)
  
}