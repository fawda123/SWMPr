#' Parse web results for swmpr
#' 
#' Parsing function for objects returned from CDMO web services
#' 
#' @param  resp_in web object returned from CDMO server, response class from httr package
#' @param  parent_in chr string of parent nodes to parse
#' 
#' @import XML
#' 
#' @export
#' 
#' @details 
#' This function parses XML objects returned from the CDMO server, which are further passed to \code{\link{swmpr}}.  It is used internally by the data retrieval functions, excluding \code{\link{import_local}}.  The function does not need to be called explicitly.
#' 
#' @return Returns a \code{data.frame} of parsed XML nodes
parser <- function(resp_in, parent_in = 'data'){
  
  # convert to XMLDocumentContent for parsing
  raw <- xmlTreeParse(resp_in, useInternalNodes = TRUE)
  
  # get parent data nodes
  parents <- xpathSApply(
    raw,
    paste0('//', parent_in)
  )
  
  # get children nodes from data parents
  out <- lapply(parents, 
                function(x) getChildrenStrings(x)
  )
  out <- do.call('rbind', out)
  out <- data.frame(out)
  names(out) <- tolower(names(out))
  
  # error if ip address invalid
  if(grepl('^Invalid ip', out[1,1])){
    msg <- as.character(out[1,])
    msg <- paste0(msg, ', is it registered? http://cdmo.baruch.sc.edu/web-services-request/')
    stop(msg)
  }
  
  # return output
  return(out)
  
}