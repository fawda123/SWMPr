#' Decompose a time series
#' 
#' The function decomposes a time series into a long-term mean, annual, seasonal and "events" component. The decomposition can be multiplicative or additive, and based on median or mean centering. Function and documentation herein are from archived wq package.
#'
#' @param x a monthly time series vector
#' @param event whether or not an "events" component should be determined
#' @param type the type of decomposition, either multiplicative ("mult") or additive ("add")
#' @param center the method of centering, either median or mean
#' 
#' @details
#' The rationale for this simple approach to decomposing a time series, with examples of its application, is given by Cloern and Jassby (2010). It is motivated by the observation that many important events for estuaries (e.g., persistent dry periods, species invasions) start or stop suddenly. Smoothing to extract the annualized term, which can disguise the timing of these events and make analysis of them unnecessarily difficult, is not used.
#' 
#' A multiplicative decomposition will typically be useful for a biological community- or population-related variable (e.g., chlorophyll-a) that experiences exponential changes in time and is approximately lognormal, whereas an additive decomposition is more suitable for a normal variable. The default centering method is the median, especially appropriate for series that have large, infrequent events.
#' 
#' If \code{event = TRUE}, the seasonal component represents a recurring monthly pattern and the events component a residual series. Otherwise, the seasonal component becomes the residual series. The latter is appropriate when seasonal patterns change systematically over time. 
#' 
#' @seealso \code{\link{decomp_cj}}
#' 
#' @return
#' A monthly time series matrix with the following individual time series:
#' \item{original }{original time series}
#' \item{annual }{annual mean series}
#' \item{seasonal }{repeating seasonal component}
#' \item{events }{optionally, the residual or "events" series}
#' 
#' @references
#' Cloern, J.E. and Jassby, A.D. (2010) Patterns and scales of phytoplankton variability in estuarine-coastal ecosystems. \emph{Estuaries and Coasts} \bold{33,} 230--241.
decompTs <-
  function(x, event = TRUE, type = c("add", "mult"),
           center = c("mean", "median")) {
    
    # Validate input
    if (!is.ts(x) || !identical(frequency(x), 12)) {
      stop("x must be a monthly 'ts' vector")
    }
    type = match.arg(type)
    center = match.arg(center)
    
    # Set the time window
    startyr <- start(x)[1]
    endyr <- end(x)[1]
    x <- window(x, start = c(startyr, 1), end = c(endyr, 12), extend=TRUE)
    
    # Choose the arithmetic typeations, depending on type
    if (type == "mult") {
      `%/-%` <- function(x, y) x / y
      `%*+%` <- function(x, y) x * y
    } else {
      `%/-%` <- function(x, y) x - y
      `%*+%` <- function(x, y) x + y
    }
    
    # Choose the centering method, depending on center
    if (center == "median") {
      center <- function(x, na.rm=FALSE) median(x, na.rm=na.rm)
    } else {
      center <- function(x, na.rm=FALSE) mean(x, na.rm=na.rm)
    }
    
    # Long-term center
    grand <- center(x, na.rm=TRUE)
    
    # Annual component
    x1 <- x %/-% grand
    annual0 <- aggregate(x1, 1, center, na.rm=TRUE)
    annual1 <- as.vector(t(matrix(rep(annual0, 12), ncol=12)))
    annual <- ts(annual1, start=startyr, frequency=12)
    
    # Remaining components
    x2 <- x1 %/-% annual
    if (event) {
      # Seasonal component
      seasonal0 <- matrix(x2, nrow=12)
      seasonal1 <- apply(seasonal0, 1, center, na.rm=TRUE)
      seasonal <- ts(rep(seasonal1, endyr - startyr + 1), start=startyr,
                     frequency=12)
      # Events component
      x3 <- x2 %/-% seasonal
      # result
      ts.union(original=x, grand, annual, seasonal, events=x3)
    } else {
      ts.union(original=x, grand, annual, seasonal=x2)
    }
  }

#' @importFrom stats var end frequency is.ts start ts.union window
NULL
