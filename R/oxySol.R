#' Dissolved oxygen at saturation
#'
#' Finds dissolved oxygen concentration in equilibrium with water-saturated air. Function and documentation herein are from archived wq package.
#'
#' @param t numeric for temperature, degrees C
#' @param S numeric for salinity, on the Practical Salinity Scale
#' @param P numeric for pressure, atm
#'
#' @details Calculations are based on the approach of Benson and Krause (1984), using Green and Carritt's (1967) equation for dependence of water vapor partial pressure on \code{t} and \code{S}. Equations are valid for temperature in the range 0-40 C and salinity in the range 0-40.
#'
#' @export
#' 
#' @return Dissolved oxygen concentration in mg/L at 100\% saturation. If \code{P = NULL}, saturation values at 1 atm are calculated.
#'
#' @references
#' Benson, B.B. and Krause, D. (1984) The concentration and isotopic fractionation of oxygen dissolved in fresh-water and seawater in equilibrium with the atmosphere. \emph{Limnology and Oceanography} \bold{29,} 620-632.
#'
#' Green, E.J. and Carritt, D.E. (1967) New tables for oxygen saturation of seawater. \emph{Journal of Marine Research} \bold{25,} 140-147.
oxySol <- function (t, S, P = NULL)
{
  T = t + 273.15
  lnCstar = -139.34411 + 157570.1/T - 66423080/T^2 + 1.2438e+10/T^3 -
    862194900000/T^4 - S * (0.017674 - 10.754/T + 2140.7/T^2)
  Cstar1 <- exp(lnCstar)
  if (is.null(P)) {
    Cstar1
  }
  else {
    Pwv = (1 - 0.000537 * S) * exp(18.1973 * (1 - 373.16/T) +
                                     3.1813e-07 * (1 - exp(26.1205 * (1 - T/373.16))) -
                                     0.018726 * (1 - exp(8.03945 * (1 - 373.16/T))) +
                                     5.02802 * log(373.16/T))
    theta = 0.000975 - 1.426e-05 * t + 6.436e-08 * t^2
    Cstar1 * P * (1 - Pwv/P) * (1 - theta * P)/((1 - Pwv) *
                                                  (1 - theta))
  }
}