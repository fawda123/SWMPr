#' Calculate oxygen mass transfer coefficient
#' 
#' Calculate oxygen mass transfer coefficient using equations in Thiebault et al. 2008.  Output is used to estimate the volumetric reaeration coefficient for ecosystem metabolism.
#'
#' @param temp numeric for water temperature (C)
#' @param sal numeric for salinity (ppt)
#' @param atemp numeric for air temperature (C)
#' @param wspd numeric for wind speed (m/s)
#' @param bp numeric for barometric pressure (mb)
#' @param height numeric for height of anemometer (meters)
#'
#' @import oce
#' 
#' @export
#' 
#' @return Returns numeric value for oxygen mass transfer coefficient (m d-1).
#' 
#' @details
#' This function is used within the \code{\link{ecometab}} function and should not be used explicitly.
#' 
#' @references
#' Ro KS, Hunt PG. 2006. A new unified equation for wind-driven surficial oxygen transfer into stationary water bodies. Transactions of the American Society of Agricultural and Biological Engineers. 49(5):1615-1622.
#' 
#' Thebault J, Schraga TS, Cloern JE, Dunlavey EG. 2008. Primary production and carrying capacity of former salt ponds after reconnection to San Francisco Bay. Wetlands. 28(3):841-851.
#' 
#' @seealso 
#' \code{\link{ecometab}}
#' 
calckl <- function(temp, sal, atemp, wspd, bp, height = 10){
  
  #celsius to kelvin conversion
  CtoK <- function(val) val + 273.15 
  
  Patm <- bp * 100; # convert from millibars to Pascals
  zo <- 1e-5; # assumed surface roughness length (m) for smooth water surface
  U10 <- wspd * log(10 / zo) / log(height / zo)
  tempK <- CtoK(temp)
  atempK <- CtoK(atemp)
  sigT <- swSigmaT(sal, temp, 10) # set for 10 decibars = 1000mbar = 1 bar = 1atm
  rho_w <- 1000 + sigT #density of SW (kg m-3)
  Upw <- 1.002e-3 * 10^((1.1709 * (20 - temp) - (1.827 * 10^-3 * (temp - 20)^2)) / (temp + 89.93)) #dynamic viscosity of pure water (sal + 0);
  Uw <- Upw * (1 + (5.185e-5 * temp + 1.0675e-4) * (rho_w * sal / 1806.55)^0.5 + (3.3e-5 * temp + 2.591e-3) * (rho_w * sal / 1806.55))  # dynamic viscosity of SW
  Vw <- Uw / rho_w  #kinematic viscosity
  Ew <- 6.112 * exp(17.65 * atemp / (243.12 + atemp))  # water vapor pressure (hectoPascals)
  Pv <- Ew * 100 # Water vapor pressure in Pascals
  Rd <- 287.05  # gas constant for dry air ( kg-1 K-1)
  Rv <- 461.495  # gas constant for water vapor ( kg-1 K-1)
  rho_a <- (Patm - Pv) / (Rd * atempK) + Pv / (Rv * tempK)
  kB <- 1.3806503e-23 # Boltzman constant (m2 kg s-2 K-1)
  Ro <- 1.72e-10     #radius of the O2 molecule (m)
  Dw <- kB * tempK / (4 * pi * Uw * Ro)  #diffusivity of O2 in water 
  KL <- 0.24 * 170.6 * (Dw / Vw)^0.5 * (rho_a / rho_w)^0.5 * U10^1.81  #mass xfer coef (m d-1)
  
  return(KL)
  
}