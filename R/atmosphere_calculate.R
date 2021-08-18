#' Saturation vapor pressure
#'
#' @param x A numeric vector of temperature \var{[C]}
#'
#' @return Saturation vapor pressure \var{[kPa]}
#'
#' @references Yoder, R. E., L. O. Odhiambo, and W. C. Wright. 2005. Effects of
#'   Vapor-Pressure Deficit and Net-Irradiance Calculation Methods on Accuracy
#'   of Standardized Penman-Monteith Equation in a Humid Climate Journal of
#'   Irrigation and Drainage Engineering 131:228-237.
#'
#' @export
vp0 <- function(x) {
  0.6108 * exp(17.27 * x / (x + 273.3)) # eq. 5 of Yoder et al. 2005
}


#' Vapor pressure deficit
#'
#' @param Tmin A numeric vector of daily minimum temperature \var{[C]}
#' @param Tmax A numeric vector of daily maximum temperature \var{[C]}
#' @param RHmean A numeric vector of
#'   daily mean relative humidity \var{[0-100\%]}
#'
#' @return Vapor pressure deficit \var{[kPa]}
#'
#' @references Yoder, R. E., L. O. Odhiambo, and W. C. Wright. 2005. Effects of
#'   Vapor-Pressure Deficit and Net-Irradiance Calculation Methods on Accuracy
#'   of Standardized Penman-Monteith Equation in a Humid Climate Journal of
#'   Irrigation and Drainage Engineering 131:228-237.
#'
#' @export
vpd <- function(Tmin, Tmax, RHmean = NULL) {
  if (is.null(RHmean)) {
    # eq. 6 - eq. 13 of Yoder et al. 2005 (VPD6 in Table 4)
    (vp0(Tmax) - vp0(Tmin)) / 2
  } else {
    # eq. 6 - eq. 11 of Yoder et al. 2005 (VPD4 in Table 4)
    (vp0(Tmax) + vp0(Tmin)) / 2 * (1 - RHmean / 100)
  }
}
