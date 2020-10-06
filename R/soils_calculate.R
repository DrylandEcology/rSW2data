
#' Deduce soil texture \var{iff} one of sand, clay, silt is missing
#'
#' @param x A \code{data.frame}. Soil horizons/layers are organized in rows
#'   and soil texture variables in columns.
#' @param var_stxt A vector of character strings. The names of the
#'   three columns representing sand, clay, and silt.
#' @param val_total A numerical value. The total value that the sum of
#'   sand, clay, and silt should be.
#' @param ignore_le A numerical value. Single missing values are not deduced
#'   if the two other values sum to less or equal to \code{ignore_le}.
#'
#' @examples
#' x <- data.frame(
#'   sand_pct = c(45.7, NA, 68.5, NA, 0, 2, NA),
#'   clay_pct = c(12.5, NA, NA, NA, 0, NA, 10),
#'   silt_pct = c(41.8, NA, 21.5, 15, NA, 1, 15)
#' )
#'
#' x2 <- deduce_complete_soil_texture(
#'   x = x,
#'   var_stxt = c("sand_pct", "clay_pct", "silt_pct"),
#'   val_total = 100,
#'   ignore_le = 5
#' )
#' x2[3, ] # previously missing clay was deduced to 10%
#' x2[5, ] # missing silt was not filled in because others sum to <= 5
#' x2[6, ] # missing clay was not filled in because others sum to <= 5
#' x2[7, ] # previously missing sand was deduced to 75%
#'
#' @export
deduce_complete_soil_texture <- function(
  x,
  var_stxt = c("sand", "clay", "silt"),
  val_total = 1,
  ignore_le = 0
) {

  cns <- colnames(x)
  for (var in var_stxt) {
    if (!(var %in% cns)) {
      stop(shQuote(var), " is a required column name, but cannot be found.")
    }
  }

  # Number of missing soil texture values
  tmp_n <- apply(x[, var_stxt, drop = FALSE], 1, function(x) sum(is.na(x)))
  ids_n1 <- tmp_n == 1

  if (sum(ids_n1) > 0) {
    # Sum of the two non-missing soil texture values
    tmp_s <- apply(x[ids_n1, var_stxt, drop = FALSE], 1, sum, na.rm = TRUE)

    # Don't deduce if one missing and the two others sum to <= ignore_le
    tmp_gti <- which(tmp_s > ignore_le)
    ids_n1gti <- which(ids_n1)[tmp_gti]

    if (length(ids_n1gti) > 0) {
      # Index of the one missing soil texture value
      tmp_nna <- which(
        is.na(x[ids_n1gti, var_stxt, drop = FALSE]),
        arr.ind = TRUE
      )
      tmp_nna <- tmp_nna[order(tmp_nna[, "row"]), , drop = FALSE]

      # Impute deduced soil texture value
      x[ids_n1gti, var_stxt][tmp_nna] <- pmax(
        0,
        pmin(val_total, val_total - tmp_s[tmp_gti])
      )
    }
  }

  x
}



#' Estimate soil bulk density
#'
#' @param theta_saturated A numeric vector. Saturated volumetric water
#'   content, i.e., at 0 \var{MPa}, in units of \var{cm3 / cm3}.
#' @param gravel_volume A numeric vector. Volume fraction of gravel/rock
#'   in units of \var{cm3 / cm3}
#'
#' @return Bulk density in units of \var{g / cm3}
#'
#' @references Saxton, K. E., and W. J. Rawls. 2006.
#' Soil water characteristic estimates by texture and organic matter for
#' hydrologic solutions. Soil Science Society of America Journal 70:1569–1578.
#'
#' @export
estimate_bulkdensity <- function(theta_saturated, gravel_volume) {
  2.65 * ((1 - theta_saturated) * (1 - gravel_volume) + gravel_volume)
}
