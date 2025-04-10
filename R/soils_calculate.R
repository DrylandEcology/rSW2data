
#' Deduce soil texture \var{iff} one of sand, clay, silt is missing
#'
#' @param x A \code{data.frame} or \code{matrix}.
#'   Soil horizons/layers are organized in rows
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
      stop(
        shQuote(var), " is a required column name, but cannot be found.",
        call. = FALSE
      )
    }
  }

  # Number of missing soil texture values
  tmp_n <- apply(x[, var_stxt, drop = FALSE], 1, function(x) sum(is.na(x)))
  ids_n1 <- tmp_n == 1

  if (sum(ids_n1) > 0) {
    # Sum of the two non-missing soil texture values
    tmp_s <- rowSums(x[ids_n1, var_stxt, drop = FALSE], na.rm = TRUE)

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



#' Replace missing values of a soil variables with a new value
#'
#'
#' @param x A \code{data.frame} or \code{matrix}.
#'   Soil horizons/layers are organized in rows and variables in columns.
#' @param variable A character string. The variable name, i.e.,
#'   one of the column names of \code{x}.
#' @param value The replacement value for missing values in
#'   \code{x[, variable]}.
#' @param where A character string. Method to specify how and where in the
#'   soil profile missing values of a variable should be replaced.
#'   Possible options are:
#'   \describe{
#'     \item{\var{"all"}}{
#'       Missing values are replaced by \code{value}.
#'     }
#'     \item{\var{"at_surface"}}{
#'       Missing values in the shallowest soil horizon
#'       are replaced by \code{value}.
#'       Note, remaining missing values in deeper horizons
#'       can subsequently be imputed, e.g.,
#'       \code{\link{impute_soils}}.
#'     }
#'     \item{\var{"none"}}{
#'        Missing values are set to \code{NA}.
#'     }
#'   }
#' @param horizon A character string or integer vector. Either a column name
#'   of \code{x} or a vector itself containing soil horizon/layer numbers that
#'   start at 1 in the most shallow/surface horizon/layer.
#' @param verbose A logical value.
#'
#' @return An updated copy of \code{x}.
#'
#' @section Details: Missing values are those that are not finite, i.e.,
#'   not one of \code{NA}, \code{NaN}, or \code{Inf}.
#'
#' @examples
#' x <- data.frame(
#'   coarse = c(NaN, 10, 50, Inf, 0, 15, NA),
#'   sand_pct = c(45.7, NA, 68.5, NA, 0, 2, NA),
#'   clay_pct = c(12.5, NA, NA, NA, 0, NA, 10),
#'   silt_pct = c(41.8, NA, 21.5, 15, NA, 1, 15)
#' )
#'
#' set_missing_soils_to_value(x, "coarse", where = "none")
#' set_missing_soils_to_value(x, "coarse", where = "all")
#' set_missing_soils_to_value(
#'   x = cbind(Layer_ID = seq_len(nrow(x)), x),
#'   variable = "coarse",
#'   where = "at_surface",
#'   horizon = "Layer_ID"
#' )
#' set_missing_soils_to_value(
#'   x,
#'   variable = "coarse",
#'   where = "at_surface",
#'   horizon = seq_len(nrow(x))
#' )
#'
#' @export
set_missing_soils_to_value <- function(
  x,
  variable,
  value = 0,
  where = c("none", "all", "at_surface"),
  horizon = colnames(x)[[1]],
  verbose = FALSE
) {

  where <- match.arg(where)
  stopifnot(variable %in% colnames(x))

  is_missing <- !is.finite(x[, variable])

  if (any(is_missing)) {
    if (where == "all") {
      x[is_missing, variable] <- value

    } else if (where == "at_surface") {

      k_horizon <- if (is.character(horizon)) {
        stopifnot(horizon %in% colnames(x))
        x[, horizon]

      } else if (is.numeric(horizon)) {
        stopifnot(length(horizon) == nrow(x))
        horizon

      } else {
        # nolint start: nonportable_path_linter.
        stop(
          "Argument `horizon` must be either a column name of `x` or ",
          "a numeric vector indicating soil horizon/layer numbers.",
          call. = FALSE
        )
        # nolint end
      }

      x[is_missing & k_horizon == 1, variable] <- value

    } else {
      x[is_missing, variable] <- NA
    }
  }

  n_missing <- sum(is_missing)

  if (
    verbose &&
      where %in% c("all", "at_surface") &&
      n_missing > 0L
  ) {
    message(
      "Missing values of variable ", shQuote(variable),
      " set to a value of ", value,
      " : n = ", n_missing
    )
  }

  x
}


#' Impute missing soil values per location
#'
#' Impute missing soil values per location by shallow-depth value carried
#' deeper (in analogy to \var{LOCF}), but do not impute missing values
#' in the shallowest horizon/layer.
#'
#' @param x A \code{data.frame} or \code{matrix}.
#'   Soil horizons/layers are organized in rows
#'   and soil texture variables in columns.
#' @param var_values A vector of character strings. The column names of \code{x}
#'   which contain the soil variables to be imputed.
#' @param var_site_id A character string. The column name of \code{x} which
#'   contains the unique location identifiers.
#' @param var_horizon A character string. The column name of \code{x} which
#'   contains the horizon/layer numbers where the shallowest horizon/layer
#'   is number one.
#' @param verbose A logical value.
#'
#' @return An updated version of \code{x}.
#'
#' @seealso \code{\link[rSW2utils]{impute_df}}
#'
#' @examples
#' x <- data.frame(
#'   id = rep(1, 7),
#'   layer_no = 1:7,
#'   coarse = c(NA, 10, 50, NA, 0, 15, NA),
#'   sand_pct = c(45.7, NA, 68.5, NA, 0, 2, NA),
#'   clay_pct = c(12.5, NA, NA, NA, 0, NA, 10),
#'   silt_pct = c(41.8, NA, 21.5, 15, NA, 1, 15)
#' )
#'
#' # Missing values in the shallowest layer are not imputed
#' impute_soils(
#'   x,
#'   var_values = c("coarse", "sand_pct"),
#'   var_site_id = "id",
#'   var_horizon = "layer_no"
#' )
#'
#' @export
impute_soils <- function(
  x,
  var_values,
  var_site_id = "COKEY",
  var_horizon = "Horizon_No",
  verbose = FALSE
) {

  cns <- colnames(x)
  for (var in c(var_values, var_site_id, var_horizon)) {
    if (!(var %in% cns)) {
      # nolint start: nonportable_path_linter.
      stop(
        shQuote(var),
        " is a required/requested column name, but cannot be found.",
        call. = FALSE
      )
      # nolint end
    }
  }


  if (verbose) {
    is_shallowest <- x[, var_horizon] == 1
    n_imped_vals <- sum(is.na(x[!is_shallowest, var_values]))
    is_imped_hzs <- apply(x[!is_shallowest, var_values, drop = FALSE], 1, anyNA)
    n_imped_hzs <- sum(is_imped_hzs)
    n_imped_cokeys <- length(
      unique(x[!is_shallowest, var_site_id][is_imped_hzs])
    )
  }


  # --- Sort by site then by horizon for imputation
  ids_order <- order(x[, var_site_id], x[, var_horizon])
  x <- x[ids_order, , drop = FALSE]


  # --- Impute
  tmp <- by(
    data = x[, var_values, drop = FALSE],
    INDICES = x[, var_site_id],
    FUN = function(x) {
      if (nrow(x) > 1) {
        x_shallowest <- x[1, ] # don't impute first/shallowest horizon

        x <- suppressMessages(
          rSW2utils::impute_df(x, imputation_type = "locf")
        )

        x[1, ] <- x_shallowest
      }

      x
    },
    simplify = FALSE
  )

  ids <- match(unique(x[, var_site_id]), names(tmp))
  x[, var_values] <- do.call(rbind, tmp[ids])


  if (verbose) {
    n_missing <- sum(is.na(x[, var_values]))

    # nolint start: nonportable_path_linter.
    message(
      "Imputed values for n = ", n_imped_cokeys, " locations ",
      "in n = ", n_imped_hzs, " soil horizons/layers ",
      "for n = ", n_imped_vals, " values",
      if (n_missing > 0) {
        paste0("; remaining missing values n = ", n_missing)
      } else {
        "."
      }
    )
    # nolint end
  }

  # --- Re-create input order of data and output
  x[match(seq_along(ids_order), ids_order), , drop = FALSE]
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




#' Calculate potential bare-soil evaporation coefficients
#'
#' Soil texture influence based on re-analysis of data from Wythers et al. 1999.
#' Default of \code{depth_max_bs_evap} = 15 cm from Torres et al. 2010.
#'
#' @references Torres EA, Calera A (2010) Bare soil evaporation under high
#'   evaporation demand: a proposed modification to the FAO-56 model.
#'   Hydrological Sciences Journal- Journal des Sciences Hydrologiques, 55,
#'   303-315.
#'
#' @references Wythers K.R., Lauenroth W.K., Paruelo J.M. (1999) Bare-Soil
#'   Evaporation Under Semiarid Field Conditions. Soil Science Society of
#'   America Journal, 63, 1341-1349.
#'
#' @param layers_depth A numeric vector, matrix, or data.frame. Values describe
#'   the lower soil layer depths in units of centimeters.
#' @param sand A numeric vector, matrix, or data.frame. Values are sand contents
#'   in units of mass-percentage / 100.
#' @param clay A numeric vector, matrix, or data.frame. Values are clay contents
#'   in units of mass-percentage / 100.
#' @param depth_max_bs_evap_cm A numeric value. The maximal soil depth in
#'   centimeters from which bare-soil evaporation is potentially drawing
#'   moisture.
#' @param method_bad_soils A character string. The method that determines how
#'   the function behaves if inputs represent "bad" soils, e.g., soil properties
#'   are missing or values out of range: \itemize{
#'     \item "stop" will stop the function with an error;
#'     \item "pass" will return \code{NA} for the sites with bad inputs.
#'   }
#'
#' @section Notes: Rows of soil input arguments \code{layers_depth},
#'   \code{sand}, and \code{clay} correspond to sites and columns to soil
#'   layers. If \code{sand} and/or \code{clay} are vectors, then they are
#'   converted to 1-row matrices. If \code{layers_depth} is a vector, then it is
#'   converted to a matrix with as many sites/rows as \code{sand} and
#'   \code{clay} have. That is the code assumes identical soil layer depths for
#'   each site. All soil input arguments must have the same number of sites
#'   and of soil layers, i.e., identical matrix dimensions.
#' @section Warning: Influence of gravel is not accounted for.
#'
#' @return A numeric matrix with potential bare-soil evaporation coefficients
#'   where rows correspond to sites and columns to soil layers.
#'
#' @examples
#' sw_soils1 <- data.frame(
#'   depth_cm = c(5, 10, 20, 30, 40, 60, 80, 100),
#'   sand_frac = c(0.51, 0.44, 0.35, 0.32, 0.31, 0.32, 0.57, 0.57),
#'   clay_frac = c(0.15, 0.26, 0.41, 0.45, 0.47, 0.47, 0.28, 0.28)
#' )
#'
#' calc_BareSoilEvapCoefs(
#'   layers_depth = sw_soils1[, "depth_cm"],
#'   sand = sw_soils1[, "sand_frac"],
#'   clay = sw_soils1[, "clay_frac"]
#' )
#'
#' sw_soils2 <- list(
#'   depth_cm = t(data.frame(
#'     site1 = c(5, 10, 20),
#'     site2 = c(5, 10, 15)
#'   )),
#'   sand = t(data.frame(
#'     site1 = c(0.50, 0.40, 0.30),
#'     site2 = c(0.25, 0.30, 0.35)
#'   )),
#'   clay = t(data.frame(
#'     site1 = c(0.20, 0.20, 0.25),
#'     site2 = c(0.15, 0.25, 0.25)
#'   ))
#' )
#'
#' calc_BareSoilEvapCoefs(
#'   layers_depth = sw_soils2[["depth_cm"]],
#'   sand = sw_soils2[["sand"]],
#'   clay = sw_soils2[["clay"]],
#' )
#'
#' @export
calc_BareSoilEvapCoefs <- function(
  layers_depth,
  sand,
  clay,
  depth_max_bs_evap_cm = 15,
  method_bad_soils = c("stop", "pass")
) {

  #--- Process inputs
  depth_max_bs_evap_cm <- depth_max_bs_evap_cm[[1]]
  stopifnot(
    is.finite(depth_max_bs_evap_cm),
    depth_max_bs_evap_cm >= 0
  )

  method_bad_soils <- match.arg(method_bad_soils)


  #--- If inputs are not site x layers, then convert them into 1 site x layers
  if (is.null(dim(sand))) {
    sand <- matrix(sand, nrow = 1, ncol = length(sand))
  }
  if (is.null(dim(clay))) {
    clay <- matrix(clay, nrow = 1, ncol = length(clay))
  }
  if (is.null(dim(layers_depth))) {
    layers_depth <- matrix(
      data = layers_depth,
      nrow = dim(sand)[[1]],
      ncol = length(layers_depth),
      byrow = TRUE
    )
  }

  #--- Fail if dimensions disagree
  # * sand and clay have identical number of sites and layers
  # * all soil inputs have identical number of sites
  stopifnot(
    identical(dim(sand), dim(clay)),
    identical(nrow(sand), nrow(layers_depth))
  )


  #--- Prepare outputs
  res <- array(NA, dim = dim(sand))


  #--- Identify layers and groups of sites with the same profile (soil layers)
  avail_sl_ids <- apply(layers_depth, 1, paste0, collapse = "x")
  layer_sets <- unique(avail_sl_ids)


  #--- Loop through sites with same layer profile and make adjustments
  for (k1 in seq_along(layer_sets)) {
    idsls <- which(avail_sl_ids == layer_sets[k1])

    if (length(idsls) == 0) next

    #--- Grab soil properties of current set of soil layers
    ls_ld <- layers_depth[idsls, , drop = FALSE]
    ls_sand <- sand[idsls, , drop = FALSE]
    ls_clay <- clay[idsls, , drop = FALSE]


    #--- Soil layers potentially affected by bare-soil evaporation
    n_slyrs <- if (
      !anyNA(ls_ld) && all(ls_ld[1, ] <= depth_max_bs_evap_cm)
    ) {
      # All layers less deep than depth_max_bs_evap_cm
      ncol(ls_ld)
    } else {
      # No missing values allowed in layers less deep than depth_max_bs_evap_cm
      tmp <- cumsum(diff(c(0, ls_ld[1, ]))) >= depth_max_bs_evap_cm
      if (any(tmp, na.rm = TRUE)) {
        which(tmp)[[1]]
      } else {
        # all layers less deep than depth_max_bs_evap_cm
        # -> left set of non-NA
        # but fail if NA mixed in with real depth values
        tmp2 <- rle(!is.na(ls_ld[1, ]))
        if (isTRUE(tmp2[["values"]][[1]]) && sum(tmp2[["values"]]) == 1) {
          tmp2[["lengths"]][[1]]
        }
      }
    }

    stopifnot(!is.null(n_slyrs), !anyNA(n_slyrs), n_slyrs > 0)
    ids_slyrs <- seq_len(n_slyrs)


    #--- Fail if soil inputs have not enough layers within depth_max_bs_evap_cm
    stopifnot(ncol(ls_sand) >= length(ids_slyrs))

    #--- Subset soil layers to those within depth_max_bs_evap_cm
    ls_sand <- ls_sand[, ids_slyrs, drop = FALSE]
    ls_clay <- ls_clay[, ids_slyrs, drop = FALSE]
    ls_ld <- ls_ld[, ids_slyrs, drop = FALSE]


    #--- Deal with sites with "bad" soil inputs
    # Check that depths are positive
    is_good_depth <- ls_ld > 0 & !apply(ls_ld, 1, anyNA)

    # Check that sand, clay, and their sum are within 0-1
    is_good_sand <- ls_sand >= 0 & ls_sand <= 1 & !apply(ls_sand, 1, anyNA)
    is_good_clay <- ls_clay >= 0 & ls_clay <= 1 & !apply(ls_clay, 1, anyNA)
    is_good_sandclay <- ls_sand + ls_clay <= 1


    ids_used <- which(apply(
      is_good_depth & is_good_sand & is_good_clay & is_good_sandclay,
      MARGIN = 1,
      function(x) all(x) && !anyNA(x)
    ))

    if (length(ids_used) != length(idsls) && method_bad_soils == "stop") {
      stop("Sites with bad soils.", call. = FALSE)
    }

    if (length(ids_used) == 0) next

    ls_ld_used <- ls_ld[ids_used, , drop = FALSE]


    #--- Calculate
    depth_min_bs_evap <- min(ls_ld_used[, 1], na.rm = TRUE)

    if (depth_min_bs_evap > depth_max_bs_evap_cm) {
      # all good sites have first layer with coeff = 1
      res[idsls[ids_used], 1] <- 1
      res[idsls[ids_used], -1] <- 0
      next
    }

    lyrs_max_bs_evap <- apply(
      X = ls_ld_used,
      MARGIN = 1,
      FUN = function(x) {
        xdm <- depth_max_bs_evap_cm - x
        i0 <- abs(xdm) < rSW2_glovars[["tol"]]
        ld <- if (any(i0, na.rm = TRUE)) {
          which(i0)
        } else {
          tmp <- which(xdm < 0)
          if (length(tmp) > 0) tmp[[1]] else length(x)
        }
        c(diff(c(0, x))[seq_len(ld)], rep(0L, length(x) - ld))
      }
    )

    lyrs_max_bs_evap <- if (is.null(dim(lyrs_max_bs_evap))) {
      matrix(lyrs_max_bs_evap, ncol = 1)
    } else {
      t(lyrs_max_bs_evap)
    }

    ldepth_max_bs_evap <- rowSums(lyrs_max_bs_evap)

    sand_mean <-
      rowSums(lyrs_max_bs_evap * ls_sand[ids_used, , drop = FALSE]) /
      ldepth_max_bs_evap
    clay_mean <-
      rowSums(lyrs_max_bs_evap * ls_clay[ids_used, , drop = FALSE]) /
      ldepth_max_bs_evap

    # equation from re-analysis
    tmp_depth <- 4.1984 + 0.6695 * sand_mean^2 + 168.7603 * clay_mean^2

    depth_bs_evap <- pmin(
      pmax(tmp_depth, depth_min_bs_evap),
      depth_max_bs_evap_cm
    )

    lyrs_bs_evap0 <- apply(
      X = depth_bs_evap - ls_ld_used,
      MARGIN = 1,
      FUN = function(x) {
        i0 <- abs(x) < rSW2_glovars[["tol"]]
        ld <- if (any(i0)) {
          which(i0)
        } else {
          tmp <- which(x < 0)
          if (length(tmp) > 0) tmp[[1]] else sum(!is.na(x))
        }
        ld0 <- max(0, ld - 1)

        c(rep(TRUE, ld0), rep(FALSE, length(x) - ld0))
      }
    )

    lyrs_bs_evap0 <- if (is.null(dim(lyrs_bs_evap0))) {
      matrix(lyrs_bs_evap0, ncol = 1)
    } else {
      t(lyrs_bs_evap0)
    }

    # function made up to match previous cumulative distributions
    tmp_coeff <- 1 - exp(-5 * ls_ld_used / depth_bs_evap)
    tmp_coeff[!lyrs_bs_evap0 | is.na(tmp_coeff)] <- 1
    tmpc <- apply(cbind(0, tmp_coeff), 1, diff)
    tmpc <- if (is.null(dim(tmpc))) matrix(tmpc, ncol = 1) else t(tmpc)
    coeff_bs_evap <- round(tmpc, 4)

    res[idsls[ids_used], ids_slyrs] <-
      coeff_bs_evap / rowSums(coeff_bs_evap, na.rm = TRUE)
    res[idsls[ids_used], -ids_slyrs] <- 0
  }

  res
}


#' Crude initial estimate of soil temperatures throughout the soil profile
#'
#' @param layers_depth A numeric vector. Values describe
#'   the lower soil layer depths in units of centimeters at which soil
#'   temperatures are estimated.
#' @param Tsoil_upper A numeric value. Initial soil temperature [C] at the
#'   upper boundary, i.e., soil surface.
#' @param Tsoil_const A numeric value. Initial soil temperature [C] at the
#'   lower boundary, i.e., the soil depth at which soil temperature is mostly
#'   constant throughout a year.
#' @param depth_Tsoil_const A numeric value. The soil depth [cm] for which
#'   \code{Tsoil_const} is valid.
#'
#' @return A numeric vector of length corresponding to \code{layers_depth}
#'
#' @examples
#' init_soiltemperature(
#'   layers_depth = c(5, 10, 20, 30, 40, 60, 80, 100),
#'   # soil surface underneath a snow pack on Jan 1
#'   Tsoil_upper = -1,
#'   # mean annual air temperature is a reasonable estimate
#'   Tsoil_const = 8,
#'   depth_Tsoil_const = 990
#' )
#'
#' @export
init_soiltemperature <- function(
  layers_depth,
  Tsoil_upper,
  Tsoil_const,
  depth_Tsoil_const = 990
) {

  # nolint start: object_usage_linter.
  sl <- c(0, depth_Tsoil_const)
  st <- c(Tsoil_upper, Tsoil_const)
  # nolint end

  stats::predict(stats::lm(st ~ sl), data.frame(sl = layers_depth))
}
