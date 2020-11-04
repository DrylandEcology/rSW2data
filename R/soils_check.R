


#' NAs present but only in deepest soil layers
#'
#' Checks that NAs are present and that NAs occur only grouped together in the
#' right-most columns per row (e.g., deepest soil layers if columns represent
#' soil layers and rows represent sites).
#'
#' @param x A data.frame, matrix, or array with at least two dimensions.
#'
#' @return A logical vector of length equal to the first dimension of \code{x}
#'   with \code{TRUE} if there are n[k] \code{NA}s in the k-th row and they
#'   occupy the k rightmost columns.
#'
has_NAs_pooled_at_depth <- function(x) {
  stopifnot(!is.null(dim(x)))

  sapply(
    apply(x, 1, function(dat) rle(is.na(dat))),
    function(dat) length(dat$values) <= 2 && dat$values[length(dat$values)]
  )
}



#' Check validity of a table with soil layer depths
#'
#' The function runs four checks: \itemize{
#'   \item \code{soil_depth} agrees with the deepest soil layer.
#'   \item \code{n_layers} agrees with the number of soil layers.
#'   \item Soil layers are organized continuously and begin at the surface.
#'   \item Depths of soil layers are strictly monotonic increasing.
#'   \item Each site has at least one soil layer.
#' }
#'
#' @param table_depths A two-dimensional numeric matrix-like object.
#'   Rows represent sites and columns soil layers \var{1, ..., n}. Values
#'   represent the lower depth of a layer.
#' @param soil_depth A numeric vector. The soil depth at each site in the same
#'   units as \code{table_depths}. If missing, then calculated and not
#'   checked.
#' @param n_layers A numeric vector. The number of soil layers at each site.
#'   If missing, then calculated and not checked.
#'
#' @return If all checks succeed, then \code{TRUE}; otherwise, a named list with
#'   check results.
#'
#' @examples
#' soils <- matrix(
#'   data = c(
#'     5, 10, 50, 150,
#'     5, 10, 50, 200,
#'     5, 10, 50, 150,
#'     5, NA, 50, 150,
#'     5, 5, 50, 150,
#'     NA, NA, NA, NA
#'   ),
#'   nrow = 6,
#'   byrow = TRUE
#' )
#' sd_cm <- c(150, 150, 150, 150, 150, 0)
#' nlyrs <- c(4, 4, 3, 3, 4, 0)
#'
#' # This illustrates the warnings
#' soil_checks1 <- suppressWarnings(check_depth_table(
#'   table_depths = soils,
#'   soil_depth = sd_cm,
#'   n_layers = nlyrs
#' ))
#'
#' # This should be ok
#' soil_checks2 <- check_depth_table(
#'   table_depths = soils[1, , drop = FALSE],
#'   soil_depth = sd_cm[1],
#'   n_layers = nlyrs[1]
#' )
#'
#' stopifnot(soil_checks2)
#'
#' @export
check_depth_table <- function(table_depths, soil_depth, n_layers) {
  msg <- list()

  has_layer <- !is.na(table_depths)

  #--- Check soil depth
  calc_depth <- apply(
    X = table_depths,
    MARGIN = 1,
    function(x) {
      if (any(is.finite(x))) max(x, na.rm = TRUE) else 0
    }
  )

  if (!missing(soil_depth)) {
    tmp <- all.equal(soil_depth, calc_depth, check.attributes = FALSE)
    if (!isTRUE(tmp)) msg[["soil_depth"]] <- tmp
  }


  #--- Check that number of soil layers agree with existing layers
  calc_n_layers <- apply(has_layer, 1, sum)

  if (!missing(n_layers)) {
    tmp <- all.equal(n_layers, calc_n_layers, check.attributes = FALSE)
    if (!isTRUE(tmp)) msg[["n_layers"]] <- tmp
  }


  #--- Check that each site has at least one soil layer
  ids_N0 <- unname(which(calc_n_layers == 0))

  if (length(ids_N0) > 0) {
    msg[["ids_sites_without_soils"]] <- ids_N0
  }


  #--- Check that soil layers are continuous and starting at the surface
  sl_1region <- apply(
    X = has_layer,
    MARGIN = 1,
    FUN = function(x) {
      if (sum(x) > 0) {
        tmp <- rle(x)
        sum(tmp[["values"]]) == 1 && tmp[["values"]][1]

      } else {
        TRUE # nosoil
      }
    }
  )

  if (!all(sl_1region) || anyNA(sl_1region)) {
    msg[["sl_1region"]] <- sl_1region
  }


  #--- Check that soil layer depths are strictly monotonic increasing
  sl_monotonic <- try(
    rSW2utils::check_monotonic_increase(
      x = table_depths,
      strictly = TRUE,
      fail = TRUE,
      na.rm = TRUE
    ),
    silent = TRUE
  )

  if (inherits(sl_monotonic, "try-error")) {
    msg[["sl_monotonic"]] <- attr(sl_monotonic, "condition")[["message"]]
  }

  # Return TRUE if all checks pass or issue warning and return list of checks
  if (length(msg) > 0) {
    warning(
      "Soil depth table has problems: \n",
      paste("\t", names(msg), "=", msg, collapse = ";\n")
    )

    msg

  } else {
    TRUE
  }
}



#' Check availability of soil texture values for a specified number of layers
#'
#' @param table_texture A two-dimensional numeric object.
#'   Rows represent sites and columns soil texture variables for each
#'   soil layers \var{1, ..., n}. See examples.
#' @param n_layers A numeric vector. The number of soil layers at each site.
#' @param vars A vector of character strings. Soil texture variables to check
#'   as base name of the column names of \code{table_texture}. See examples.
#' @param vars_notzero A vector of character strings. Variables,
#'   as base name of the column names of \code{table_texture}, that are
#'   checked against being zero.
#'
#' @return A list with three elements
#'   \itemize{
#'     \item \var{checks_passed}:
#'       A logical value. \code{TRUE} if no missing values.
#'
#'     \item \var{missing}: See below
#'
#'     \item \var{zero}:
#'       See below; \code{NULL} if \code{vars_notzero} is \code{NULL}.
#'   }
#'
#'   The latter two elements are named lists each with elements:
#'   \itemize{
#'     \item \var{cond_N}:
#'       An integer n x p matrix for n sites and p variables
#'       (\code{vars} or \code{vars_notzero} respectively).
#'       The number of missing/zero values for a site \var{i} and
#'       variable \var{k} among the specified soil layers (\code{n_layers[i]}).
#'
#'     \item \var{is_cond_anylayer}:
#'       A logical n x p matrix for n sites and p variables
#'       (\code{vars} or \code{vars_notzero} respectively).
#'       \code{TRUE} for a site \var{i} if at least one value of
#'       variable \var{k} is missing/zero among the specified
#'       soil layers (\code{n_layers[i]}).
#'
#'     \item \var{is_cond_pctlayer}: \code{cond_N} divided by \code{n_layers}
#'
#'     \item \var{ids_sites_cond_anylayer}:
#'       An integer vector of site indices (row number in \code{table_texture})
#'       for which at least one variable in at least one layer is missing/zero.
#'
#'     \item \var{ids_sites_cond_alllayers}:
#'       An integer vector of site indices (row number in \code{table_texture})
#'       for which at least one variable is missing/zero in all layers.
#'
#'     \item \var{ids_sites_cond_somelayers}:
#'       \var{ids_sites_cond_anylayer} without
#'       \var{ids_sites_cond_alllayers}
#'   }
#'
#' @examples
#' soils <- data.matrix(data.frame(
#'   sand_L1 = c(0.828, 0.963, NA),
#'   clay_L1 = c(0.065, 0.03, 0.03),
#'   sand_L2 = c(0.57, NA, NA),
#'   clay_L2 = c(0.25, 0.03, 0.03),
#'   sand_L3 = c(0, NA, NA),
#'   clay_L3 = c(0, NA, NA)
#' ))
#' N_horizons <- rep(3, 3)
#'
#' texture_checks <- check_texture_table(
#'   table_texture = soils,
#'   n_layers = N_horizons,
#'   vars = c("sand", "clay"),
#'   vars_notzero = c("sand", "clay")
#' )
#'
#' # Does our soils table have no issues?
#' texture_checks[["checks_passed"]]
#'
#' if (!texture_checks[["checks_passed"]]) {
#'   #--- What are issues with missing data?
#'   check_missing <- texture_checks[["missing"]]
#'
#'   # How many sites have at least one missing value per variable?
#'   apply(check_missing[["is_cond_anylayer"]], 2, sum)
#'
#'   # Tabulate sites for number of layers with any missing sand values against
#'   # proportion of layers with any missing sand values
#'   addmargins(table(
#'     missing_pct = check_missing[["is_cond_pctlayer"]][, "sand"],
#'     missing_N = check_missing[["cond_N"]][, "sand"]
#'   ))
#'
#'   # Tabulate sites number of layers with only missing values against
#'   # proportion of layers with only missing values
#'   ids <- check_missing[["ids_sites_cond_alllayers"]]
#'   addmargins(table(
#'     missing_pct = check_missing[["is_cond_pctlayer"]][ids, ],
#'     missing_N = check_missing[["cond_N"]][ids, ]
#'   ))
#'
#'   #--- What are issues with zero data?
#'   check_zero <- texture_checks[["zero"]]
#'
#'   # How many sites have at least one zero value per variable?
#'   apply(check_zero[["is_cond_anylayer"]], 2, sum)
#' }
#'
#' @export
check_texture_table <- function(
  table_texture,
  n_layers,
  vars = c("sand", "clay"),
  vars_notzero = NULL
) {

  # Find missing values
  list_missing <- check_soillayer_condition(
    data = table_texture,
    n_layers = n_layers,
    vars = vars,
    fun = function(x) !is.finite(x)
  )

  res_missing <- aggregate_soillayer_condition(list_missing, n_layers)

  # Find zero values
  if (!is.null(vars_notzero)) {
    list_zeros <- check_soillayer_condition(
      data = table_texture,
      n_layers = n_layers,
      vars = vars_notzero,
      fun = function(x) abs(x) < sqrt(.Machine$double.eps)
    )

    res_zero <- aggregate_soillayer_condition(list_zeros, n_layers)

  } else {
    res_zero <- NULL
  }


  list(
    checks_passed =
      !any(res_missing[["is_cond_anylayer"]]) &&
      (is.null(res_zero) || !any(res_zero[["is_cond_anylayer"]])),
    missing = res_missing,
    zero = res_zero
  )
}


# Helper function for `check_texture_table()`
check_soillayer_condition <- function(data, n_layers, vars, fun) {
  res <- list()

  for (k in seq_along(vars)) {
    ivars <- grep(vars[k], colnames(data), ignore.case = TRUE)

    tmp <- apply(
      X = cbind(
        n_layers,
        data[, ivars]
      ),
      MARGIN = 1,
      function(x) {
        n <- length(x) - 1
        ids <- seq_len(min(n, x[1]))
        tmp <- rep(NA, n)
        tmp[ids] <- fun(x[1 + ids])
        tmp
      }
    )

    res[[vars[k]]] <- if (is.null(dim(tmp))) {
      matrix(tmp, ncol = 1)
    } else {
      t(tmp)
    }
  }

  res
}

# Helper function for `check_texture_table()`
aggregate_soillayer_condition <- function(x, n_layers) {
  res <- list()

  # Number of conditioned values per site
  res[["cond_N"]] <- sapply(
    x,
    function(x) apply(x, 1, sum, na.rm = TRUE)
  )

  res[["is_cond_anylayer"]] <- res[["cond_N"]] > 0

  # Percentage of conditioned values per site
  res[["is_cond_pctlayer"]] <- sweep(
    x = res[["cond_N"]],
    MARGIN = 1,
    STATS = n_layers,
    FUN = "/"
  )
  res[["is_cond_pctlayer"]][n_layers == 0] <- 0


  res[["ids_sites_cond_anylayer"]] <- which(
    apply(res[["is_cond_anylayer"]], 1, function(x) any(x))
  )

  res[["ids_sites_cond_alllayers"]] <- which(
    apply(res[["is_cond_pctlayer"]], 1, function(x) any(x == 1))
  )

  res[["ids_sites_cond_somelayers"]] <- setdiff(
    res[["ids_sites_cond_anylayer"]],
    res[["ids_sites_cond_alllayers"]]
  )


  res
}
