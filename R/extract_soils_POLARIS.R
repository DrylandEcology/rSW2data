create_reference_for_POLARIS <- function() {
  paste0(
    "Chaney, N. W., B. Minasny, J. D. Herman, T. W. Nauman, C. Brungard, ",
    "C. L. S. Morgan, A. B. McBratney, E. F. Wood, and Y. T. Yimam. 2019. ",
    "POLARIS soil properties: 30-meter probabilistic maps of soil properties ",
    "over the contiguous United States. ",
    "Water Resources Research 55:2916-2938. ",
    "https://doi.org/10.1029/2018WR022797. ",
    "Accessed [",
    format(as.POSIXlt(Sys.Date()), "%Y-%b-%e"),
    "]"
  )
}


#' Create \var{wget} script to download soil data files from \var{POLARIS}
#'
#' This function only writes out a text file to disk. The user
#' is responsible to run the script after making sure that \var{wget} is
#' available and the script is executable.
#'
#' @param path A character string. The path to where the local copy of the
#'   \var{POLARIS} folder hierarchy and files should be downloaded.
#' @param version A character string. The \var{POLARIS} release version.
#' @param vars A vector of character strings. See Chaney et al. 2019
#' @param stat A vector of character strings. See Chaney et al. 2019
#'
#' @return (Invisibly) the file path of the generated bash script.
#'
#' @references
#'  Chaney, N. W., B. Minasny, J. D. Herman, T. W. Nauman, C. Brungard,
#'  C. L. S. Morgan, A. B. McBratney, E. F. Wood, and Y. T. Yimam. 2019.
#'  POLARIS soil properties: 30-meter probabilistic maps of soil properties
#'  over the contiguous United States. Water Resources Research 55:2916-2938.
#'  \url{https://doi.org/10.1029/2018WR022797}.
#'
#' @examples
#' fname_wget_polaris <- prepare_script_for_POLARIS()
#'
#' ## in a shell
#' ## give execute permission if needed: chmod +x <fname_wget_polaris>
#' ## download data: ./<fname_wget_polaris>
#'
#' unlink(fname_wget_polaris)
#'
#' @export
prepare_script_for_POLARIS <- function(
  path = ".",
  version = "v1.0",
  vars = c("bd", "sand", "clay", "silt"),
  stat = "mean"
) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  bash_shebang <- "#!/bin/bash"

  wget <- paste0(
    "wget -nc -c --recursive --no-parent --no-host-directories --cut-dirs=1 ",
    "--reject=\"index.html*\" "
  )

  # valid url as of Sep 28, 2020
  # TODO: consider adding a check that this url is (i) still alive and
  #       (ii) that v1.0 is the latest version
  url_polaris <- "http://hydrology.cee.duke.edu/POLARIS/PROPERTIES"

  # create requests and add one for vrt
  requests <- c(
    "vrt",
    paste(rep(vars, each = length(stat)), rep(stat, length(vars)), sep = "/")
  )

  # create requests and write script out to disk
  file <- file.path(
    path,
    paste0("wget_POLARIS_", format(as.POSIXlt(Sys.Date()), "%Y%m%d"), ".sh")
  )

  writeLines(
    text = c(
      bash_shebang,
      paste(
        wget,
        url_polaris, version,
        requests,
        sep = "/"
      )
    ),
    con = file
  )

  invisible(file)
}


depth_profile_POLARIS <- function() {
  c("0_5", "5_15", "15_30", "30_60", "60_100", "100_200")
}


filepath_vrt_POLARIS <- function(path, var, stat, depth) {
  file.path(
    path,
    "vrt",
    paste0(var, "_", stat, "_", depth, ".vrt")
  )
}



#' Check that \var{POLARIS} soil data are locally available
#'
#' @param path A character string. The path to the local copy of the
#'   \var{POLARIS} folder hierarchy, e.g.,
#'   \code{dirname(prepare_script_for_POLARIS())}.
#' @param vars A vector of character strings. See Chaney et al. 2019
#' @param stats A vector of character strings. See Chaney et al. 2019
#'
#' @return A logical array with four dimensions: \describe{
#'   \item{File type}{
#'     \var{"vrt"} or \var{"tif"}; if \var{"vrt"} is missing, then \var{"tif"}
#'     are not checked.}
#'   \item{Variables}{Checks for each \code{vars}.}
#'   \item{Statistics}{Checks for each \code{stats}.}
#'   \item{Soil layer depths}{Checks for each soil layer in \var{POLARIS}.}
#' }
#'
#' @references
#'  Chaney, N. W., B. Minasny, J. D. Herman, T. W. Nauman, C. Brungard,
#'  C. L. S. Morgan, A. B. McBratney, E. F. Wood, and Y. T. Yimam. 2019.
#'  POLARIS soil properties: 30-meter probabilistic maps of soil properties
#'  over the contiguous United States. Water Resources Research 55:2916-2938.
#'  \url{https://doi.org/10.1029/2018WR022797}.
#'
#' @examples
#' script_to_download_polaris <- prepare_script_for_POLARIS()
#'
#' ## Execute script to download data
#' ## (or set `path_polaris` to your local copy)
#'
#' path_polaris <- dirname(script_to_download_polaris)
#' vars <- c("bd", "sand", "clay", "silt")
#' stat <- "mean"
#'
#' ## Check that we have POLARIS data
#' has_POLARIS <- check_POLARIS(path = path_polaris, vars = vars, stat = stat)
#'
#' # Do we have all files?
#' isTRUE(all(has_POLARIS))
#'
#' # If not, then examine
#' # (i) whether vrt files are missing, and/or
#' has_vrt <- !is.na(has_POLARIS["vrt", , , ]) & has_POLARIS["vrt", , , ]
#'
#' # (ii) whether tif files are missing
#' has_tif <- !is.na(has_POLARIS["tif", , , ]) & has_POLARIS["tif", , , ]
#'
#' @export
check_POLARIS <- function(
  path = ".",
  vars = c("bd", "sand", "clay", "silt"),
  stats = "mean"
) {
  depths <- depth_profile_POLARIS()

  ftypes <- c("vrt", "tif")

  res <- array(
    NA,
    dim = c(length(ftypes), length(vars), length(stats), length(depths)),
    dimnames = list(ftypes, vars, stats, depths)
  )

  for (k1 in seq_along(vars)) {
    for (k2 in seq_along(stats)) {
      for (k3 in seq_along(depths)) {

        ftmp <- filepath_vrt_POLARIS(path, vars[k1], stats[k2], depths[k3])

        res["vrt", k1, k2, k3] <- file.exists(ftmp)

        if (res["vrt", k1, k2, k3]) {
          # List all tif files referenced by the vrt
          x <- sf::gdal_utils("info", ftmp, quiet = TRUE)
          ftmps <- regmatches(
            x,
            m = gregexpr(paste0(path, ".+?\\.(tif|vrt)"), x)
          )

          res["tif", k1, k2, k3] <- all(file.exists(ftmps[[1]])[-1])
        }
       }
    }
  }

  res
}


#' Extract soil information from the \var{POLARIS} soil dataset
#'
#' @inheritParams check_POLARIS
#' @inheritParams rSW2st::as_points
#' @param stat A character string. See Chaney et al. 2019
#' @param buffer_m A numeric value. The radius of a buffer around each point
#'   from which to extract cell values and across which \code{fun} is applied.
#'   Passed to \code{\link[raster]{extract}}. Set to \code{NULL} to extract
#'   \var{POLARIS} gridcell values at point locations.
#' @param fun A function. Summarizing gridcell values if more than one value
#'   is extracted per location. See \code{\link[raster]{extract}}.
#' @param na.rm A logical value. Passed to \code{fun}.
#' @param verbose A logical value.
#'
#' @section Notes: This is a function with minimal functionality;
#' use \code{\link{extract_soils_POLARIS}} for a user-friendly interface.
#'
#' @references
#'  Chaney, N. W., B. Minasny, J. D. Herman, T. W. Nauman, C. Brungard,
#'  C. L. S. Morgan, A. B. McBratney, E. F. Wood, and Y. T. Yimam. 2019.
#'  POLARIS soil properties: 30-meter probabilistic maps of soil properties
#'  over the contiguous United States. Water Resources Research 55:2916-2938.
#'  \url{https://doi.org/10.1029/2018WR022797}.
#'
#' @export
fetch_soils_from_POLARIS <- function(x, crs,
  vars, stat, path, buffer_m = NULL, fun = NULL, na.rm = TRUE, verbose = FALSE
) {

  depths <- depth_profile_POLARIS()

  #--- Make sure inputs are correctly formatted
  locations <- rSW2st::as_points(x, to_class = "sf", crs = crs)


  #--- Prepare result object
  res <- array(
    NA,
    dim = c(length(locations), length(vars), length(depths)),
    dimnames = list(NULL, vars, depths)
  )


  #--- Extract values
  for (iv in seq_along(vars)) {
    for (id in seq_along(depths)) {

      ftmp <- filepath_vrt_POLARIS(path, vars[iv], stat, depths[id])

      if (file.exists(ftmp)) {
        if (verbose) {
          message(
            Sys.time(),
            " extracting ", vars[iv], " at ", sub("_", "-", depths[id]), " cm"
          )
        }

        res[, iv, id] <- raster::extract(
          x = raster::raster(ftmp),
          y = locations,
          method = "simple",
          buffer = buffer_m,
          fun = fun,
          na.rm = na.rm
        )

      } else {
        stop("POLARIS data ", shQuote(basename), " not found.")
      }
    }
  }

  res
}



#' Extract soil information from the \var{POLARIS} soil dataset
#' for \pkg{SOILWAT2} applications
#'
#' @inheritParams check_POLARIS
#' @inheritParams fetch_soils_from_POLARIS
#' @inheritParams rSW2st::as_points
#' @param method A character string. Method that determines extraction approach:
#'   (i) values are extracted using arguments
#'       \code{buffer_m}, \code{fun}, and \code{na.rm}
#'       and are returned \var{"asis"} or
#'   (ii) values are extracted for point locations,
#'       i.e., temporarily setting \code{buffer_m = NULL}; then,
#'       sites with problematic values (as determined by \code{fix_criteria})
#'       are extracted again under \var{"fix_with_buffer"} based on
#'       \code{buffer_m}, \code{fun}, and \code{na.rm}
#' @param fix_criteria A named list. Names correspond to \code{vars} or
#'   to \var{"texture"} if criterion is to be applied to the sum of
#'   sand, clay, and silt. Each element is applied to the variable of the
#'   element name to determine whether a site has problematic values.
#'   Elements are each a named list with two elements
#'   \var{"op"} for the relationship operator, e.g., \var{"<"}, and
#'   \var{"value"} for the value to compare against. See examples.
#' @param fun A function if \code{method} is either value or
#'   a named list of functions if \code{method = "fix_with_buffer"} where
#'   names correspond to \code{vars} or to \var{"texture"} if function is to
#'   be applied (individually) to sand, clay, and silt.
#'   Summarizing gridcell values if more than one value
#'   is extracted per location. See \code{\link[raster]{extract}}.
#' @param digits An integer value. The number of digits to which soil texture
#'   variables are rounded. Skip rounding if \code{NA} or \code{NULL}.
#'
#' @section Notes: A local copy of \var{POLARIS} is required. The function
#'   \code{\link{prepare_script_for_POLARIS}} creates a script that can be used
#'   to download \var{POLARIS} files.
#'
#' @section Notes: \var{POLARIS} uses weight-based percent as unit for
#'   \var{sand}, \var{clay}, \var{silt}; values occur in
#'   1% increments within \code{[0.5, 98.5]%}. However, the function returns
#'   soil texture in units of weight-based fractions.
#'
#' @references
#'  Chaney, N. W., B. Minasny, J. D. Herman, T. W. Nauman, C. Brungard,
#'  C. L. S. Morgan, A. B. McBratney, E. F. Wood, and Y. T. Yimam. 2019.
#'  POLARIS soil properties: 30-meter probabilistic maps of soil properties
#'  over the contiguous United States. Water Resources Research 55:2916-2938.
#'  \url{https://doi.org/10.1029/2018WR022797}.
#'
#' @seealso \code{\link[raster]{extract}}
#'
#' @examples
#' script_to_download_polaris <- prepare_script_for_POLARIS()
#'
#' ## Execute script to download data
#' ## (or set `path_polaris` to your local copy)
#'
#' path_polaris <- dirname(script_to_download_polaris)
#' vars <- c("bd", "sand", "clay", "silt")
#' stat <- "mean"
#'
#' ## Check that we have POLARIS data
#' has_POLARIS <- isTRUE(all(
#'   check_POLARIS(path = path_polaris, vars = vars, stat = stat)
#' ))
#'
#' if (has_POLARIS) {
#'
#'   locations <- matrix(
#'     data = c(-120.1286878, -111.8511136, 39.8182913, 36.9047396),
#'     nrow = 2
#'   )
#'
#'   ## Extract median of mean gridcell values across 100-m buffer
#'   ## around point locations
#'   res1 <- extract_soils_POLARIS(
#'     x = locations,
#'     vars = vars,
#'     stat = stat,
#'     path = path_polaris,
#'     buffer_m = 100,
#'     fun = median,
#'     na.rm = TRUE
#'   )
#'
#'   ## Extract mean gridcell values at point locations and use 70-m buffer at
#'   ## sites with bad values
#'   res2 <- extract_soils_POLARIS(
#'     x = locations,
#'     vars = vars,
#'     stat = stat,
#'     path = path_polaris,
#'     method = "fix_with_buffer",
#'     fix_criteria = list(
#'       bd = list(op = "<", value = 0.6),
#'       texture = list(op = "<", value = 50)
#'     ),
#'     buffer_m = 70,
#'     fun = list(
#'       bd = function(x, na.rm = TRUE) median(x[x > 0.6], na.rm = na.rm),
#'       texture = median
#'     ),
#'     na.rm = TRUE,
#'     digits = 3
#'   )
#' }
#'
#' # Clean up example
#' unlink(script_to_download_polaris)
#'
#'
#' @export
extract_soils_POLARIS <- function(
  x,
  crs = 4326,
  vars = c("bd", "sand", "clay", "silt"),
  stat = "mean",
  path,
  method = c("asis", "fix_with_buffer"),
  fix_criteria = list(
    bd = list(op = "<", value = 0.6),
    texture = list(op = "<", value = 0.5)
  ),
  buffer_m = NULL, fun = NULL, na.rm = TRUE,
  digits = 3L,
  verbose = FALSE
) {

  #--- Make sure inputs are correctly formatted
  var_stxt3 <- c("sand", "clay", "silt")
  var_stxt <- intersect(var_stxt3, vars)
  var_others <- setdiff(vars, var_stxt)

  method <- match.arg(method)

  locations <- rSW2st::as_points(x, to_pkg = "sf", crs = crs)


  # Extract values from POLARIS
  res <- fetch_soils_from_POLARIS(
    x = locations,
    crs = crs,
    vars = vars,
    stat = stat,
    path = path,
    buffer_m = if (method == "fix_with_buffer") NULL else buffer_m,
    fun = if (method == "fix_with_buffer") NULL else fun,
    na.rm = na.rm,
    verbose = verbose
  )

  N_layers <- dim(res)[3]


  #--- Attempt to replace sites with problematic values by buffered extractions
  if (method == "fix_with_buffer") {

    # Determine for which variables we have criteria to determine problems
    tmp <- intersect(c(vars, "texture"), names(fix_criteria))
    ok <- sapply(
      X = fix_criteria[tmp],
      FUN = function(x) all(c("op", "value") %in% names(x))
    )
    check_vars <- tmp[ok]

    # Is `fix_criteria` well formed?
    if (any(!ok)) {
      warning(
        "Cannot apply `fix_with_buffer` for ",
        paste(shQuote(tmp[!ok]), collapse = ", "),
        " because of incomplete criteria."
      )
    }


    # Determine whether we have one `fun` to be applied to all fixes or
    # separate `fun`s
    one_fun <- !is.list(fun) && is.function(try(match.fun(fun), silent = TRUE))
    ok <- if (one_fun) TRUE else check_vars %in% names(fun)

    # Is `fun` well formed?
    if (any(!ok)) {
      warning(
        "Cannot apply `fix_with_buffer` for ",
        paste(shQuote(tmp[!ok]), collapse = ", "),
        " because of missing summarizing function `fun`."
      )
    }


    # Fix for texture variables
    if ("texture" %in% check_vars) {
      hasnot_texture <- !(var_stxt3 %in% vars)

      if (any(hasnot_texture)) {
        warning(
          "Cannot apply `fix_with_buffer` for `texture` because of ",
          "missing texture variables: ",
          paste(shQuote(var_stxt3[hasnot_texture]), collapse = ", ")
        )

      } else {
        tmp <- fix_criteria[["texture"]]

        is_bad_texture <- apply(
          X = apply(res[, var_stxt3, ], c(1, 3), sum),
          MARGIN = 1,
          FUN = function(x) {
            any(do.call(tmp[["op"]], args = list(x, tmp[["value"]])))
          }
        )

        if (any(is_bad_texture)) {
          res[is_bad_texture, var_stxt3, ] <- fetch_soils_from_POLARIS(
            path = path,
            x = locations[is_bad_texture, ],
            crs = crs,
            vars = var_stxt3,
            stat = stat,
            buffer_m = buffer_m,
            fun = if (one_fun) fun else fun[["texture"]],
            na.rm = na.rm,
            verbose = verbose
          )
        }
      }

      check_vars <- grep("texture", check_vars, value = TRUE, invert = TRUE)
    }

    # Fix for all other variables
    for (k in seq_along(check_vars)) {
      tmp <- fix_criteria[[check_vars[k]]]

      is_bad <- apply(
        X = res[, check_vars[k], , drop = FALSE],
        MARGIN = 1,
        FUN = function(x) {
          any(do.call(tmp[["op"]], args = list(x, tmp[["value"]])))
        }
      )

      if (any(is_bad)) {
        res[is_bad, check_vars[k], ] <- fetch_soils_from_POLARIS(
          path = path,
          x = locations[is_bad, ],
          crs = crs,
          vars = check_vars[k],
          stat = stat,
          buffer_m = buffer_m,
          fun = if (one_fun) fun else fun[[check_vars[k]]],
          na.rm = na.rm,
          verbose = verbose
        )
      }
    }
  }


  #--- Convert units & rounding
  # Convert % to fraction
  res[, var_stxt, ] <- res[, var_stxt, ] / 100

  # Round
  if (is.finite(digits)) {
    res[, var_others, ] <- round(res[, var_others, ], digits)

    for (k in seq_len(N_layers)) {
      has_vals <-
        complete.cases(res[, var_stxt, k]) &
        apply(res[, var_stxt, k, drop = FALSE], 1, sum, na.rm = TRUE) > 0

      res[has_vals, var_stxt, k] <- rSW2utils::scale_rounded_by_sum(
        x = res[has_vals, var_stxt, k],
        digits = digits,
        icolumn_adjust = 3
      )
    }
  }


  #--- Create texture table
  # Convert to wide format (one row for each point location)
  locs_table_texture <- reshape2::acast(reshape2::melt(res), Var1 ~ Var3 + Var2)
  colnames(locs_table_texture) <- paste0(
    rep(vars, times = N_layers),
    "_L",
    rep(seq_len(N_layers), each = length(vars))
  )


  #--- Set (fixed) soil depth of profile in wide-format for output
  layer_depths <- as.integer(sapply(
    X = strsplit(depth_profile_POLARIS(), split = "_"),
    FUN = function(x) x[2]
  ))

  locs_table_depths <- cbind(
    N_horizons = N_layers,
    SoilDepth_cm = max(layer_depths),
    matrix(
      data = layer_depths,
      nrow = length(locations),
      ncol = N_layers,
      byrow = TRUE,
      dimnames = list(NULL, paste0("depth_L", seq_len(N_layers)))
    )
  )


  #--- Return tables
  list(
    ref = create_reference_for_POLARIS(),
    table_depths = locs_table_depths,
    table_texture = locs_table_texture
  )
}
