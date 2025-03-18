#' Determine soil layer widths from depth profile
#'
#' @param layers_depth A numeric vector. The lower depths of soil layers,
#'   sorted by increasing depth in a consistent unit.
#'
#' @examples
#' depths_bottom <- c(10, 30, 50, 100)
#' widths <- getLayersWidth(depths_bottom)
#'
#' data.frame(
#'   layer_depth_top = depths_bottom - widths,
#'   layer_width = widths,
#'   layer_depth_bottom = depths_bottom
#' )
#'
#' @export
getLayersWidth <- function(layers_depth) {
  diff(c(0, layers_depth))
}


#' Adjust a depth interval by the depth of a possible restriction
#'
#' @param depths A numeric vector of length two. The upper and lower depth
#'   bound that should be adjusted.
#' @param imp_depth A numeric value. The depth of a restriction.
#' @param sdepths A numeric vector. The lower depths of soil layers,
#'   sorted by increasing depth in a consistent unit.
#'
#' @return The adjusted values of \code{depths}.
#'
#' @examples
#' depths_bottom <- c(10, 30, 50, 100)
#'
#' adjustLayer_byImp(c(15, 70), 60, depths_bottom)
#' adjustLayer_byImp(c(15, 70), 5, depths_bottom)
#' adjustLayer_byImp(c(15, 70), 150, depths_bottom)
#'
#' @export
adjustLayer_byImp <- function(depths, imp_depth, sdepths) {
  if (any(imp_depth < depths[[1]])) {
    depths <- imp_depth
    if (length(sdepths) >= 2) {
      tmp <- findInterval(imp_depth, sdepths)
      depths <- if (tmp > 1) {
        c(sdepths[tmp - 1], imp_depth)
      } else {
        c(imp_depth, sdepths[tmp + 1])
      }
    }
  } else if (any(imp_depth < depths[[2]])) {
    depths <- c(depths[[1]], imp_depth)
  }

  depths
}



#' Add or dissolve a soil layer/horizon
#'
#' @param x A two-dimensional numeric \code{matrix}
#'   or \code{data.frame}.
#'   Rows represent sites (or variables);
#'   columns represent soil layers/horizons.
#' @param target_cm A numeric value. The lower depth in centimeter of the
#'   soil horizon/layer that is to be added or dissolved.
#' @param depths_cm A numeric vector. The lower depths in centimeters of the
#'   existing soil horizons/layers.
#' @param method A character string. The method by which variable values are
#'   assigned to a new or dissolved soil horizon/layer:
#'   \itemize{
#'     \item \var{"interpolate"} calculates the mean across affected soil
#'       horizons/layers weighted by their widths.
#'     \item \var{"exhaust"} calculates updated values so that their sum
#'       across affected soil horizons/layers remains constant.
#'   }
#'
#' @return An updated copy of \code{x}.
#'
#' @section Details:
#'   Soil layers/horizons are added independently for each row; i.e.,
#'   rows can represent either sites or variables as long as all rows can
#'   be subjected to the same \code{method} option.
#'
#' @seealso \code{\link{update_soil_profile}}
#'
#' @examples
#' soil_layers <- c(8, 20, 150)
#' N <- length(soil_layers)
#'
#' ### Examples where rows represent variables
#' xsoils <- data.frame(
#'   sand = c(0.3, 0.35, 0.4),
#'   clay = c(0.1, 0.15, 0.1)
#' )
#'
#' t(add_soil_layer(
#'   t(xsoils),
#'   target_cm = 15,
#'   depths_cm = soil_layers,
#'   method = "interpolate"
#' ))
#'
#' t(dissolve_soil_layer(
#'   t(xsoils),
#'   target_cm = 20,
#'   depths_cm = soil_layers,
#'   method = "interpolate"
#' ))
#'
#'
#' ### Examples where rows represent sites
#' soil_data <- cbind(
#'   sand = t(data.frame(
#'     site1 = c(0.5, 0.55, NA),
#'     site2 = c(0.3, 0.35, 0.4)
#'   )),
#'   TranspCoeff = t(data.frame(
#'     site1 = c(0.9, 0.1, NA),
#'     site2 = c(0.7, 0.1, 0.2)
#'   ))
#' )
#' colnames(soil_data) <- paste0(
#'   rep(c("sand", "TranspCoeff"), each = N),
#'   "_L",
#'   seq_len(N)
#' )
#'
#' ## Add new 0-5 cm layer, i.e., split the existing 0-8 cm layer into
#' ## two layers of 0-5 cm and 5-8 cm
#' add_soil_layer(
#'   soil_data[, paste0("sand_L", seq_len(N))],
#'   target_cm = 5,
#'   depths_cm = soil_layers,
#'   method = "interpolate"
#' )
#'
#' ## Add new 20-80 cm layer, i.e., split the existing 20-150 cm layer into
#' ## two layers of 20-80 cm and 80-150 cm
#' add_soil_layer(
#'   soil_data[, paste0("TranspCoeff_L", seq_len(N))],
#'   target_cm = 80,
#'   depths_cm = soil_layers,
#'   method = "exhaust"
#' )
#'
#' ## Add new 150-200 cm layer, i.e., extend the soil profile
#' add_soil_layer(
#'   soil_data[, paste0("sand_L", seq_len(N))],
#'   target_cm = 200,
#'   depths_cm = soil_layers,
#'   method = "interpolate"
#' )
#'
#'
#' ## Remove the shallowest 0-8 cm layer, i.e., combine layers 0-8 cm and
#' ## 8-20 cm into a new 0-20 cm layer
#' dissolve_soil_layer(
#'   soil_data[, paste0("sand_L", seq_len(N))],
#'   target_cm = 8,
#'   depths_cm = soil_layers,
#'   method = "interpolate"
#' )
#'
#' ## Remove the 8-20 cm layer, i.e., combine layers 8-20 cm and
#' ## 20-150 cm into a new 8-150 cm layer
#' dissolve_soil_layer(
#'   soil_data[, paste0("TranspCoeff_L", seq_len(N))],
#'   target_cm = 20,
#'   depths_cm = soil_layers,
#'   method = "exhaust"
#' )
#'
#' ## Attempt to remove the deepest 20-150 cm layer, but not possible to combine
#' dissolve_soil_layer(
#'   soil_data[, paste0("sand_L", seq_len(N))],
#'   target_cm = 150,
#'   depths_cm = soil_layers,
#'   method = "interpolate"
#' )
#'
#' @name update_soil_data
NULL


#' @rdname update_soil_data
#'
#' @section Details:
#'   \code{add_soil_layer} inserts a new soil layer by splitting
#'   an existing one into two layers.
#' @section Details:
#'   With \code{method == "interpolate"},
#'   adding a new shallowest layer (\code{target_cm < depths_cm}) or
#'   a new deepest layer (\code{target_cm > depths_cm})
#'   will copy values from the previously shallowest or, respectively, deepest
#'   layer.
#'
#' @export
add_soil_layer <- function(
  x,
  target_cm,
  depths_cm,
  method = c("interpolate", "exhaust")
) {

  #--- Check inputs
  method <- match.arg(method)

  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  ncols <- dim(x)[[2]]


  #--- Determine weights based on depth profile
  il <- findInterval(target_cm, depths_cm)

  w <- if (il == 0) {
    c(target_cm, depths_cm[[1]] - target_cm)

  } else if (il >= length(depths_cm)) {
    tmp <- depths_cm[length(depths_cm)]

    if (length(depths_cm) == 1) {
      c(tmp, target_cm - tmp)
    } else {
      c(tmp - depths_cm[length(depths_cm) - 1], target_cm - tmp)
    }

  } else {
    abs(target_cm - depths_cm[il + c(1, 0)])
  }


  stopifnot(length(w) == 2L, ncols > 0, is.finite(il), il >= 0, il <= ncols)
  w_sum <- sum(w)

  #--- Add new layer
  if (ncols > il) {
    # Add layer at an intermediate depth of existing layers
    x <- x[, c(seq_len(il), NA, (il + 1):ncols), drop = FALSE]

    if (method == "interpolate") {
      if (il > 0) {
        x[, il + 1] <- (x[, il] * w[[1]] + x[, il + 2] * w[[2]]) / w_sum

      } else {
        # Add layer at a more shallow depth than any existing layer
        x[, 1] <- x[, 2]
      }

    } else if (method == "exhaust") {
      x[, il + 1] <- x[, il + 2] * w[[1]] / w_sum
      x[, il + 2] <- x[, il + 2] * w[[2]] / w_sum
    }

  } else if (ncols == il) {
    # Add a deeper layer than any existing layer
    x <- x[, c(seq_len(ncols), NA), drop = FALSE]

    if (method == "interpolate") {
      x[, il + 1] <- x[, il]

    } else if (method == "exhaust") {
      # First calculate x[, il + 1] so that x[, il] maintains previous value
      x[, il + 1] <- x[, il] * w[[2]] / w_sum
      x[, il] <- x[, il] * w[[1]] / w_sum
    }
  }

  x
}


#' @rdname update_soil_data
#'
#' @section Details:
#'   \code{dissolve_soil_layer} combines/dissolves a soil layer with a lower
#'   depth of \code{target_cm} with the next deeper layer.
#'   The deepest (or only) soil layer cannot be removed in which case
#'   the code issues a warning and returns an unmodified copy of \code{x}.
#'
#' @export
dissolve_soil_layer <- function(
  x,
  target_cm,
  depths_cm,
  method = c("interpolate", "exhaust")
) {
  #--- Check inputs
  method <- match.arg(method)

  if (!is.matrix(x)) x <- data.matrix(x)

  ncols <- dim(x)[[2]]

  il <- which(target_cm == depths_cm)

  if (length(il) != 1) {
    stop("Unique soil layer to dissolve could not be located.", call. = FALSE)
  }

  if (ncols > 1 && il < ncols) {

    #--- Remove column
    xnew <- x[, -il, drop = FALSE]

    if (method == "interpolate") {
      # mean of previous layers weighted by layer-widths
      w <- if (il == 1) {
        c(target_cm, depths_cm[il + 1] - target_cm)
      } else {
        c(target_cm - depths_cm[il - 1], depths_cm[il + 1] - target_cm)
      }

      xnew[, il] <- (x[, il] * w[[1]] + x[, il + 1] * w[[2]]) / sum(w)

    } else if (method == "exhaust") {
      # sum of previous layers
      xnew[, il] <- rowSums(x[, il:(il + 1L), drop = FALSE])
    }

    x <- xnew

  } else {
    #--- Cannot remove the deepest (or only) soil layer
    # nolint start: nonportable_path_linter.
    msg <- paste(
      "Cannot remove/combine the",
      if (ncols == 1) "only" else "deepest",
      "soil layer."
    )
    # nolint end
    warning(msg, call. = FALSE)
  }

  x
}



#' Adjust soil horizon/layer depth profile and update soil data
#'
#' @param soil_layers A two-dimensional numeric \code{matrix}
#'   or \code{data.frame}.
#'   Rows represents sites; columns represent soil layers/horizons;
#'   and values are depths of the lower limits of soil layers/horizons in
#'   centimeters.
#' @param requested_soil_layers A numeric vector. The soil depths that should
#'   be added if not already present.
#' @param soil_data A two-dimensional numeric matrix or \code{data.frame}.
#'   Rows represents sites and columns represent
#'   variables x soil layers/horizons combinations.
#'   Column names are formatted as \var{"var_Lx"} where \var{x} represents
#'   an increasing soil layer/horizon number.
#' @param variables A vector of character strings. The patterns identifying
#'   all variables that are to be updated.
#' @param vars_exhaust A vector of character strings. The patterns
#'   identifying variables whose values are "exhausted" when a soil layer is
#'   split into two layers. See \code{\link{update_soil_data}} for details.
#' @param keep_prev_soildepth A logical value.
#'   If \code{TRUE}, add \code{requested_soil_layers} only up to
#'   previous maximum soil depth.
#'   If \code{FALSE}, add all \code{requested_soil_layers}.
#' @param keep_prev_soillayers A logical value.
#'   If \code{TRUE}, add \code{requested_soil_layers} and keep previous layers.
#'   If \code{FALSE}, add \code{requested_soil_layers} and dissolve previous
#'   layers.
#' @param verbose A logical value.
#'
#' @return A named list with elements: \describe{
#'   \item{updated}{
#'     A logical value indicating whether any layers/horizons
#'     have been modified (added or dissolved).
#'   }
#'   \item{soil_layers}{An updated copy of \code{soil_layers}.}
#'   \item{soil_data}{An updated copy of \code{soil_data}.}
#' }
#'
#' @seealso
#'   \code{\link{dissolve_soil_layer}} and \code{\link{add_soil_layer}}
#'
#' @examples
#' soil_layers <- t(data.frame(
#'   site1 = c(20, 81, NA),
#'   site2 = c(8, 20, 150)
#' ))
#'
#' N <- ncol(soil_layers)
#' colnames(soil_layers) <- paste0("depth_L", seq_len(N))
#'
#' soil_data <- cbind(
#'   sand = t(data.frame(
#'     site1 = c(0.5, 0.55, NA),
#'     site2 = c(0.3, 0.35, 0.4)
#'   )),
#'   TranspCoeff = t(data.frame(
#'     site1 = c(0.9, 0.1, NA),
#'     site2 = c(0.7, 0.1, 0.2)
#'   ))
#' )
#' colnames(soil_data) <- paste0(
#'   rep(c("sand", "TranspCoeff"), each = N),
#'   "_L",
#'   seq_len(N)
#' )
#'
#' new_soils1 <- update_soil_profile(
#'   soil_layers = soil_layers,
#'   requested_soil_layers = c(5, 10, 20, 100, 200),
#'   soil_data = soil_data,
#'   keep_prev_soildepth = FALSE,
#'   keep_prev_soillayers = TRUE
#' )
#'
#' new_soils2 <- update_soil_profile(
#'   soil_layers = soil_layers,
#'   requested_soil_layers = c(5, 10, 20, 100, 200),
#'   soil_data = soil_data,
#'   keep_prev_soildepth = TRUE,
#'   keep_prev_soillayers = FALSE
#' )
#'
#' @export
update_soil_profile <- function(
  soil_layers,
  requested_soil_layers,
  soil_data,
  variables = NULL,
  vars_exhaust = c("EvapCoeff", "TranspCoeff", "Imperm"),
  keep_prev_soildepth = TRUE,
  keep_prev_soillayers = TRUE,
  verbose = FALSE
) {
  #--- Check inputs
  # Available soil layers: rows = sites, columns = layer depths
  soil_layers <- round(data.matrix(soil_layers))

  # Requested soil layers
  requested_soil_layers <- as.integer(round(requested_soil_layers))
  stopifnot(requested_soil_layers > 0, diff(requested_soil_layers) > 0)

  # Available soil data: rows = sites, columns = variables
  soil_data <- data.matrix(soil_data)
  cns_data <- colnames(soil_data)

  # Variables for which to update soil profile
  if (length(variables) == 0) {
    # Assumption: colnames of `soil_data` are formatted as `var_Lx`
    variables <- unique(
      vapply(
        X = strsplit(cns_data, split = "_", fixed = TRUE),
        FUN = function(x) paste(x[-length(x)], collapse = "_"),
        FUN.VALUE = NA_character_
      )
    )

  } else {
    stopifnot(
      vapply(variables, function(x) any(grepl(x, cns_data)), FUN.VALUE = NA)
    )
  }

  # Determine how to add different soil variables
  # - variables whose values will be exhausted (all others are interpolated)
  tmp <- vapply(
    vars_exhaust,
    function(x) any(grepl(x, cns_data)),
    FUN.VALUE = NA
  )
  vars_exhaust <- vars_exhaust[tmp]


  # Prepare output container
  has_updates <- FALSE
  new_soil_data <- array(
    NA,
    dim = dim(soil_data),
    dimnames = dimnames(soil_data)
  )
  new_soil_layers <- array(
    NA,
    dim = dim(soil_layers),
    dimnames = dimnames(soil_layers)
  )


  #--- Identify layers and groups of sites with the same profile (soil layers)
  ids_layers <- seq_len(dim(soil_layers)[[2]])
  avail_sl_ids <- apply(soil_layers, 1, paste0, collapse = "x")
  layer_sets <- unique(avail_sl_ids)
  N_sets <- length(layer_sets)

  has_updates <- FALSE

  if (N_sets > 0) {
    if (verbose) {
      message("Processing ", N_sets, " soil layer sets:")
      has_progress_bar <- requireNamespace("utils")
    } else {
      has_progress_bar <- FALSE
    }

    if (has_progress_bar) {
      pb <- utils::txtProgressBar(max = N_sets, style = 3)
    }

    # Prepare data by variable
    soil_data_by_var <- lapply(
      X = variables,
      FUN = function(x) {
        soil_data[, grep(x, cns_data)[ids_layers], drop = FALSE]
      }
    )

    new_soil_data_by_var <- NULL


    #--- Loop through sites with same layer profile and make adjustments
    for (k1 in seq_along(layer_sets)) {
      has_updatek1 <- FALSE

      if (has_progress_bar) {
        utils::setTxtProgressBar(pb, k1)
      }

      il_set <- avail_sl_ids == layer_sets[k1]
      if (sum(il_set, na.rm = TRUE) == 0) next

      new_soil_data_by_var <- lapply(
        soil_data_by_var,
        function(x) x[il_set, , drop = FALSE]
      )


      # --- Add requested soil layers
      # Identify which requested layers need to be added
      ldset_prev <- stats::na.exclude(soil_layers[which(il_set)[[1]], ])
      ldset <- ldset_prev
      req_sd_toadd <- setdiff(requested_soil_layers, ldset)

      if (isTRUE(keep_prev_soildepth)) {
        req_sd_toadd <- req_sd_toadd[req_sd_toadd < max(ldset)]
      }

      if (length(req_sd_toadd) > 0) {
        # Add identified layers
        for (lnew in req_sd_toadd) {
          for (k2 in seq_along(variables)) {
            new_soil_data_by_var[[k2]] <- add_soil_layer(
              new_soil_data_by_var[[k2]],
              target_cm = lnew,
              depths_cm = ldset,
              method = if (variables[k2] %in% vars_exhaust) {
                "exhaust"
              } else {
                "interpolate"
              }
            )
          }

          ldset <- sort(c(ldset, lnew))
        }

        has_updatek1 <- TRUE
      }


      #--- Remove soil layers that are no longer requested
      if (isFALSE(keep_prev_soillayers)) {
        # Identify which layers to dissolve, but cannot dissolve deepest layer
        tmp_keep <- if (isTRUE(keep_prev_soildepth)) {
          c(
            requested_soil_layers[requested_soil_layers < max(ldset_prev)],
            ldset_prev[length(ldset_prev)]
          )

        } else {
          c(requested_soil_layers, max(ldset))
        }

        req_sd_todis <- setdiff(ldset, tmp_keep)

        if (length(req_sd_todis) > 0) {
          # Dissolve previous layers
          for (ldis in req_sd_todis) {
            for (k2 in seq_along(variables)) {
              new_soil_data_by_var[[k2]] <- dissolve_soil_layer(
                new_soil_data_by_var[[k2]],
                target_cm = ldis,
                depths_cm = ldset,
                method = if (variables[k2] %in% vars_exhaust) {
                  "exhaust"
                } else {
                  "interpolate"
                }
              )
            }

            ldset <- ldset[ldset != ldis]
          }

          has_updatek1 <- TRUE
        }
      }


      #--- Update soil data
      if (has_updatek1) {
        # Determine how many layers are already available as columns
        n_has <- Inf

        for (k2 in seq_along(variables)) {
          tmp <- grep(variables[k2], colnames(new_soil_data))
          n_has <- min(n_has, length(tmp))
        }

        # Determine how many layers need to be accommodated
        nadd <- length(ldset) - n_has

        if (nadd > 0) {
          # Add columns for additional layers
          ids <- ncol(new_soil_layers) + seq_len(nadd)

          new_soil_data <- cbind(
            new_soil_data,
            matrix(
              data = NA,
              nrow = nrow(new_soil_data),
              ncol = length(variables) * nadd,
              dimnames = list(
                NULL,
                paste0(
                  rep(variables, nadd),
                  "_L",
                  rep(ids, each = length(variables))
                )
              )
            )
          )
        }

        lyrs <- seq_along(ldset)

        for (k2 in seq_along(variables)) {
          tmp <- grep(variables[k2], colnames(new_soil_data))

          new_soil_data[il_set, tmp[lyrs]] <- new_soil_data_by_var[[k2]][, lyrs]
        }


        #--- Update soil layer depths
        nadd <- length(ldset) - ncol(new_soil_layers)

        if (nadd > 0) {
          # Add columns for additional layers
          ids <- ncol(new_soil_layers) + seq_len(nadd)

          new_soil_layers <- cbind(
            new_soil_layers,
            matrix(
              data = NA,
              nrow = nrow(new_soil_layers),
              ncol = nadd,
              dimnames = list(
                NULL,
                paste0("depth_L", ids)
              )
            )
          )
        }

        new_soil_layers[il_set, lyrs] <- matrix(
          data = ldset,
          nrow = sum(il_set),
          ncol = length(ldset),
          byrow = TRUE
        )

      } else {
        # Nothing changed: copy previous values
        ids <- seq_len(min(ncol(new_soil_data), ncol(soil_data)))
        new_soil_data[il_set, ids] <- soil_data[il_set, ids]

        ids <- seq_len(min(ncol(new_soil_layers), ncol(soil_layers)))
        new_soil_layers[il_set, ids] <- soil_layers[il_set, ids]
      }

      has_updates <- has_updates || has_updatek1
    }

    if (has_progress_bar) {
      close(pb)
    }
  }


  list(
    updated = has_updates,
    soil_data = new_soil_data,
    soil_layers = new_soil_layers
  )
}



#' Find the soil layer numbers in a soil layer profile that are affected by
#' a soil depth or a soil band.
#'
#' @param depths A numeric vector of length one or two.
#' @param sdepth A numeric vector. The lower depths of soil layers,
#'   sorted by increasing depth in a consistent unit.
#'
#' @examples
#' depths_bottom <- c(10, 30, 50, 100)
#' identify_soillayers(5, depths_bottom)
#' identify_soillayers(c(15, 40), depths_bottom)
#' identify_soillayers(c(15, 150), depths_bottom)
#'
#' @export
identify_soillayers <- function(depths, sdepth) {
  it <- findInterval(depths, sdepth)
  if (anyNA(it)) {
    as.integer(na.exclude(it))
  } else if (length(it) > 1 && diff(it) > 0) {
    (1 + it[[1]]):(it[[2]])
  } else {
    it[[1]]
  }
}

#' Reshape soil properties between wide format and (semi-)long format
#'
#' @param x_wide A data frame in wide format (see details).
#' @param x_long A data frame in long or semi-long format (see details).
#' @param type_to A character string. Determines if output format
#'  is long or semi-long (see details).
#' @param type_from A character string. Describes if format of `x_long` is
#'   long or semi-long.
#' @param id_site A character string. Column name of `x` that
#'   contains unique site names.
#' @param id_soillayer A character string. Name of the column that will
#'   be created to contain soil layer numbers.
#' @param sep_soillayer A character string. See details.
#' @param soilproperties A character vector. Names of soil properties;
#'   see details.
#' @param id_values A character string. Name of the column in long format
#'   that contains values of soil properties.
#' @param id_property A character string. Name of the column in long format
#'   that contains names of soil properties.
#'
#' @return
#'  * [reshape_soilproperties_to_long()] returns
#'    * a data frame in long format if `type_to` is `"long"`
#'    * a data frame in semi-long format if `type_to` is `"long_by_properties"`.
#'  * [reshape_soilproperties_to_long()] returns a data frame in wide format.
#'
#' @seealso [stats::reshape()]
#'
#' @section Details:
#' Formats of soil-property data frames
#'   * `"wide"`
#'     * each row represents a unique site
#'       (encoded by column `id_site`)
#'     * besides `id_site`, columns represent soil properties
#'       at different soil layers and column names follow the pattern
#'       `<soilproperties><sep_soillayer><soil layer id>`, e.g.,
#'       `db_L1`, `fsand_L1`, `fclay_L1`, `fcoarse_L1`, `db_L2`, `fsand_L2`, ...
#'   * `"semi-long"` (or `"long_by_properties"`)
#'     * each row represents a unique combination of sites and soil layers
#'       (encoded by columns `id_site` and `id_soillayer`)
#'     * columns are `id_site`, `id_soillayer`, `soilproperties`, e.g.,
#'       `site`, `soillayer`, `db`, `fsand`, `fclay`, `fcoarse`
#'   * `"long"`
#'     * each row represents a unique observation, e.g.,
#'       `SiteA`, 1, `fsand`, 0.70
#'     * columns are `id_site`, `id_soillayer`, `id_property`, `id_values`
#'
#'
#' @examples
#' x_wide <- data.frame(
#'   location = c("SiteA", "SiteB"),
#'   db_L1 = c(1.5, 1.6),
#'   fsand_L1 = c(0.7, 0.2),
#'   fclay_L1 = c(0.1, 0.2),
#'   db_L2 = c(1.6, 1.7),
#'   fsand_L2 = c(0.75, 0.3),
#'   fclay_L2 = c(0.1, 0.15)
#' )
#'
#' x_long <- reshape_soilproperties_to_long(
#'   x_wide,
#'   type_to = "long",
#'   id_site = "location",
#'   soilproperties = c("db", "fsand", "fclay")
#' )
#' x_wide_from_long <- reshape_soilproperties_to_wide(
#'   x_long,
#'   type_from = "long",
#'   id_site = "location",
#'   soilproperties = c("db", "fsand", "fclay")
#' )
#' all.equal(x_wide_from_long, x_wide, check.attributes = FALSE)
#'
#' x_semilong <- reshape_soilproperties_to_long(
#'   x_wide,
#'   type_to = "long_by_properties",
#'   id_site = "location",
#'   soilproperties = c("db", "fsand", "fclay")
#' )
#' x_wide_from_semilong <- reshape_soilproperties_to_wide(
#'   x_semilong,
#'   type_from = "long_by_properties",
#'   id_site = "location",
#'   soilproperties = c("db", "fsand", "fclay")
#' )
#' all.equal(x_wide_from_semilong, x_wide, check.attributes = FALSE)
#'
#' @md
#' @name reshape_soilproperties
NULL

#' @rdname reshape_soilproperties
#'
#' @export
reshape_soilproperties_to_long <- function(
  x_wide,
  type_to = c("long", "long_by_properties"),
  id_site = "site",
  id_soillayer = "soillayer",
  sep_soillayer = "_L",
  soilproperties = c("db", "fsand", "fclay", "fcoarse"),
  id_values = "value",
  id_property = "variable"
) {
  type_to <- match.arg(type_to)

  cns <- colnames(x_wide)
  ids <- lapply(
    X = soilproperties,
    FUN = grep,
    x = cns,
    value = TRUE
  )
  v_names <- if (is.null(names(ids))) {
    soilproperties
  } else {
    names(ids)
  }

  var_drop <- cns[!(cns %in% c(id_site, unlist(ids)))]

  tmp <- stats::reshape(
    data = x_wide,
    direction = "long",
    idvar = id_site,
    drop = if (length(var_drop) > 0) var_drop,
    varying = ids,
    v.names = v_names,
    sep = sep_soillayer,
    timevar = id_soillayer
  )

  res <- switch(
    EXPR = type_to,
    long_by_properties = tmp,
    long = stats::reshape(
      data = tmp,
      direction = "long",
      idvar = c(id_site, id_soillayer),
      varying = list(v_names),
      v.names = id_values,
      sep = "",
      timevar = id_property,
      times = v_names
    )
  )

  rownames(res) <- NULL
  res
}


#' @rdname reshape_soilproperties
#'
#' @export
reshape_soilproperties_to_wide <- function(
  x_long,
  type_from = c("long", "long_by_properties"),
  id_site = "site",
  id_soillayer = "soillayer",
  sep_soillayer = "_L",
  soilproperties = c("db", "fsand", "fclay", "fcoarse"),
  id_values = "value",
  id_property = "variable"
) {
  type_from <- match.arg(type_from)

  res <- if (type_from == "long") {
    stats::reshape(
      data = x_long[, c(id_site, id_soillayer, id_values, id_property)],
      direction = "wide",
      idvar = c(id_site, id_soillayer),
      timevar = id_property,
      v.names = id_values,
      varying = list(soilproperties)
    )
  } else if (type_from == "long_by_properties") {
    x_long
  }

  res <- stats::reshape(
    data = res[, c(id_site, id_soillayer, soilproperties)],
    direction = "wide",
    idvar = id_site,
    timevar = id_soillayer,
    sep = sep_soillayer
  )
  rownames(res) <- NULL

  res
}
