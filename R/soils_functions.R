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
  if (any(imp_depth < depths[1])) {
    depths <- imp_depth
    if (length(sdepths) >= 2) {
      temp <- findInterval(imp_depth, sdepths)
      if (temp > 1) {
        depths <- c(sdepths[temp - 1], imp_depth)
      } else {
        depths <- c(imp_depth, sdepths[temp + 1])
      }
    }
  } else if (any(imp_depth < depths[2])) {
    depths <- c(depths[1], imp_depth)
  }

  depths
}



#' Calculate the relative contribution of existing soil layers at a new depth
#'
#' @param il_new A numeric value.
#' @param target_cm A numeric value.
#' @param depths_cm A numeric vector.
#'
#' @return A numeric vector of length two.
#'
#' @examples
#' target <- 45
#' depths <- c(10, 30, 50, 100)
#' calc_weights_from_depths(
#'   il_new = findInterval(target, depths),
#'   target_cm = target,
#'   depths_cm = depths
#' )
#'
#' @export
calc_weights_from_depths <- function(il_new, target_cm, depths_cm) {
  if (il_new == 0) {
    c(target_cm, depths_cm[1] - target_cm)

  } else if (il_new >= length(depths_cm)) {
    c(0, target_cm)

  } else {
    abs(target_cm - depths_cm[il_new + c(1, 0)])

  }
}





#' Split soil layer in two layers
#'
#' @param x A numeric data.frame or matrix. Columns are soil layers.
#' @param il An integer value. The column/soil layer number after which a new
#'   layer is added.
#' @param w A numeric vector of length one or two. The weights used to calculate
#'   the values of the new layer.
#' @param method A character string. See \code{Details}.
#'
#' @section Details: If the weight vector is of length one and \code{x} contains
#'   a row with name 'depth_cm', then it is assumed that the value of \code{w}
#'   corresponds to the weight of the first layer and the weight of the second
#'   layer is calculated as \code{(depth of first layer of x) - (first value of
#'   w)}. If this is case and if the added layer is either more shallow or
#'   deeper than any input layers, then the depth of the added layer is
#'   calculated proportionally if \code{sum(w) <= 1} otherwise additively.
#' @section Details: The method \code{interpolate} calculates the weighted mean
#'   of the columns/layers \code{il} and \code{il + 1}. If \code{il == 0}, i.e.,
#'   add layer at a more shallow depth than any existing layer, then values from
#'   the previously first layer are copied to the newly created layer. The
#'   method \code{exhaust} distributes the value of \code{il + 1} according to
#'   the weights.
#'
#' @return An object like x with one column more at position \code{il + 1}.
#' @export
add_layer_to_soil <- function(x, il, w, method = c("interpolate", "exhaust")) {
  method <- match.arg(method)
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  ncols <- dim(x)[2]
  if (length(w) == 1L && "depth_cm" %in% dimnames(x)[[1]] &&
      x["depth_cm", 1] >= w) {
    w <- c(w, x["depth_cm", 1] - w)
  }

  stopifnot(length(w) == 2L, ncols > 0, is.finite(il), il >= 0, il <= ncols)
  w_sum <- sum(w)

  if (ncols > il) {
    # Add layer at an intermediate depth of existing layers
    x <- x[, c(seq_len(il), NA, (il + 1):ncols), drop = FALSE]

    if (method == "interpolate") {
      if (il > 0) {
        x[, il + 1] <- (x[, il] * w[1] + x[, il + 2] * w[2]) / w_sum

      } else {
        # Add layer at a more shallow depth than any existing layer
        x[, 1] <- x[, 2]
        if ("depth_cm" %in% dimnames(x)[[1]])
          x["depth_cm", 1] <- if (w_sum <= 1 || w[1] > x["depth_cm", 2]) {
            x["depth_cm", 2] * w[1] / w_sum
          } else {
            w[1]
          }
      }

    } else if (method == "exhaust") {
      x[, il + 1] <- x[, il + 2] * w[1] / w_sum
      x[, il + 2] <- x[, il + 2] * w[2] / w_sum
    }

  } else if (ncols == il) {
    # Add a deeper layer than any existing layer
    x <- x[, c(seq_len(ncols), NA), drop = FALSE]

    if (method == "interpolate") {
      x[, il + 1] <- x[, il]
      if ("depth_cm" %in% dimnames(x)[[1]])
        x["depth_cm", il + 1] <- if (w_sum <= 1) {
          x["depth_cm", il] * (1 + w[2] / w_sum)
        } else {
          x["depth_cm", il] + w[2]
        }

    } else if (method == "exhaust") {
      x[, il + 1] <- x[, il] * w[2] / w_sum
      x[, il] <- x[, il] * w[1] / w_sum
    }
  }

  x
}



#' Add/remove soil layers to soil data
#'
#' @param soil_layers A two-dimensional numeric matrix or \code{data.frame}.
#'   Rows represents sites; columns represent soil layers/horizons;
#'   and values are depths of the lower limits of soil layers/horizons in
#'   centimeters.
#' @param requested_soil_layers A numeric vector. The soil depths that should
#'   be added if not already present.
#' @param soil_data A two-dimensional numeric matrix or \code{data.frame}.
#'   Rows represents sites and columns represent
#'   variables x soil layers/horizons combinations.
#'   Column names are formatted as \var{var_Lx} where \var{x} represents
#'   an increasing soil layer/horizon number.
#' @param variables A vector of character strings. The patterns identifying
#'   all variables that are to be updated.
#' @param vars_exhaust A vector of character strings. The patterns
#'   identifying variables whose values are "exhausted" when a soil layer is
#'   split into two layers. See \code{\link{add_layer_to_soil}} for details.
#' @param keep_prev_soildepth A logical value.
#' @param keep_prev_soillayers A logical value.
#' @param verbose A logical value.
#'
#' @return A named list with elements: \describe{
#'   \item{updated}{
#'     A logical value indicating whether any layers/horizons
#'     have been added or removed.
#'   }
#'   \item{soil_layers}{An updated copy of \code{soil_layers}.}
#'   \item{soil_data}{An updated copy of \code{soil_data}.}
#' }
#'
#' @examples
#' soil_layers <- t(data.frame(
#'   site1 = c(20, 81, NA, NA, NA, NA),
#'   site2 = c(8, 20, 150, NA, NA, NA)
#' ))
#'
#' N <- ncol(soil_layers)
#' colnames(soil_layers) <- paste0("depth_L", seq_len(N))
#'
#' soil_data <- cbind(
#'   sand = t(data.frame(
#'     site1 = c(0.5, 0.55, NA, NA, NA, NA),
#'     site2 = c(0.3, 0.35, 0.4, NA, NA, NA)
#'   )),
#'   TranspCoeff = t(data.frame(
#'     site1 = c(0.9, 0.1, NA, NA, NA, NA),
#'     site2 = c(0.7, 0.1, 0.2, NA, NA, NA)
#'   ))
#' )
#' colnames(soil_data) <- paste0(
#'   rep(c("sand", "TranspCoeff"), each = N),
#'   "_L",
#'   seq_len(N)
#' )
#'
#' new_soils <- update_soil_profile(
#'   soil_layers = soil_layers,
#'   requested_soil_layers = c(5, 10, 20, 100, 200),
#'   soil_data = soil_data
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
    variables <- unique(sapply(
      X = strsplit(cns_data, split = "_", fixed = TRUE),
      FUN = function(x) paste0(x[-length(x)], collapse = "_")
    ))

  } else {
    stopifnot(sapply(variables, function(x) any(grepl(x, cns_data))))
  }

  # Determine how to add different soil variables
  # - variables whose values will be exhausted (all others are interpolated)
  tmp <- sapply(vars_exhaust, function(x) any(grepl(x, cns_data)))
  vars_exhaust <- vars_exhaust[tmp]


  # Prepare output container
  has_updates <- FALSE
  new_soil_data <- soil_data
  new_soil_layers <- soil_layers


  #--- Identify layers and groups of sites with the same profile (soil layers)
  ids_layers <- seq_len(dim(soil_layers)[2])
  avail_sl_ids <- apply(soil_layers, 1, paste0, collapse = "x")
  layer_sets <- unique(avail_sl_ids)

  if (length(layer_sets) > 0) {

    soil_data_by_var <- lapply(
      X = variables,
      FUN = function(x) {
        soil_data[, grep(x, cns_data)[ids_layers], drop = FALSE]
      }
    )

    new_soil_data_by_var <- NULL

    #--- Loop through sites with same layer profile and make adjustements
    for (k1 in seq_along(layer_sets)) {
      if (verbose) {
        print(paste(
          "Processing", k1, "out of", length(layer_sets), "soil layer sets."
        ))
      }

      il_set <- avail_sl_ids == layer_sets[k1]
      if (sum(il_set, na.rm = TRUE) == 0) next


      # --- Add requested soil layers
      # Identify which requested layers need to be added
      ldset_prev <- stats::na.exclude(soil_layers[which(il_set)[1], ])
      ldset <- ldset_prev
      req_sd_toadd <- setdiff(requested_soil_layers, ldset)

      if (isTRUE(keep_prev_soildepth)) {
        req_sd_toadd <- req_sd_toadd[req_sd_toadd < max(ldset)]
      }

      if (length(req_sd_toadd) == 0) next

      # Add identified layers
      new_soil_data_by_var <- lapply(
        soil_data_by_var,
        function(x) x[il_set, , drop = FALSE]
      )

      for (lnew in req_sd_toadd) {
        ilnew <- findInterval(lnew, ldset)
        il_weight <- rSW2data::calc_weights_from_depths(ilnew, lnew, ldset)

        for (k2 in seq_along(variables)) {
          new_soil_data_by_var[[k2]] <- rSW2data::add_layer_to_soil(
            new_soil_data_by_var[[k2]],
            il = ilnew,
            w = il_weight,
            method = if (variables[k2] %in% vars_exhaust) {
              "exhaust"
            } else {
              "interpolate"
            }
          )
        }

        ldset <- sort(c(ldset, lnew))
      }


      #--- Remove soil layers that are no longer requested
      if (isFALSE(keep_prev_soillayers)) {
        # Identify which layers to delete
        tmp_req <- if (isTRUE(keep_prev_soildepth)) {
          c(
            requested_soil_layers[requested_soil_layers < max(ldset_prev)],
            ldset_prev[length(ldset_prev)]
          )

        } else {
          requested_soil_layers
        }

        req_sd_todel <- setdiff(ldset, tmp_req)

        if (length(req_sd_todel) > 0) {
          # Delete layers
          stop("Removing soil layers is not yet implemented.")
        }
      }


      #--- Update soil data
      lyrs <- seq_along(ldset)

      for (k2 in seq_along(variables)) {
        tmp <- grep(variables[k2], cns_data)

        if (length(tmp) < length(ldset)) {
          stop(
            "`sw_input_soils` has columns for ", length(tmp), " soil layers, ",
            "but ", length(ldset), " are requested."
          )
        }

        icol <- tmp[lyrs]

        new_soil_data[il_set, icol] <- round(
          new_soil_data_by_var[[k2]][, lyrs],
          if (variables[k2] %in% vars_exhaust) 4L else 2L
        )
      }

      # Update soil layer depths
      new_soil_layers[il_set, lyrs] <-
        matrix(ldset, nrow = sum(il_set), ncol = length(ldset), byrow = TRUE)

      has_updates <- TRUE
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
  if (any(is.na(it))) {
    as.integer(na.exclude(it))
  } else if (length(it) > 1 && diff(it) > 0) {
    (1 + it[1]):(it[2])
  } else {
    it[1]
  }
}
