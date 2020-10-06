
#' Extract elevation, slope, and aspect from National Elevation Dataset
#' \var{NED} for \pkg{SOILWAT2} applications
#'
#' @inheritParams rSW2st::convert_points
#' @param path A character string. The path to the folder containing
#'   \var{NED-1arcsec} files, see \code{file_datasets}.
#' @param file_datasets A named list. The paths/\var{filenames} of
#'   the \var{NED} files relative to \code{path}. The names are
#'   \var{"elev"}, \var{"slope"}, and \var{"aspect"}. At least one must exists.
#' @param units_slope A character string. \var{"degrees"} or \var{"radians"}.
#' @param units_aspect A character string. \var{"degrees"} or \var{"radians"}.
#' @param south_aspect An integer value. The value indicating a south aspect
#'   in degrees, either 0 or 180 degrees. See details.
#' @param method A character string passed to \code{\link[raster]{extract}}.
#' @param flat_lt_slope A numeric value. Slope values less than
#'   \code{flat_lt_slope} are set to 0 and aspect values to \code{NA};
#'   units are the same as the slope dataset.
#'
#' @return A \code{data.frame} with one row per \code{locations} and up to
#'   three columns named like the elements of existing \code{file_datasets},
#'   i.e., \var{"elev"} in units \var{[m a.s.l.]},
#'   \var{"slope"} in units \var{[0 - 90 degree]}, and
#'   \var{"aspect"} in units
#'   \var{[degree; South = 0, E = -90, N = ±180, W = 90; no aspect = NA]}.
#'
#' @section Details:
#'   The raster values are expected to be in units of
#'   \var{[m a.s.l.]} for elevation,
#'   degrees or radians for slope with a
#'   range of \var{[0 - 90 degree]} or \var{[0 - pi/2]}, and
#'   degrees or radians for aspect with an orientation of either
#'   \var{[North = 0 = 360, E = 90, S = 180, W = 270]} and
#'   no aspect indicated by one of \var{\{NA, 999, < 0\}} or
#'   \var{[South = 0, E = -90, N = ±180, W = 90]} and
#'   no aspect indicated by one of \var{\{NA, 999\}}, or
#'   respectively in radians.
#'
#' @section Notes:
#'   First, obtain \var{NED}, e.g.,
#'   via \code{\link[FedData]{get_ned}};
#'   then, derive slope and aspect,
#'   e.g., via \code{\link[raster]{terrain}}
#'   and make sure that gridcells without aspect are correctly encoded.
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("FedData") && curl::has_internet()) {
#'   label_ned <- "ned_1s_example"
#'   path_ned <- "."
#'   filenames_ned_examples <- list(
#'     elev = paste0(label_ned, "_NED_1.tif"),
#'     slope = paste0("slope_", label_ned, "_NED_1.tif"),
#'     aspect = paste0("aspect_", label_ned, "_NED_1.tif")
#'   )
#'
#'   locations <- rSW2st::convert_points(
#'     matrix(data = c(-120.325, -120.328, 43.328, 43.242), nrow = 2),
#'     to_class = "sp",
#'     crs = "+init=epsg:4326"
#'   )
#'   extent_polygon <- FedData::polygon_from_extent(
#'     x = 1.1 * raster::extent(locations),
#'     proj4string = "+init=epsg:4326"
#'   )
#'
#'   ### Download NED
#'   ned_1s_example <- FedData::get_ned(
#'     template = extent_polygon,
#'     label = label_ned,
#'     res = 1,
#'     extraction.dir = path_ned
#'   )
#'
#'   ### Derive slope and aspect
#'   for (opt in c("slope", "aspect")) {
#'     tmp <- raster::terrain(
#'       x = ned_1s_example,
#'       opt = opt,
#'       unit = "degrees",
#'       filename = filenames_ned_examples[[opt]],
#'       datatype = "FLT4S",
#'       options = c("COMPRESS=DEFLATE", "ZLEVEL=9", "INTERLEAVE=BAND")
#'     )
#'   }
#'
#'   ### Get values
#'   vals_topo <- extract_topography_NEDUSA(
#'     locations,
#'     path = path_ned,
#'     file_datasets = filenames_ned_examples,
#'     south_aspect = 180,
#'     method = "simple"
#'   )
#'
#'   # Fix column names for output
#'   cns <- c(elev = "ELEV_m", slope = "Slope_deg", aspect = "Aspect_deg")
#'   colnames(vals_topo) <- cns[colnames(vals_topo)]
#'   vals_topo
#'
#'   # Clean up
#'   unlink(file.path(path_ned, unlist(filenames_ned_examples)))
#' }
#' }
#'
#' @export
extract_topography_NEDUSA <- function(
  locations,
  crs = 4326,
  path,
  file_datasets = list(
    elev = "ned_1s.tif",
    slope = file.path("terrain", "slope_ned_1s.tif"),
    aspect = file.path("terrain", "aspect_ned_1s.tif")
  ),
  units_slope = c("degrees", "radians"),
  units_aspect = c("degrees", "radians"),
  south_aspect = c(180, 0),
  method = "simple",
  flat_lt_slope = 0
) {

  units_slope <- match.arg(units_slope)
  units_aspect <- match.arg(units_aspect)
  south_aspect <- as.integer(south_aspect)

  #--- Check availability
  stopifnot(
    dir.exists(path),
    names(file_datasets) %in% c("elev", "slope", "aspect")
  )

  filepaths_topo <- lapply(file_datasets, function(x) file.path(path, x))
  names(filepaths_topo) <- names(file_datasets)

  has_topo <- file.exists(unlist(filepaths_topo))
  names(has_topo) <- names(filepaths_topo)

  stopifnot(sum(has_topo) > 1)


  #--- Load topographic data
  rtopo <- raster::stack(filepaths_topo[has_topo])


  #--- Extract values
  locations <- rSW2st::convert_points(locations, to_class = "sf", crs = crs)
  locs_tmp <- sf::st_transform(locations, crs = raster::crs(rtopo))

  vals_topo <- raster::extract(
    rtopo,
    locs_tmp,
    method = method
  )


  has_aspect <- "aspect" %in% colnames(vals_topo)

  if (has_aspect) {
    # Convert radians -> degrees
    if (units_aspect == "radians") {
      vals_topo[, "aspect"] <- vals_topo[, "aspect"] * 180 / pi
    }

    # Identify no aspect and rotate origin to S = 0
    is_noaspect <- is.na(vals_topo[, "aspect"]) | vals_topo[, "aspect"] == 999

    if (south_aspect == 180) {
      is_noaspect <- is_noaspect | vals_topo[, "aspect"] < 0

      # rotate origin
      # from: North = 0 = 360, E = 90, S = 180, W = 270
      # to: South = 0, E = -90, N = ±180, W = 90
      vals_topo[, "aspect"] <- rSW2utils::circ_minus(
        vals_topo[, "aspect"],
        180,
        int = 360
      )

    } else if (south_aspect != 0) {
      warning("`south_aspect` with value ", south_aspect, " not implemented.")
    }

    vals_topo[is_noaspect, "aspect"] <- NA
  }

  if ("slope" %in% colnames(vals_topo)) {
    # Slopes < x are considered flat (and without aspect)
    if (isTRUE(flat_lt_slope > 0)) {
      is_noslope <- vals_topo[, "slope"] < flat_lt_slope
      vals_topo[is_noslope, "slope"] <- 0

      if (has_aspect) {
        vals_topo[is_noslope, "aspect"] <- NA
      }
    }

    # Convert radians -> degrees
    if (units_slope == "radians") {
      vals_topo[, "slope"] <- vals_topo[, "slope"] * 180 / pi
    }

  }

  vals_topo
}
