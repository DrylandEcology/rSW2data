# TODO: incomplete and inefficient functionality
#  - fix code for issues fixed in `fetch_soils_from_NRCS_SDA` and
#    `extract_soils_NRCS_SDA`
#  - speed up code by using the `query` argument when reading with `sf::query`


#' Extract soil information for site locations or \var{mukeys} from \var{gNATSGO}
#'
#' @details \var{gNATSGO} is organized in soil map units \var{mukey} that are
#'   spatially explicit (i.e., we can query their values by geographic location)
#'   and in soil map unit components \var{cokey} which are hierarchically
#'   assigned to soil map units (but without explicit spatial arrangement
#'   within a soil map unit).
#'   Because soil texture information is specific to soil map unit components,
#'   geographic location alone is insufficient to query soil texture.
#'
#' @details Soil map unit components \var{cokey} can be identified by
#'   three \code{method}:
#'   \itemize{
#'     \item{component}{
#'       The \code{cokeys} argument is used and its values are checked
#'       against the \var{mukey} values obtained from spatially querying by
#'       the \code{locations}. Note: A \var{cokey} is associated with exactly
#'       one \var{mukey}.
#'     }
#'     \item{ecological_class}{
#'       The \code{ecoclassids} argument is used and its values extract
#'       \var{cokey} values from the table \var{coecoclass} which are then
#'       checked against the \var{mukey} values obtained from spatially
#'       querying by the \code{locations}. Note: Not every \var{cokey} is
#'       associated with an \var{ecoclassid}; and an \var{ecoclassid} may
#'       occur in multiple \var{cokey} within the same \var{mukey}.
#'     }
#'     \item{ecological_class_fix_by_mostcover}{
#'       The same as \var{ecological_class}, but the most widespread
#'       component is selected if they are not uniquely identified. See
#'       \var{dominant_component}.
#'     }
#'     \item{dominant_component}{
#'       For each location, the first \var{cokey} with the
#'       highest representative component percent \var{comppct_r} is used.
#'       This simple method does not take into account ties and low/high
#'       values if available (i.e., \var{comppct_l} and \var{comppct_h}).
#'       See \var{GetDominantComponent.py}
#'       from \url{https://github.com/ncss-tech/SoilDataDevelopmentToolbox}.
#'     }
#'   }
#'
#' @param locations A two dimensional object of site coordinates or
#'   a spatial points object.
#' @param path A character string. The path to the \var{gNATSGO} files.
#' @param file_gNATSGO_gdb A character string. The path to the
#'   \var{NCRS's} original \var{ESRI} geodatabase, relative to \code{path}.
#' @param file_gNATSGO_tif A character string. The path to \var{gNATSGO}
#'   raster converted to \var{GeoTIFF} format where values represent
#'   \var{mukey}, relative to \code{path}.
#' @param method A character string to identify how soil map unit components
#'   (i.e., \var{cokey}) should be identified for given \code{locations};
#'   see details.
#' @param cokeys A vector of character strings identifying a \var{cokey}
#'   for each \code{locations}; used if \code{method = "component"}.
#' @param ecoclassids A vector of character strings identifying a
#'   \var{ecoclassid} for each \code{locations};
#'   used if \code{method = "ecological_class"}.
#' @param digits A scalar integer. The number of digits to which soil texture
#'   values are rounded.
#'
#' @references
#'   Soil Survey Staff (2020) Gridded National Soil Survey Geographic (gNATSGO)
#'    Database for the Conterminous United States.
#'    United States Department of Agriculture,
#'    Natural Resources Conservation Service.
#'    Available online at https://nrcs.app.box.com/v/soils.
#'    (FY2020 official release).
#'
#' @examples
#' locations_ex1 <- array(
#'   data = c(-117.017, -117.225, -117.850, 41.942, 38.067, 42.442),
#'   dim = c(3, 2),
#'   dimnames = list(NULL, c("Longitude", "Latitude"))
#' )
#'
#' locations_ex2 <- array(
#'   data = c(-99.98199, -102.017, -101.0421, 42.65088, 42.93422, 43.06987),
#'   dim = c(3, 2),
#'   dimnames = list(NULL, c("Longitude", "Latitude"))
#' )
#'
#' # Example 1a: cokeys are the dominant components
#' res_ex1a <- extract_soils_gNATSGO(
#'   locations = locations_ex1,
#'   method = "component",
#'   cokeys = c("18632145", "18579702", "3089735:2604944")
#' )
#'
#' # Example 1b: cokeys[2:3] are non-dominant
#' res_ex1b <- extract_soils_gNATSGO(
#'   locations = locations_ex1,
#'   method = "component",
#'   cokeys = c("18632145", "18579704", "3089735:2604948")
#' )
#'
#' # Example 1c: cokeys[3] does not occur at that location --> error
#' res_ex1b <- extract_soils_gNATSGO(
#'   locations = locations_ex1,
#'   method = "component",
#'   cokeys = c("18632145", "18579704", "18579705")
#' )
#'
#' # Example 2a: ecoclassids identify components uniquely
#' res_ex2a <- extract_soils_gNATSGO(
#'   locations = locations_ex2,
#'   method = "ecological_class",
#'   ecoclassids = c("R066XY036NE", "G065XY120NE", "G065XY700NE")
#' )
#'
#' # Example 2b: ecoclassids identify components non-uniquely --> error
#' res_ex2b <- extract_soils_gNATSGO(
#'   locations = locations_ex2,
#'   method = "ecological_class",
#'   ecoclassids = c("G065XY000NE", "R065XY029NE", "G065XY900NE")
#' )
#'
#' # Example 2c: ecoclassids identify components non-uniquely, but select
#' # the ones with the highest cover
#' res_ex2c <- extract_soils_gNATSGO(
#'   locations = locations_ex2,
#'   method = "ecological_class_fix_by_mostcover",
#'   ecoclassids = c("G065XY000NE", "R065XY029NE", "G065XY900NE")
#' )
#'
#' # Example 2d: ecoclassids[3] does not occur at that location --> error
#' res_ex2d <- extract_soils_gNATSGO(
#'   locations = locations_ex2,
#'   method = "ecological_class",
#'   ecoclassids = c("G065XY000NE", "R065XY029NE", "R066XY054NE")
#' )
#'
#' # Example 3: select the dominant components
#' res_ex3 <- extract_soils_gNATSGO(
#'   locations = locations_ex1,
#'   method = "dominant_component"
#' )
#'
#' all.equal(res_ex1a, res_ex3)
#' all.equal(res_ex2a, res_ex3)
#'
#' @export
extract_soils_NRCS_gNATSGO <- function(
  locations,
  mukeys,
  path = "2020 gNATSGO CONUS",
  file_gNATSGO_gdb = "gNATSGO_CONUS.gdb",
  file_gNATSGO_tif = "gNATSGO_CONUS.tif",
  method = c(
    "component", "ecological_class", "ecological_class_fix_by_mostcover",
    "dominant_component"
  ),
  cokeys = NULL,
  ecoclassids = NULL,
  digits = 3L
) {

  stopifnot(
    requireNamespace("sf"),
    sf:::is_driver_available("OpenFileGDB")
  )

  method <- match.arg(method)
  vals_ec <- c("ecological_class", "ecological_class_fix_by_mostcover")

  #------ gNATSGO metadata
  # NCRS's original ESRI geodatabase
  file_gNATSGO_gdb <- file.path(path, file_gNATSGO_gdb)

  stopifnot(file.exists(file_gNATSGO_gdb))


  meta_gnatsgo <- list(
    nmax_horizons = 13,
    varnames = c(
      "depth_cm", "bulkdensity", "sand_wpct", "clay_wpct", "silt_wpct",
      "frag_volpct"
    ),
    vars = c(
      "hzdepb_r",
      "dbovendry_r", "sandtotal_r", "claytotal_r", "silttotal_r",
      "fragvol_r"
    ),
    tables = c(
      "chorizon", "chorizon", "chorizon", "chorizon", "chorizon",
      "chfrags"
    )
  )

  var_texture <- c(
    "dbovendry_r",
    "fragvol_r",
    "sandtotal_r", "claytotal_r", "silttotal_r"
  )


  #------ Collect identifying information

  #--- Extract mukey (soil map unit key) for site locations
  locs_keys <- data.frame(
    mukey = if (missing(mukeys)) {
      tmp <- query_mukeys_spatially_NRCS_gNATSGO(
        locations = locations,
        path = path,
        file_gNATSGO_tif = file_gNATSGO_tif
      )
      as.character(as.integer(tmp[["mukeys"]]))

    } else {
      mukeys
    },
    cokey = NA
  )

  stopifnot(!anyNA(locs_keys[["mukey"]]))

  N_sites <- nrow(locs_keys)



  #--- Extract all cokey values (soil map unit component key) for sites
  x_component <- sf::st_read(file_gNATSGO_gdb, layer = "component")

  ids <- x_component[, "mukey"] %in% locs_keys[, "mukey"]
  varkeys <- c("mukey", "cokey", "comppct_r", "compname")
  list_keys <- x_component[ids, varkeys, drop = FALSE]


  #------ Identify/select a single `cokey` for each site
  if (method == "component") {
    stopifnot(
      method == "component" && !is.null(cokeys),
      N_sites == length(cokeys)
    )

  } else if (method %in% vals_ec) {
    stopifnot(
      method %in% c("ecological_class", "ecological_class_fix_by_mostcover") &&
        !is.null(ecoclassids),
      N_sites == length(ecoclassids)
    )
  }


  if (method %in% c("component", vals_ec)) {

    if (method == "component") {
      locs_keys[, "cokey"] <- cokeys
      input_keys_ID <- apply(locs_keys, 1, paste, collapse = "_x_")
      has_multiple_cokeys_per_mukey <- FALSE

    } else if (method %in% vals_ec) {
      # Extract cokey values from ecoclassids
      x_coecoclass <- sf::st_read(file_gNATSGO_gdb, layer = "coecoclass")
      ids <- x_coecoclass[, "ecoclassid"] %in% ecoclassids
      list_ecokeys <- unique(x_coecoclass[ids, c("ecoclassid", "cokey")])

      # Assign cokey to mukey
      ids <- match(list_ecokeys[, "cokey"], list_keys[, "cokey"], nomatch = 0)
      list_keys <- cbind(
        list_keys[ids, ],
        ecoclassid = list_ecokeys[ids > 0, "ecoclassid"]
      )

      has_multiple_cokeys_per_mukey <- anyDuplicated(list_keys[["mukey"]]) > 0

      if (!has_multiple_cokeys_per_mukey) {
        # Sort according to sites
        ids <- match(locs_keys[, "mukey"], list_keys2[, "mukey"], nomatch = NA)
        tmp <- c(varkeys[-1], "ecoclassid")
        locs_keys[, tmp] <- list_keys2[ids, tmp]

        # Check whether we have a unique cokey for each mukey
        input_keys_ID <- apply(
          X = locs_keys[, c("mukey", "cokey")],
          MARGIN = 1,
          FUN = paste,
          collapse = "_x_"
        )

      } else if (method == "ecological_class_fix_by_mostcover") {
        stop("Provided 'ecoclassid' do not identify 'cokey' uniquely.")
      }
    }

    # Check that input cokeys can occur at site, i.e.,
    # for spatially inferred `mukey`
    list_keys_ID <- apply(
      X = list_keys[, c("mukey", "cokey")],
      MARGIN = 1,
      FUN  = paste,
      collapse = "_x_"
    )

    hasnt_cokeys <- !(input_keys_ID %in% list_keys_ID)

    if (any(hasnt_cokeys)) {
      stop(
        "Provided 'cokey' values do not occur in 'gNATSGO' at ",
        sum(hasnt_cokeys), " location(s) / for the 'mukey' value(s): ",
        paste0(
          if (!missing(locations)) {
            apply(
              sp::coordinates(locations[hasnt_cokeys, ]),
              MARGIN = 1,
              FUN = paste,
              collapse = "/"
            )
          },
          " for mukey x cokey combination = ",
          gsub("_x_", " x ", input_keys_ID[hasnt_cokeys]),
          collapse = "; "
        )
      )
    }

  } else {
    has_multiple_cokeys_per_mukey <- TRUE
  }


  if (method == "dominant_component" || has_multiple_cokeys_per_mukey) {
    # Select the first `cokey` with the largest `comppct_r` (for each `mukey`)
    ids <- order(
      list_keys[["mukey"]],
      - list_keys[["comppct_r"]],
      list_keys[["cokey"]]
    )

    tmp1 <- list_keys[ids, c("mukey", "cokey"), drop = FALSE]
    tmp2 <- list_keys[ids, , drop = FALSE]
    list_unique_keys <- tmp2[!duplicated(tmp1[["mukey"]]), , drop = FALSE]

    # Sort according to sites
    ids <- match(
      x = locs_keys[, "mukey"],
      table = list_unique_keys[, "mukey"],
      nomatch = NA
    )
    locs_keys[, "cokey"] <- list_unique_keys[ids, "cokey"]
  }


  #------ Extract soil information for site based on selected `cokey`
  #--- Prepare output container
  res <- array(
    data = NA,
    dim = c(
      N_sites,
      length(meta_gnatsgo[["vars"]]),
      meta_gnatsgo[["nmax_horizons"]]
    ),
    dimnames = list(NULL, meta_gnatsgo[["varnames"]], NULL)
  )


  #--- Read soil tables
  x_chorizon <- sf::st_read(file_gNATSGO_gdb, layer = "chorizon")

  # Select required 'chkey' based on 'cokey'
  ids <- x_chorizon[, "cokey"] %in% locs_keys[, "cokey"]
  use_vars <- meta_gnatsgo[["vars"]][meta_gnatsgo[["tables"]] == "chorizon"]
  list_soils <- x_chorizon[ids, c("cokey", "chkey", use_vars), drop = FALSE]

  # Add other tables and link via 'chkey'
  other_tables <- setdiff(unique(meta_gnatsgo[["tables"]]), "chorizon")

  for (k in seq_along(other_tables)) {
    x_table <- sf::st_read(file_gNATSGO_gdb, layer = other_tables[k])

    tmp <- meta_gnatsgo[["tables"]] == other_tables[k]
    use_vars <- meta_gnatsgo[["vars"]][tmp]

    list_soils <- merge(
      x = list_soils,
      y = x_table[, c("chkey", use_vars)],
      by = "chkey",
      all.x = TRUE
    )
  }

  # Calculate a single (mean) value for each 'cokey' x 'chkey' combination
  # (e.g., table "chfrags" may have multiple entries per horizon)
  list_soils2 <- aggregate(
    x = list_soils[, -(1:2)],
    by = list_soils[c("chkey", "cokey")],
    FUN = mean, # TODO: this should be the sum aross `fragvol_r`
    na.rm = TRUE
  )

  # Sort by depth and add soil layer number
  ids <- order(list_soils2[["cokey"]], list_soils2[["hzdepb_r"]])
  tmp_sorted <- list_soils2[ids, ]

  tmp <- table(tmp_sorted[, "cokey"])
  stopifnot(rep(names(tmp), tmp) == tmp_sorted[, "cokey"])
  nmax_layers <- max(tmp)
  LayerNo <- unlist(unname(lapply(tmp, seq_len)))
  list_soils3 <- cbind(tmp_sorted, LayerNo)


  #--- Convert units & rounding
  # Convert % to fraction
  var_pct_to_fraction <- c(
    "fragvol_r", "sandtotal_r", "claytotal_r", "silttotal_r"
  )
  list_soils3[, var_pct_to_fraction] <- list_soils3[, var_pct_to_fraction] / 100

  # Round texture
  list_soils3[, "fragvol_r"] <- round(list_soils3[, "fragvol_r"], 3)

  var_stxt <- c("sandtotal_r", "claytotal_r", "silttotal_r")
  hasvals <-
    complete.cases(list_soils3[, var_stxt]) &
    apply(list_soils3[, var_stxt], 1, sum, na.rm = TRUE) > 0
  list_soils3[hasvals, var_stxt] <- rSW2utils::scale_rounded_by_sum(
    x = list_soils3[hasvals, var_stxt],
    digits = 3,
    icolumn_adjust = 3
  )


  #--- Create depth table
  tmp_depths <- reshape2::acast(
    data = list_soils3[, c("LayerNo", "cokey", "hzdepb_r")],
    formula = cokey ~ LayerNo,
    value.var = "hzdepb_r"
  )

  ids <- match(locs_keys[, "cokey"], rownames(tmp_depths), nomatch = NA)
  locs_table_depths <- tmp_depths[ids, ]
  colnames(locs_table_depths) <- paste0("depth_L", colnames(locs_table_depths))

  locs_table_depths <- cbind(
    SoilDepth_cm = apply(locs_table_depths, 1, max, na.rm = TRUE),
    depth_L = locs_table_depths
  )


  #--- Create texture table
  tmp_texture <- reshape2::acast(
    data = reshape2::melt(
      data = list_soils3[, c("LayerNo", "cokey", var_texture)],
      id.vars = c("LayerNo", "cokey")
    ),
    formula = cokey ~ LayerNo + variable
  )

  ids <- match(locs_keys[, "cokey"], rownames(tmp_texture), nomatch = NA)
  locs_table_texture <- tmp_texture[ids, ]

  tmp <- strsplit(colnames(locs_table_texture), split = "_")
  colnames(locs_table_texture) <- sapply(
    X = tmp,
    FUN = function(x) paste0(x[2], "_L", x[1])
  )


  #--- Return tables
  list(
    table_keys = locs_keys,
    table_depths = locs_table_depths,
    table_texture = locs_table_texture
  )
}


#' Spatially query \var{mukey} values for point locations from a local copy
#' of a \var{NRCS} \var{gNATSGO} database
#'
#' @inheritParams query_mukeys_spatially_NRCS_SDA
#' @param path A character string. The path to the \var{gNATSGO} files.
#' @param file_gNATSGO_tif A character string. The path to \var{gNATSGO}
#'   raster converted to \var{GeoTIFF} format where values represent
#'   \var{mukey}, relative to \code{path}.
#'
#' @return A named list with two elements: \itemize{
#'   \item{ref} The data reference.
#'   \item{mukeys} A vector with a \var{mukey} value for each \code{locations}.
#' }
#'
#' @export
query_mukeys_spatially_NRCS_gNATSGO <- function(locations,
  path, file_gNATSGO_tif, ...
) {

  # gNATSGO raster converted to GeoTIFF format: values represent `mukey`
  file_gNATSGO_tif <- file.path(path, file_gNATSGO_tif)

  stopifnot(file.exists(file_gNATSGO_tif))

  #------ Make sure inputs are correctly formatted
  is_sp <- inherits(locations, "SpatialPoints")
  is_sf <- inherits(locations, "sf")

  if (!(is_sp || is_sf)) {
    warning("Assume that coordinates of 'locations' are WGS84.")

    locations <- sp::SpatialPoints(
      coords = locations,
      proj4string = as(sf::st_crs(4326), "CRS")
    )

    is_sp <- TRUE
  }

  rgnatsgo <- raster::raster(x = file_gNATSGO_tif)

  locs_tmp <- if (is_sp) {
    sp::spTransform(locations, CRSobj = raster::crs(rgnatsgo))
  } else if (is_sf) {
    sf::st_transform(locations, crs = raster::crs(rgnatsgo))
  }

  raster::extract(rgnatsgo, locs_tmp)
}
