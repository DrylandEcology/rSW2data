
create_reference_for_NRCS_SDA <- function() {
  paste0(
    "Soil Survey Staff, Natural Resources Conservation Service, ",
    "United States Department of Agriculture. Web Soil Survey. ",
    "Available online at https://websoilsurvey.nrcs.usda.gov/. ",
    "Accessed [",
    format(as.POSIXlt(Sys.Date()), "%Y-%b-%e"),
    "]"
  )
}



#' Check whether a \var{NRCS} soil horizon is organic
#'
#' Based on function \code{CheckTexture()}, version 2020-08-31, of the
#' \var{SoilDataDevelopmentToolbox}.
#'
#' @param x A two-dimensional character object. Required columns are
#'   \var{taxorder}, \var{taxsubgrp}, \var{desgnmaster}, \var{texture},
#'   and \var{lieutex}. Each horizon is represented by exactly one row.
#'
#' @return A logical vector representing each horizon. \code{TRUE} indicates
#'   that a horizon is considered organic; default is \code{FALSE}
#'   representing mineral soils. \code{NA}s in the input propagate.
#'
#' @references Code based on \var{CheckTexture()} version \var{2020-Aug-31} from
# nolint start
#'   \url{https://github.com/ncss-tech/SoilDataDevelopmentToolbox/blob/master/SDA_Valu2Table.py}
# nolint end
#'
#' @examples
#' x <- data.frame(
#'   taxorder = c("Histosols", "x", "x", "x", "x", "x", NA),
#'   taxsubgrp = c("x", "histic", "x", "x", "x", "x", NA),
#'   desgnmaster = c("L", "L", "O", "x", "x", "x", NA),
#'   texture = c("x", "x", "x", "CE", "x", "x", NA),
#'   lieutex = c("x", "x", "x", "x", "Muck", "x", NA)
#' )
#'
#' cbind(
#'   organic = is_NRCS_horizon_organic(x),
#'   expected_organic = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, NA)
#' )
#'
#' if (curl::has_internet()) {
#'   x <- fetch_soils_from_NRCS_SDA(mukeys_unique = c(471168, 1606800))
#'   is_NRCS_horizon_organic(x)
#' }
#'
#' @export
is_NRCS_horizon_organic <- function(x) {

  vars <- c("taxorder", "taxsubgrp", "desgnmaster", "texture", "lieutex")
  cns <- colnames(x)

  for (var in vars) {
    if (!(var %in% cns)) {
      stop(shQuote(var), " is a required column name, but cannot be found.")
    }
  }

  lieuList <- c(
    "Slightly decomposed plant material",
    "Moderately decomposed plant material",
    "Highly decomposed plant material",
    "Undecomposed plant material",
    "Muck",
    "Mucky peat",
    "Peat",
    "Coprogenous earth"
  )

  txList <- c(
    "CE", "COP-MAT", "HPM", "MPM", "MPT", "MUCK", "PDOM", "PEAT",
    "SPM", "UDOM"
  )

  # propagate NAs conservatively
  is_mineral <-
    x[, "taxorder"] == "Histosols" |
    grepl("histic", x[, "taxsubgrp"], ignore.case = TRUE)

  is_organic <- cbind(
    desgnmaster = x[, "desgnmaster"] %in% c("O", "L"),
    texture = x[, "texture"] %in% txList,
    lieutex = x[, "lieutex"] %in% lieuList
  )
  is_organic[is.na(x[, c("desgnmaster", "texture", "lieutex")])] <- NA

  !is_mineral & apply(is_organic, 1, any)
}


#' Determine soil depth
#'
#' @param x A \code{data.frame}. The result of
#'   function \code{\link{fetch_soils_from_NRCS_SDA}} that
#'   returned horizon-level data for a set of \var{mukey} values.
#' @param target_cokeys A vector. The set of \var{cokey} values for which
#'   soil depth is to be determined based on \code{x}.
#' @param restrict_by_ec_or_ph A logical value. Include depth restrictions
#'   by \code{ph <= 3.5} or \code{ec >= 16}.
#'
#' @return A \code{data.frame} with at least three columns. Each row represents
#'   one value of \code{target_cokeys}. The columns are: \itemize{
#'     \item \var{N_horizons}: The number of soil horizons/layers.
#'     \item \var{SoilDepth_cm}: The soil depth in centimeters.
#'     \item \var{depth_Lx}: The lower depth of soil horizon/layer \var{x}.
#'       Note: \var{depth_L1} may vary from \code{x[, "hzdepb_r"]}.
#'   }
#'
#' @references Code based on \var{CalcRZDepth()} version 2020-08-31:
# nolint start
#'   \url{https://github.com/ncss-tech/SoilDataDevelopmentToolbox/blob/master/SDA_Valu2Table.py}
# nolint end
#'   Note: currently ignores "dense" layer restrictions
#'
#' @examples
#' if (curl::has_internet()) {
#'   x <- fetch_soils_from_NRCS_SDA(mukeys_unique = c(471168, 1606800))
#'
#'   calculate_NRCS_soil_depth(x, restrict_by_ec_or_ph = FALSE)
#'
#'   x2 <- cbind(x, organic = is_NRCS_horizon_organic(x))
#'   calculate_NRCS_soil_depth(x2, restrict_by_ec_or_ph = TRUE)
#' }
#'
#' @export
calculate_NRCS_soil_depth <- function(x, target_cokeys,
  restrict_by_ec_or_ph = TRUE
) {

  vars <- c(
    "COKEY", "Horizon_No", "hzdepb_r", "Horizon_depth",
    "RootZoneRestriction_depth", "Bedrock_depth",
    "sandtotal_r", "claytotal_r", "silttotal_r"
  )

  if (restrict_by_ec_or_ph) {
    vars <- c(
      vars,
      "hzdept_r", "taxorder", "taxsubgrp", "organic", "ec_r", "ph1to1h2o_r"
    )
  }

  cns <- colnames(x)

  for (var in vars) {
    if (!(var %in% cns)) {
      stop(shQuote(var), " is a required column name, but cannot be found.")
    }
  }

  if (missing(target_cokeys)) {
    target_cokeys <- x[["COKEY"]]
  }

  # Determine number of soil horizons with complete soil texture values
  tmp <- aggregate(
    x = apply(
      x[, c("sandtotal_r", "claytotal_r", "silttotal_r"), drop = FALSE],
      MARGIN = 1,
      FUN = function(x) sum(!is.na(x))
    ),
    by = x["COKEY"],
    FUN = function(x) sum(x == 3)
  )

  ids <- match(target_cokeys, tmp[["COKEY"]], nomatch = NA)
  target_complete_soiltexture <- tmp[ids, "x"]


  # Calculate additional restrictions
  if (restrict_by_ec_or_ph) {
    x[, "is_organic"] <- x[, "organic"] %in% TRUE
    x[, "is_histosol_histic"] <-
      x[, "taxorder"] %in% "Histosols" |
      grepl("histic", x[, "taxsubgrp"], ignore.case = TRUE)

    # Restrictions pH < 3.5 or EC > 16 apply only
    # if horizon is non-organic and not a histosol/histic soil
    x[, "check"] <- !x[, "is_organic"] & !x[, "is_histosol_histic"]

    tmp <- by(
      data = x,
      INDICES = x[, "COKEY"],
      FUN = function(xc) {
        is_ec_restricted <-
          xc[, "check"] & !is.na(xc[, "ec_r"]) & xc[, "ec_r"] >= 16
        is_ph_restricted <-
          xc[, "check"] & !is.na(xc[, "ph1to1h2o_r"]) &
          xc[, "ph1to1h2o_r"] <= 3.5

        c(
          ec_restriction_depth = xc[which(is_ec_restricted)[1], "hzdept_r"],
          ph_restriction_depth = xc[which(is_ph_restricted)[1], "hzdept_r"]
        )
      },
      simplify = FALSE
    )

    restriction2_depth <- matrix(
      data = unlist(tmp[match(x[["COKEY"]], names(tmp))]),
      ncol = 2,
      byrow = TRUE
    )
  }


  # Convert to wide format (one row for each point location)
  tmp_depths <- reshape2::acast(
    data = x[, c("Horizon_No", "COKEY", "hzdepb_r")],
    formula = COKEY ~ Horizon_No,
    value.var = "hzdepb_r"
  )

  ids <- match(target_cokeys, rownames(tmp_depths), nomatch = NA)
  locs_table_depths <- tmp_depths[ids, ]
  colnames(locs_table_depths) <- paste0("depth_L", colnames(locs_table_depths))


  # Determine soil depth as depth of shallowest restriction
  soil_depth_cm <- pmin(
    x[["Horizon_depth"]],
    x[["RootZoneRestriction_depth"]],
    x[["Bedrock_depth"]],
    na.rm = TRUE
  )

  if (restrict_by_ec_or_ph) {
    soil_depth_cm <- pmin(
      soil_depth_cm,
      restriction2_depth[, 1],
      restriction2_depth[, 2],
      na.rm = TRUE
    )
  }

  ids <- match(target_cokeys, x[["COKEY"]], nomatch = NA)
  soil_depth_cm <- round(soil_depth_cm)[ids]


  # Case:
  #   * soil depth is missing and
  #   * no complete soil texture available
  # ==> adjust soil depth to 0
  ids <-
    is.na(soil_depth_cm) &
    target_complete_soiltexture == 0
  soil_depth_cm[ids] <- 0

  # Adjust soil depth and/or layers where needed
  locs_table_depths <- cbind(
    SoilDepth_cm = soil_depth_cm,
    depth_L = locs_table_depths
  )


  # Case:
  #   * soil_depth disagrees with horizon_depth
  #   * soil_depth > 0 and N_horizons_tmp > 0
  # ==> adjust depth_Lx to soil_depth
  L_at_soildepth <- apply(
    X = locs_table_depths,
    MARGIN = 1,
    FUN = function(x) {
      findInterval(x[1], c(0, na.exclude(x[-1])), left.open = TRUE)
    }
  )
  ids <- which(!apply(
    X = locs_table_depths,
    MARGIN = 1,
    FUN = function(x) x[1] == 0 || all(is.na(x[-1])) || x[1] %in% x[-1]
  ))
  locs_table_depths[cbind(ids, 1 + L_at_soildepth[ids])] <-
    locs_table_depths[ids, "SoilDepth_cm"]

  # Case:
  #   * soil_depth > 0 and horizon_depth L1 in (0, NA) and
  #   * target_complete_soiltexture > 0
  # ==> adjust depth_L1 to soil_depth
  ids <- apply(
    X = cbind(
      target_complete_soiltexture,
      locs_table_depths
    ),
    MARGIN = 1,
    FUN = function(x) x[1] > 0 & x[2] > 0 & x[3] %in% c(0, NA)
  )
  locs_table_depths[ids, "depth_L1"] <- locs_table_depths[ids, "SoilDepth_cm"]

  # Case:
  #   * soil_depth > 0 and horizon_depth L1 in (0, NA) and
  #   * target_complete_soiltexture == 0
  # ==> adjust soil_depth to 0
  ids <- apply(
    X = cbind(
      target_complete_soiltexture,
      locs_table_depths
    ),
    MARGIN = 1,
    FUN = function(x) x[1] == 0 & x[2] > 0 & x[3] %in% c(0, NA)
  )
  locs_table_depths[ids, "SoilDepth_cm"] <- 0

  # Case:
  #   * soil_depth == 0 and horizon_depth L1 > 0 and
  #   * target_complete_soiltexture > 0
  # ==> adjust soil_depth to depth_L1
  ids <- apply(
    X = cbind(
      target_complete_soiltexture,
      locs_table_depths
    ),
    MARGIN = 1,
    FUN = function(x) x[1] > 0 & x[2] == 0 & !is.na(x[3]) & x[3] > 0
  )
  locs_table_depths[ids, "SoilDepth_cm"] <- locs_table_depths[ids, "depth_L1"]


  # Clean layer depths > soil dept
  ids <- locs_table_depths[, "SoilDepth_cm"] < locs_table_depths[, -1]
  locs_table_depths[, -1][ids] <- NA


  # Put together for output
  cbind(
    N_horizons = apply(
      X = locs_table_depths[, -1],
      MARGIN = 1,
      function(x) sum(!is.na(x))
    ),
    depth_L = locs_table_depths
  )
}



#' Spatially query \var{mukey} values for point locations from \var{NRCS}
#' web-based \var{SDA} service
#'
#' @inheritParams rSW2st::convert_points
#' @param db A character string. Query \var{mukey} from the \var{SSURGO} or
#'   from the \var{STATSGO} soil database.
#' @param ... Currently ignored.
#' @param chunk_size An integer value. The size of chunks into which
#'   \code{locations} is broken up and looped over for processing.
#' @param progress_bar A logical value. Display a progress bar as the code
#'   loops over the chunks?
#'
#' @return A named list with two elements: \itemize{
#'   \item{\var{ref}} The data reference.
#'   \item{\var{mukeys}} A vector with a \var{mukey} value for each
#'     \code{locations}.
#' }
#'
#' @examples
#' if (curl::has_internet()) {
#'   locations <- matrix(
#'     data = c(-120.325, -111.245, 39.855, 36.753),
#'     nrow = 2
#'   )
#'
#'   fetch_mukeys_spatially_NRCS_SDA(locations)
#' }
#'
#' @export
fetch_mukeys_spatially_NRCS_SDA <- function(
  locations,
  crs = 4326,
  db = c("SSURGO", "STATSGO"),
  ...,
  chunk_size = 50L,
  progress_bar = FALSE
) {

  stopifnot(
    requireNamespace("soilDB"),
    "db" %in% methods::formalArgs(soilDB::SDA_spatialQuery),
    curl::has_internet()
  )

  #------ Make sure inputs are correctly formatted
  db <- match.arg(db)

  # We convert to `sp` because of `soilDB::SDA_spatialQuery` (v2.5.7)
  locations <- rSW2st::convert_points(locations, to_class = "sp", crs = crs)


  #--- Extract mukeys for each point location
  x <- list()

  ids_chunks <- rSW2utils::make_chunks(
    nx = length(locations),
    chunk_size = chunk_size
  )

  N_chunks <- length(ids_chunks)
  progress_bar <- progress_bar && N_chunks > 2

  if (progress_bar) {
    message("Fetch 'mukey' values from ", shQuote(db))

    has_progress_bar <- requireNamespace("utils")

    if (has_progress_bar) {
      pb <- utils::txtProgressBar(max = N_chunks, style = 3)
    } else {
      warning("Progress bar requested but package 'utils' is not available.")
    }

  } else {
    has_progress_bar <- FALSE
  }

  for (k in seq_len(N_chunks)) {
    res_mukeys <- try(
      # Warnings: "CRS object has comment, which is lost in output"
      soilDB::SDA_spatialQuery(
        geom = locations[ids_chunks[[k]], ],
        db = db,
        what = "geom"
      ),
      silent = FALSE
    )

    if (inherits(res_mukeys, "try-error")) {
      warning("Spatial SDA query produced error: chunk = ", k)
      x[[k]] <- rep(NA, length(ids_chunks[[k]]))

    } else if (!inherits(res_mukeys, "SpatialPolygons")) {
      warning("Spatial SDA query returned non-spatial object: chunk = ", k)
      x[[k]] <- rep(NA, length(ids_chunks[[k]]))

    } else {
      # Return values of `SDA_spatialQuery` are not ordered by input `geom`,
      # as of soilDB v2.5.7
      x[[k]] <- sp::over(
        x = sp::spTransform(
          locations[ids_chunks[[k]], ],
          CRSobj = sp::proj4string(res_mukeys)
        ),
        y = res_mukeys
      )[, "mukey"]
    }

    if (has_progress_bar) {
      utils::setTxtProgressBar(pb, k)
    }
  }

  if (has_progress_bar) {
    close(pb)
  }

  list(
    ref = create_reference_for_NRCS_SDA(),
    mukeys = unlist(x)
  )
}



#' Download soil data from \var{NRCS} \var{SDA} web service
#'
#' @param mukeys_unique An integer vector with unique \var{mukey} values.
#' @param sql_template A character vector.
#'   A valid \var{T-SQL} query with a \var{WHERE} clause so that the code can
#'   inject chunks of \code{mukeys_unique} values,
#'   i.e., \var{"mapunit.mukey IN (\%s)"}.
#' @param majcompflag A character string. \var{"subset"} keeps
#'   the WHERE clause \var{component.majcompflag = 'Yes'} that is contained in
#'   \code{sql_template}; \var{"ignore"} removes it from the query. Note that
#'   the field \var{"majcompflag} exists only in the \var{SSURGO} version
#'   of the \var{component} table, but not in the \var{STATSGO} version.
#' @param chunk_size An integer value. The size of chunks into which
#'   \code{mukeys_unique} is broken up and looped over for processing.
#' @param progress_bar A logical value. Display a progress bar as the code
#'   loops over the chunks?
#'
#' @return A \var{data.frame} according to the specifications of \code{sql}.
#'
#' @section Notes: A live internet connection is required to access \var{SDA}.
#'
#' @section Notes: This is a function with minimal functionality;
#' use \code{\link{extract_soils_NRCS_SDA}} for a user-friendly interface.
#'
#' @seealso \code{\link[soilDB]{SDA_query}}
#'
#' @export
fetch_soils_from_NRCS_SDA <- function(
  mukeys_unique,
  sql_template = readLines(
    system.file(
      "NRCS", "nrcs_sql_template.sql",
      package = "rSW2data",
      mustWork = TRUE
    )
  ),
  majcompflag = c("subset", "ignore"),
  chunk_size = 1000L,
  progress_bar = FALSE
) {

  stopifnot(requireNamespace("soilDB"), curl::has_internet())

  majcompflag <- match.arg(majcompflag)

  mukeys_unique <- as.integer(mukeys_unique)
  stopifnot(!anyDuplicated(mukeys_unique))

  ids_chunks <- rSW2utils::make_chunks(
    nx = length(mukeys_unique),
    chunk_size = chunk_size
  )

  N_chunks <- length(ids_chunks)

  #--- Extract soil horizon data for mukeys (chunked)
  res <- list()

  # trim off comments at top of file
  sql_base <- sql_template[-{1:(grep("SELECT", sql_template)[1] - 1)}]

  # remove majcompflag (may be necessary for STATSGO)
  if (majcompflag == "ignore") {
    txt_majcompflag <- "AND component.majcompflag = 'Yes'"
    tmp <- regexpr(txt_majcompflag, sql_base, fixed = TRUE)
    iline <- which(tmp > 0)[1]
    sql_base[iline] <- sub(txt_majcompflag, "", sql_base[iline])
  }

  progress_bar <- progress_bar && N_chunks > 2

  if (progress_bar) {
    print("Fetch soil information from NRCS SDA:")

    has_progress_bar <- requireNamespace("utils")

    if (has_progress_bar) {
      pb <- utils::txtProgressBar(max = N_chunks, style = 3)
    } else {
      warning("Progress bar requested but package 'utils' is not available.")
    }

  } else {
    has_progress_bar <- FALSE
  }


  for (k in seq_along(ids_chunks)) {
    # Prepare SQL query for SDA
    sql <- sql_base

    # Insert requested mukey values
    tmp <- regexpr("mukey IN (%s)", sql, fixed = TRUE)
    iline <- which(tmp > 0)[1]
    sql[iline] <- sprintf(
      fmt = sql[iline],
      paste(shQuote(mukeys_unique[ids_chunks[[k]]]), collapse = ",")
    )

    # Send query to SDA
    # Suppress messages about returning a data.frame
    res[[k]] <- suppressMessages(soilDB::SDA_query(paste(sql, collapse = " ")))
    stopifnot(!inherits(res[[k]], "try-error"))

    if (has_progress_bar) {
      utils::setTxtProgressBar(pb, k)
    }
  }

  if (has_progress_bar) {
    close(pb)
  }

  do.call(rbind, res)
}


#' Extract soil information from the Soil Data Access \var{SDA} service by
#' \var{NRCS} for \pkg{SOILWAT2} applications
#'
#' @inheritParams rSW2st::convert_points
#' @inheritParams fetch_mukeys_spatially_NRCS_SDA
#' @param mukeys A character or integer vector. List of soil map unit keys
#'   for which soil information should be extracted. Provide \code{locations}
#'   or \code{mukeys}.
#' @param method A character string. Method indicating whether \var{SDA}
#'   should query \var{"SSURGO"}, \var{"STATSGO"}, or
#'   \var{"SSURGO_then_STATSGO"} which attempts to replace
#'   \code{locations} without \var{"SSURGO"} soil information,
#'   e.g., due to unmapped areas, with \var{"STATSGO"} data.
#' @param remove_organic_horizons A character string. Method
#'   indicating how to deal with organic horizons as determined by
#'   function \code{\link{is_NRCS_horizon_organic}}. See details.
#' @param replace_missing_fragvol_with_zero A character string. Method
#'   indicating how missing/null values of rock/gravel fragment fractions
#'   should be interpreted. See details.
#' @param estimate_missing_bulkdensity A logical value. Estimate missing
#'   bulk density values from saturated water content and gravel volume.
#'   See \code{\link{estimate_bulkdensity}}.
#' @param impute_locf A logical value. Impute missing values with a
#'   shallow-depth value carried deeper approach (in analogy to \var{LOCF}).
#'   Consequently, missing values in the shallowest horizon are not imputed.
#'   See \code{\link[rSW2utils]{impute_df}}.
#' @param digits An integer value. The number of digits to which soil texture
#'   variables are rounded. Skip rounding if \code{NA} or \code{NULL}.
#' @param verbose A logical value.
#' @inheritParams fetch_soils_from_NRCS_SDA
#' @inheritParams calculate_NRCS_soil_depth
#'
#' @section Details: \var{NRCS} soil datasets \var{SSURGO} and \var{STATSGO} are
#'   organized in soil map units \var{mukey} that are
#'   spatially explicit (i.e., we can query their values by geographic location)
#'   and within each \var{mukey} into soil map unit components \var{cokey}
#'   which have no explicit spatial arrangement within a soil map unit.
#'   Because soil texture information is specific to soil map unit components,
#'   geographic location alone is insufficient to query soil texture.
#'
#'   This function relies that soil information of exactly one \var{cokey}
#'   per each \code{location} or \code{mukeys} is returned by
#'   \code{\link{fetch_soils_from_NRCS_SDA}}. The default \var{SQL} template
#'   \var{"nrcs_sql_template.sql"} extracts the "dominant component".
#'   The dominant component is defined as the the first \var{cokey} with the
#'   highest representative component percent \var{comppct_r}.
#'   See \var{GetDominantComponent.py}
#'   from \url{https://github.com/ncss-tech/SoilDataDevelopmentToolbox}.
#'
#' @section Details:
#'   The argument \code{remove_organic_horizons} is one of
#'   \describe{
#'     \item{\var{"all"}}{
#'       All organic layers (at surface or buried) are removed and horizon
#'       number and depths are recalculated.
#'     }
#'     \item{\var{"at_surface"}}{
#'       Organic layer(s) at the soil surface are removed and horizon
#'       number and depths are recalculated. Buried organic horizons remain
#'       unmodified.
#'     }
#'     \item{\var{"none"}}{
#'        Horizons are not modified.
#'     }
#'   }
#'
#' @section Details:
#'   The argument \code{replace_missing_fragvol_with_zero} is one of
#'   \describe{
#'     \item{\var{"all"}}{
#'       Missing/null values of rack/gravel fragments are
#'       replaced by 0 %.
#'       See also argument \var{nullFragsAreZero} of
#'       function \code{\link[soilDB]{fetchSDA}}.
#'     }
#'     \item{\var{"at_surface"}}{
#'       Missing/null values of rack/gravel fragments are
#'       replaced by 0 % in the shallowest horizon only.
#'       Note, remaining missing values in deeper horizons
#'       can subsequently be imputed by argument \code{impute_locf}.
#'     }
#'     \item{\var{"none"}}{
#'        Missing/null values remain unmodified unless
#'        argument \code{impute_locf} is activated.
#'     }
#'   }
#'
#' @section Notes: A live internet connection is required to access \var{SDA}.
#'
#' @seealso \code{\link[soilDB]{fetchSDA}} and \code{\link[soilDB]{SDA_query}}
#'
#' @examples
#' if (curl::has_internet()) {
#'   locations <- matrix(
#'     data = c(-120.325, -111.245, 39.855, 36.753),
#'     nrow = 2
#'   )
#'
#'   # Example 1: extract soils by mukey values
#'   extract_soils_NRCS_SDA(mukeys = c(471168, 1606800))
#'
#'   # Example 2: extract soils by geographic location
#'   extract_soils_NRCS_SDA(locations = locations)
#'
#'   # Example 3: first identify mukey values by geographic location,
#'   # then query soils from SSURGO by mukey,
#'   # but still pass locations in case we need to query STATSGO as well
#'
#'   mukeys <- fetch_mukeys_spatially_NRCS_SDA(
#'     locations,
#'     db = "SSURGO",
#'     progress_bar = TRUE
#'   )
#'
#'   extract_soils_NRCS_SDA(
#'     locations = locations,
#'     mukeys = mukeys[["mukeys"]],
#'     method = "SSURGO_then_STATSGO",
#'     remove_organic_horizons = "at_surface",
#'     replace_missing_fragvol_with_zero = "at_surface",
#'     estimate_missing_bulkdensity = TRUE,
#'     restrict_by_ec_or_ph = FALSE,
#'     impute_locf = TRUE,
#'     progress_bar = TRUE,
#'     verbose = TRUE
#'   )
#' }
#'
#' @export
extract_soils_NRCS_SDA <- function(
  locations,
  mukeys,
  method = c("SSURGO", "STATSGO", "SSURGO_then_STATSGO"),
  remove_organic_horizons = c("none", "all", "at_surface"),
  replace_missing_fragvol_with_zero = c("none", "all", "at_surface"),
  estimate_missing_bulkdensity = FALSE,
  restrict_by_ec_or_ph = TRUE,
  impute_locf = FALSE,
  digits = 3L,
  chunk_size = 1000L,
  progress_bar = FALSE,
  verbose = FALSE
) {

  stopifnot(
    !(missing(locations) && missing(mukeys)),
    curl::has_internet()
  )

  method <- match.arg(method)

  if (missing(locations) && method == "SSURGO_then_STATSGO") {
    warning(
      "'method' == \"SSURGO_then_STATSGO\" has no effect ",
      "if locations are missing"
    )
  }

  db <- if (method == "STATSGO") "STATSGO" else "SSURGO"

  locs_keys <- data.frame(
    source = db,
    mukey = if (missing(mukeys)) {
      fetch_mukeys_spatially_NRCS_SDA(
        locations = locations,
        db = db,
        progress_bar = progress_bar
      )[["mukeys"]]

    } else {
      mukeys
    },
    cokey = NA,
    compkind = NA,
    comppct_r = NA
  )

  stopifnot(!anyNA(locs_keys[["mukey"]]))


  # Download soil data from NRCS SDA web service
  res <- fetch_soils_from_NRCS_SDA(
    mukeys_unique = unique(locs_keys[["mukey"]]),
    majcompflag = switch(db, SSURGO = "subset", STATSGO = "ignore"),
    chunk_size = chunk_size,
    progress_bar = progress_bar
  )


  #--- Assign (dominant) cokey to point locations
  ids <- match(locs_keys[, "mukey"], res[, "MUKEY"], nomatch = 0)
  locs_keys[ids > 0, c("cokey", "compkind", "comppct_r")] <-
    res[ids, c("COKEY", "compkind", "comppct_r")]


  #--- Identify which variables are fixed per COKEY
  var_stxt <- c("sandtotal_r", "claytotal_r", "silttotal_r")
  var_output <- c(
    "dbovendry_r",
    "fragvol_r",
    var_stxt,
    "organic", "om_r"
  )

  tmp <- by(
    data = res,
    INDICES = res[["COKEY"]],
    FUN = function(x) {
      sapply(
        X = x,
        FUN = function(v) {
          if (is.numeric(v)) {
            var(v, na.rm = TRUE) > 0
          } else {
            nlevels(factor(v)) > 1
          }
        }
      )
    },
    simplify = FALSE
  )

  tmp <- matrix(unlist(tmp), ncol = ncol(res), byrow = TRUE)

  fx_per_cokey <- colnames(res)[!apply(tmp, 2, any, na.rm = TRUE)]

  fx_per_cokey <- union(fx_per_cokey, c("MUKEY", "COKEY"))
  fx_per_cokey <- setdiff(fx_per_cokey, c("hzdept_r", "hzdepb_r", var_output))


  #--- Deduce soil texture iff one of three values is missing
  res <- deduce_complete_soil_texture(
    x = res,
    var_stxt = var_stxt,
    val_total = 100,
    ignore_le = 5
  )


  #--- Interpret missing values for rock/gravel fragments as 0 %
  replace_missing_fragvol_with_zero <- match.arg(
    replace_missing_fragvol_with_zero
  )

  is_missing_fragvol <- !is.finite(res[, "fragvol_r"])

  if (any(is_missing_fragvol)) {
    if (replace_missing_fragvol_with_zero == "all") {
      res[is_missing_fragvol, "fragvol_r"] <- 0

    } else if (replace_missing_fragvol_with_zero == "at_surface") {
      is_missing_fragvol <- is_missing_fragvol & res[, "Horizon_No"] == 1
      res[is_missing_fragvol, "fragvol_r"] <- 0

    } else {
      res[is_missing_fragvol, "fragvol_r"] <- NA
    }
  }

  if (
    verbose &&
    replace_missing_fragvol_with_zero %in% c("all", "at_surface") &&
    (n_missing_fragvol <- sum(is_missing_fragvol)) > 0
  ) {
    message(
      "Missing values of rock/gravel fragments set to zero: n = ",
      n_missing_fragvol
    )
  }


  #--- Estimate soil bulk density if missing
  if (estimate_missing_bulkdensity) {
    is_missing_bd <- !is.finite(res[, "dbovendry_r"])

    if (any(is_missing_bd)) {
      res[is_missing_bd, "dbovendry_r"] <- estimate_bulkdensity(
        theta_saturated = res[is_missing_bd, "wsatiated_r"] / 100,
        gravel_volume = res[is_missing_bd, "fragvol_r"] / 100
      )

      if (verbose) {
        tmp <- is.finite(res[is_missing_bd, "dbovendry_r"])
        nm <- sum(!tmp)

        message(
          "Missing bulk density values estimated: n = ",
          sum(tmp),
          if (nm) paste0("; however, n = ", nm, " remain missing.")
        )
      }
    }
  }


  #--- Remove organic horizons
  res[, "organic"] <- is_NRCS_horizon_organic(res) %in% TRUE

  remove_organic_horizons <- match.arg(remove_organic_horizons)

  if (remove_organic_horizons %in% c("all", "at_surface")) {
    is_organic <- res[, "organic"]

    if (remove_organic_horizons == "all") {
      is_remove <- is_organic

      if (verbose && (n_remove <- sum(is_remove)) > 0) {
        message("Removed organic horizons: n = ", n_remove)
      }

    } else if (remove_organic_horizons == "at_surface") {
      #--- Organic horizon at the surface
      is_remove <- is_organic & res[, "Horizon_No"] == 1

      # Check whether more than one surface horizon is organic
      cokeys_w_osurf <- unique(res[is_remove, "COKEY"])
      ids_check <- which(res[["COKEY"]] %in% cokeys_w_osurf)

      if (length(ids_check) > 0) {
        tmp <- tapply(
          X = is_organic[ids_check],
          INDEX = res[ids_check, "COKEY"],
          FUN = function(x) {
            # First element corresponds to TRUE
            # because we only look at COKEYS with an organic surface horizon
            n <- rle(x)[["lengths"]][1]
            c(rep(TRUE, n), rep(FALSE, length(x) - n))
          },
          simplify = FALSE
        )

        is_remove[ids_check] <- unlist(tmp[match(cokeys_w_osurf, names(tmp))])
      }


      if (verbose && (n_remove <- sum(is_remove)) > 0) {
        n_oburied <- sum(is_organic & !is_remove)

        message(
          "Removed organic horizons at surface: n = ", n_remove,
          " for n = ", length(cokeys_w_osurf), " unique COKEYs",
          if (n_oburied > 0) {
            paste0("; n = ", n_oburied, " buried organic horizons remain.")
          } else "."
        )
      }
    }


    #--- Remove identified organic horizons

    if (any(is_remove)) {
      # Identify cokeys with horizons to be removed
      cokeys_affected <- unique(res[is_remove, "COKEY"])
      ids_affected <- which(res[["COKEY"]] %in% cokeys_affected)

      res_affected <- cbind(
        res[ids_affected, ],
        remove = is_remove[ids_affected]
      )

      # Recalculate horizon ranks and all depth values
      tmp <- by(
        data = res_affected,
        INDICES = res_affected[["COKEY"]],
        FUN = function(x) {
          ids_keep <- !x[["remove"]]

          xnew <- x[ids_keep, , drop = FALSE]
          n <- nrow(xnew)

          if (n > 0) {
            tmp <- rep(0, length(x[["remove"]]))
            tmp[x[["remove"]]] <-
              x[x[["remove"]], "hzdepb_r"] - x[x[["remove"]], "hzdept_r"]
            removed_total <- sum(tmp)
            removed_widths <- cumsum(tmp)[ids_keep]

            # Re-calculate horizon rank
            xnew[, "Horizon_No"] <- seq_len(n)

            # Re-calculate upper/lower horizon depth limits
            ids <- grep("hzdep", colnames(xnew))
            xnew[, ids] <- xnew[, ids] - removed_widths

            # Re-calculate depth restrictions
            ids <- grep("_depth", colnames(xnew))
            xnew[, ids] <- xnew[, ids] - removed_total

          } else {
            # We need at least one entry per COKEY
            # Copy values of first row of those columns that are fixed per
            # COKEY and set others to NA
            xnew <- x[1, , drop = FALSE]
            xnew[, setdiff(colnames(xnew), fx_per_cokey)] <- NA
          }

          xnew
        },
        simplify = FALSE
      )

      tmp <- do.call(rbind, tmp)

      # Put data back together
      res <- rbind(
        res[-ids_affected, ],
        tmp[, !grepl("remove", colnames(tmp))]
      )
    }
  }

  res[, "organic"] <- as.integer(res[, "organic"])


  #--- Calculate soil depth of profile (per COKEY)
  # and convert depth table to wide-format for output
  locs_table_depths <- calculate_NRCS_soil_depth(
    x = res,
    target_cokeys = locs_keys[, "cokey"],
    restrict_by_ec_or_ph = restrict_by_ec_or_ph
  )

  # Transfer final soil depth and (potentially adjusted depth_L1)
  ids <- match(res[["COKEY"]], rownames(locs_table_depths))
  res[, "SoilDepth_cm"] <- locs_table_depths[ids, "SoilDepth_cm"]
  res[, "N_horizons"] <- locs_table_depths[ids, "N_horizons"]

  is_shallowest <- res[, "Horizon_No"] == 1
  res[is_shallowest, "hzdepb_r"] <-
    locs_table_depths[ids[is_shallowest], "depth_L1"]


  #--- Last step: impute remaining missing values per COKEY
  # by shallow-depth value carried deeper (in analogy to LOCF)
  # but do not impute missing values in the shallowest horizon
  if (impute_locf) {

    if (verbose) {
      is_shallowest <- res[, "Horizon_No"] == 1
      n_imped_vals <- sum(is.na(res[!is_shallowest, var_output]))
      is_imped_hzs <- apply(res[!is_shallowest, var_output], 1, anyNA)
      n_imped_hzs <- sum(is_imped_hzs)
      n_imped_cokeys <- length(
        unique(res[!is_shallowest, "COKEY"][is_imped_hzs])
      )
    }

    tmp <- by(
      data = res[, var_output],
      INDICES = res[["COKEY"]],
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

    ids <- match(unique(res[["COKEY"]]), names(tmp))
    res[, var_output] <- do.call(rbind, tmp[ids])

    if (verbose) {
      n_missing <- sum(is.na(res[, var_output]))

      message(
        "Imputed values for n = ", n_imped_cokeys, " COKEYs ",
        "in n = ", n_imped_hzs, " soil horizons/layers ",
        "for n = ", n_imped_vals, " values",
        if (n_missing > 0) {
          paste0("; remaining missing values n = ", n_missing)
        } else "."
      )
    }
  }


  #--- Convert units & rounding
  # Convert % to fraction
  var_pct_to_fraction <- c("fragvol_r", var_stxt)
  res[, var_pct_to_fraction] <- res[, var_pct_to_fraction] / 100

  # Round texture
  if (is.finite(digits)) {
    res[, "fragvol_r"] <- round(res[, "fragvol_r"], digits)

    has_vals <-
      complete.cases(res[, var_stxt]) &
      apply(res[, var_stxt, drop = FALSE], 1, sum, na.rm = TRUE) > 0

    res[has_vals, var_stxt] <- rSW2utils::scale_rounded_by_sum(
      x = res[has_vals, var_stxt],
      digits = digits,
      icolumn_adjust = 3
    )
  }


  #--- Create texture table
  # Convert to wide format (one row for each point location)
  tmp_texture <- reshape2::acast(
    data = reshape2::melt(
      data = cbind(
        res[, c("Horizon_No", "COKEY", var_output)]
      ),
      id.vars = c("Horizon_No", "COKEY")
    ),
    formula = COKEY ~ Horizon_No + variable
  )

  ids <- match(locs_keys[, "cokey"], rownames(tmp_texture), nomatch = NA)
  locs_table_texture <- tmp_texture[ids, ]

  colnames(locs_table_texture) <- sapply(
    X = strsplit(colnames(locs_table_texture), split = "_"),
    FUN = function(x) paste0(x[2], "_L", x[1])
  )



  #--- Attempt to replace nosoil-sites with STATSGO
  ids_h0 <- which(locs_table_depths[, "N_horizons"] == 0)
  if (
    length(ids_h0) > 0 &&
    !missing(locations) &&
    method == "SSURGO_then_STATSGO"
  ) {

    # Call again for nosoil rows and extract from STATSGO instead of SSURGO
    res0 <- Recall(
      locations = locations[ids_h0, ],
      method = "STATSGO",
      remove_organic_horizons = remove_organic_horizons,
      replace_missing_fragvol_with_zero = replace_missing_fragvol_with_zero,
      estimate_missing_bulkdensity = estimate_missing_bulkdensity,
      restrict_by_ec_or_ph = restrict_by_ec_or_ph,
      impute_locf = impute_locf,
      digits = digits,
      chunk_size = chunk_size,
      progress_bar = progress_bar,
      verbose = verbose
    )

    # Check that we have no longer nosoils
    is_res0_good <-
      !is.na(res0[["table_depths"]][, "N_horizons"]) &
      res0[["table_depths"]][, "N_horizons"] > 0
    ids_h0_replace <- ids_h0[is_res0_good]

    # Update nosoil data with soils from STATSGO
    if (length(ids_h0_replace) > 0) {
      locs_keys[ids_h0_replace, ] <- res0[["table_keys"]][is_res0_good, ]

      ivars <- seq_len(
        min(ncol(locs_table_depths), ncol(res0[["table_depths"]]))
      )
      locs_table_depths[ids_h0_replace, ivars] <-
        res0[["table_depths"]][is_res0_good, ivars]

      ivars <- seq_len(
        min(ncol(locs_table_texture), ncol(res0[["table_texture"]]))
      )
      locs_table_texture[ids_h0_replace, ivars] <-
        res0[["table_texture"]][is_res0_good, ivars]
    }
  }


  #--- Return tables
  list(
    ref = create_reference_for_NRCS_SDA(),
    table_keys = locs_keys,
    table_depths = locs_table_depths,
    table_texture = locs_table_texture
  )
}
