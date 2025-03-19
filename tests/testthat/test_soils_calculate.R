
test_that("deduce_complete_soil_texture", {
  x <- data.frame(
    sand_pct = c(45.7, NA, 68.5, NA, 0, 2, NA),
    clay_pct = c(12.5, NA, NA, NA, 0, NA, 10),
    silt_pct = c(41.8, NA, 21.5, 15, NA, 1, 15)
  )

  expect_identical(which(complete.cases(x)), 1L)


  x2 <- deduce_complete_soil_texture(
    x = x,
    var_stxt = c("sand_pct", "clay_pct", "silt_pct"),
    val_total = 100,
    ignore_le = 5
  )
  # row 3: previously missing clay was deduced to 10%
  # row 5: missing silt was not filled in because others sum to <= 5
  # row 6: missing clay was not filled in because others sum to <= 5
  # row 7: previously missing sand was deduced to 75%

  expect_identical(which(complete.cases(x2)), c(1L, 3L, 7L))


  x3 <- deduce_complete_soil_texture(
    x = x,
    var_stxt = c("sand_pct", "clay_pct", "silt_pct"),
    val_total = 100,
    ignore_le = 0
  )

  expect_identical(which(complete.cases(x3)), c(1L, 3L, 6:7))
})


test_that("set_missing_soils_to_value", {
  x <- data.frame(
    coarse = c(NaN, 10, 50, Inf, NA, 15, NA, 20),
    sand_pct = c(45.7, NA, 68.5, NA, 0, 2, NA, NA)
  )

  cns <- colnames(x)
  var <- "coarse"
  cns_wo_var <- !grepl(var, cns)

  is_missing <- !is.finite(x[, var])

  res <- set_missing_soils_to_value(x, var, where = "none")
  expect_identical(is.na(res[, var]), is_missing)
  expect_identical(res[, !grepl(var, colnames(res))], x[, cns_wo_var])

  res <- set_missing_soils_to_value(x, var, where = "all")
  expect_true(all(is.finite(res[, var])))
  expect_identical(res[, !grepl(var, colnames(res))], x[, cns_wo_var])



  for (k in 1:2) {
    horizons <- if (k == 1) seq_len(nrow(x)) else rep(seq_len(nrow(x) / 2), 2)
    is_shallowest <- horizons == 1
    is_missing_but_shallow <- is_missing
    is_missing_but_shallow[is_shallowest] <- FALSE

    res <- set_missing_soils_to_value(
      x = cbind(Layer_ID = horizons, x),
      variable = var,
      where = "at_surface",
      horizon = "Layer_ID"
    )
    expect_true(all(is.finite(res[is_shallowest, var])))
    expect_identical(!is.finite(res[, var]), is_missing_but_shallow)
    expect_identical(res[, !grepl(var, colnames(res))][, -1], x[, cns_wo_var])


    res <- set_missing_soils_to_value(
      x,
      variable = var,
      where = "at_surface",
      horizon = horizons
    )
    expect_true(all(is.finite(res[is_shallowest, var])))
    expect_identical(!is.finite(res[, var]), is_missing_but_shallow)
    expect_identical(res[, !grepl(var, colnames(res))], x[, cns_wo_var])

    expect_message(set_missing_soils_to_value(
      x,
      variable = var,
      where = "at_surface",
      horizon = horizons,
      verbose = TRUE
    ))
  }
})


test_that("impute_soils", {
  # --- Example: sorted by site and by soil layer
  tmp <- data.frame(
    layer_no = 1:7,
    coarse = c(NA, 10, 50, NA, 0, 15, NA),
    sand_pct = c(45.7, NA, 68.5, NA, 0, 2, NA),
    clay_pct = c(12.5, NA, NA, NA, 0, NA, 10),
    silt_pct = c(41.8, NA, 21.5, 15, NA, 1, 15)
  )

  x1 <- rbind(
    cbind(id = rep(1, 7), tmp),
    cbind(id = rep(2, 7), tmp)
  )

  # Change id 2 values a bit
  ir <- x1[["id"]] == 2L
  ic <- c("coarse", "sand_pct", "clay_pct", "silt_pct")
  x1[ir, ic] <- x1[ir, ic] + 5

  res1 <- suppressWarnings(impute_soils(
    x1,
    var_values = c("coarse", "sand_pct"),
    var_site_id = "id",
    var_horizon = "layer_no"
  ))

  # NAs in shallowest layer are not imputed even if requested
  expect_identical(
    which(is.na(res1[, "coarse"])),
    which(x1[, "layer_no"] == 1)
  )
  # NAs of requested variables replaced with values in deeper layers
  expect_false(anyNA(res1[, "sand_pct"]))

  # Values of unrequested variables are not changed
  expect_identical(
    x1[, c("clay_pct", "silt_pct")],
    res1[, c("clay_pct", "silt_pct")]
  )

  # Order of data by site and layer is not changed
  expect_identical(res1[, c("id", "layer_no")], x1[, c("id", "layer_no")])

  # verbose argument produces messages
  expect_message(suppressWarnings(impute_soils(
    x1,
    var_values = c("coarse", "sand_pct"),
    var_site_id = "id",
    var_horizon = "layer_no",
    verbose = TRUE
  )))


  # --- Example: sorted by soil layer and by site
  x2 <- x1[order(x1[, "layer_no"], x1[, "id"]), ]

  res2 <- suppressWarnings(impute_soils(
    x2,
    var_values = c("coarse", "sand_pct"),
    var_site_id = "id",
    var_horizon = "layer_no"
  ))

  # Order of data by site and layer is not changed
  expect_identical(res2[, c("id", "layer_no")], x2[, c("id", "layer_no")])
  # Imputation is independent of order of data
  expect_identical(
    res2[order(res2[, "id"], res2[, "layer_no"]), ],
    res1
  )


  # --- Example: unsorted rows
  set.seed(444)
  x3 <- x1[sample(nrow(x1)), ]

  res3 <- suppressWarnings(impute_soils(
    x3,
    var_values = c("coarse", "sand_pct"),
    var_site_id = "id",
    var_horizon = "layer_no"
  ))

  # Order of data by site and layer is not changed
  expect_identical(res3[, c("id", "layer_no")], x3[, c("id", "layer_no")])
  # Imputation is independent of order of data
  expect_identical(
    res3[order(res3[, "id"], res3[, "layer_no"]), ],
    res1
  )
})


test_that("estimate_bulkdensity", {
  tol <- sqrt(.Machine[["double.eps"]])

  for (theta_sat in seq(0, 1, by = 0.1)) {
    for (fragvol in seq(0, 1, by = 0.1)) {
      x <- estimate_bulkdensity(
        theta_saturated = theta_sat,
        gravel_volume = fragvol
      )

      expect_gt(x, 0 - tol)
      expect_lt(x, 2.65 + tol)
    }
  }
})



test_that("Bare-soil evaporation coefficients", {
  check_bsevap_coeffs <- function(bsevap_coeff, ld, md, Ns, Nl, info = NULL) {
    # Coeffs of each site sum to one
    expect_identical(rowSums(bsevap_coeff), rep(1, Ns), info = info)

    # Coeffs are between 0 and 1
    expect_identical(
      as.vector(bsevap_coeff <= 1), rep(TRUE, Ns * Nl),
      info = info
    )

    expect_identical(
      as.vector(bsevap_coeff >= 0), rep(TRUE, Ns * Nl),
      info = info
    )

    # If max is shallower than first layer, then first layer is 1
    if (ld[[1]] >= md) {
      expect_identical(bsevap_coeff[, 1], rep(1, Ns))
    }

    # Monotonic decrease with soil depth
    tmpn <- min(sum(!is.na(ld)), ncol(bsevap_coeff))
    if (tmpn > 1) {
      tmpids <- seq_len(tmpn)
      tmp <- sweep(
        bsevap_coeff[, tmpids, drop = FALSE],
        MARGIN = 2,
        STATS = ld[tmpids],
        FUN = "/"
      )
      deltas <- apply(tmp, 1, diff)
      deltas <- if (is.null(dim(deltas))) {
        matrix(deltas, ncol = 1)
      } else {
        t(deltas)
      }
      expect_identical(
        deltas <= 0,
        matrix(TRUE, nrow = Ns, ncol = tmpn - 1)
      )
    }

    # No bare-soil evaporation from depths greater than
    # 'depth_max_bs_evap_cm'
    lmax <- max(1, min(Nl, findInterval(md, c(0, na.exclude(ld)))))
    expect_identical(
      apply(
        X = bsevap_coeff,
        MARGIN = 1,
        FUN = function(x) sum(x > 0)
      ) <= rep(lmax, Ns),
      rep(TRUE, Ns),
      info = info
    )

    invisible(TRUE)
  }


  #--- INPUTS ------
  N_sites <- 3
  ids_sets <- list(c(1, 3), 2)

  tldv1 <- c(5, 10, 15, 30)
  tldv2 <- c(5, 15, 20, 50, 100)
  tldvs <- list(tldv1, tldv2)
  tmd <- tldv1[[3]] # depth_max_bs_evap_cm

  tldm <- matrix(NA, nrow = N_sites, ncol = max(lengths(tldvs)))
  for (k1 in seq_along(ids_sets)) {
    tldm[ids_sets[[k1]], seq_along(tldvs[[k1]])] <- matrix(
      tldvs[[k1]],
      nrow = length(ids_sets[[k1]]),
      ncol = length(tldvs[[k1]]),
      byrow = TRUE
    )
  }

  tspm <- matrix(0.5, nrow = N_sites, ncol = 5)
  tcpm <- matrix(0.2, nrow = N_sites, ncol = 5)

  ids_bad_sites <- N_sites
  tspmna <- tspm
  tspmna[ids_bad_sites, 1] <- NA


  #--- Expect errors ------

  #--- * Error if md is non-finite ------
  expect_error(
    calc_BareSoilEvapCoefs(tldv1, tspm, tcpm, depth_max_bs_evap_cm = "bad")
  )

  #--- * Error if md is negative ------
  expect_error(
    calc_BareSoilEvapCoefs(tldv1, tspm, tcpm, depth_max_bs_evap_cm = -5)
  )

  #--- * Error if missing values in `layers_depth` within md ------
  expect_error(
    calc_BareSoilEvapCoefs(c(5, NA, 15, 30), tspm, tcpm, tmd)
  )

  # but no error if all missing values pooled at depth
  expect_silent(
    calc_BareSoilEvapCoefs(c(5, 10, NA, NA), tspm, tcpm, tmd)
  )

  #--- * Error if sand and clay have different dimensions ------
  expect_error(
    calc_BareSoilEvapCoefs(tldv1, tspm, tcpm[, seq_len(ncol(tcpm) - 1)], tmd)
  )

  #--- * Error if sand or clay has different number of sites than layers ------
  expect_error(
    calc_BareSoilEvapCoefs(matrix(tldv1, nrow = 1), tspm, tcpm, tmd)
  )

  #--- * Error if sand and clay have fewer layers than needed to meet md ------
  expect_error(
    calc_BareSoilEvapCoefs(tldv1, tspm[, 1:2], tcpm[, 1:2], tmd)
  )

  #--- * Error if some sites have missing values and stop on bad sites ------
  expect_error(
    calc_BareSoilEvapCoefs(tldv1, tspmna, tcpm, tmd, method_bad_soils = "stop")
  )



  #--- Expect output ------

  #--- * No missing values in `layers_depth`; deeper than md ------
  check_bsevap_coeffs(
    calc_BareSoilEvapCoefs(tldv1, tspm, tcpm, tmd),
    ld = tldv1,
    md = tmd,
    Ns = nrow(tspm),
    Nl = ncol(tspm)
  )

  bsevap_coeff <- calc_BareSoilEvapCoefs(tldm, tspm, tcpm, tmd)

  for (k1 in seq_along(ids_sets)) {
    check_bsevap_coeffs(
      bsevap_coeff[ids_sets[[k1]], , drop = FALSE],
      ld = tldm[ids_sets[[k1]][[1]], seq_len(length(tldvs[[k1]]))],
      md = tmd,
      Ns = length(ids_sets[[k1]]),
      Nl = ncol(tspm)
    )
  }


  #--- * No missing values in `layers_depth`; shallower than md ------
  check_bsevap_coeffs(
    calc_BareSoilEvapCoefs(
      tldv1, tspm, tcpm,
      depth_max_bs_evap_cm = max(tldv1)
    ),
    ld = tldv1,
    md = max(tldv1),
    Ns = nrow(tspm),
    Nl = ncol(tspm)
  )

  bsevap_coeff <- calc_BareSoilEvapCoefs(
    tldm, tspm, tcpm,
    depth_max_bs_evap_cm = 30
  )

  for (k1 in seq_along(ids_sets)) {
    check_bsevap_coeffs(
      bsevap_coeff[ids_sets[[k1]], , drop = FALSE],
      ld = tldm[ids_sets[[k1]][[1]], seq_len(length(tldvs[[k1]]))],
      md = 30,
      Ns = length(ids_sets[[k1]]),
      Nl = ncol(tspm)
    )
  }


  #--- * Missing values in `layers_depth` but deeper than md ------
  check_bsevap_coeffs(
    calc_BareSoilEvapCoefs(c(tldv1, NA), tspm, tcpm, tmd),
    ld = tldv1,
    md = tmd,
    Ns = nrow(tspm),
    Nl = ncol(tspm)
  )

  bsevap_coeff <- calc_BareSoilEvapCoefs(cbind(tldm, NA), tspm, tcpm, tmd)

  for (k1 in seq_along(ids_sets)) {
    check_bsevap_coeffs(
      bsevap_coeff[ids_sets[[k1]], , drop = FALSE],
      ld = tldm[ids_sets[[k1]][[1]], seq_len(length(tldvs[[k1]]))],
      md = tmd,
      Ns = length(ids_sets[[k1]]),
      Nl = ncol(tspm)
    )
  }


  #--- * Arguments sand and clay are site x layer matrices ------
  check_bsevap_coeffs(
    calc_BareSoilEvapCoefs(tldv1, tspm, tcpm, tmd),
    ld = tldv1,
    md = tmd,
    Ns = nrow(tspm),
    Nl = ncol(tspm)
  )

  bsevap_coeff <- calc_BareSoilEvapCoefs(tldm, tspm, tcpm, tmd)

  for (k1 in seq_along(ids_sets)) {
    check_bsevap_coeffs(
      bsevap_coeff[ids_sets[[k1]], , drop = FALSE],
      ld = tldm[ids_sets[[k1]][[1]], seq_len(length(tldvs[[k1]]))],
      md = tmd,
      Ns = length(ids_sets[[k1]]),
      Nl = ncol(tspm)
    )
  }


  #--- * Arguments sand and clay are vectors of layers ------
  check_bsevap_coeffs(
    calc_BareSoilEvapCoefs(tldv1, tspm[1, ], tcpm[1, ], tmd),
    ld = tldv1,
    md = tmd,
    Ns = 1,
    Nl = ncol(tspm)
  )


  #--- * Arguments sand and clay are site x 1 layer matrices ------
  check_bsevap_coeffs(
    calc_BareSoilEvapCoefs(
      tldv1,
      tspm[, 1, drop = FALSE],
      tcpm[, 1, drop = FALSE],
      tldv1[[1]]
    ),
    ld = tldv1,
    md = tldv1[[1]],
    Ns = nrow(tspm),
    Nl = 1
  )

  bsevap_coeff <- calc_BareSoilEvapCoefs(
    tldm,
    tspm[, 1, drop = FALSE],
    tcpm[, 1, drop = FALSE],
    min(tldm[1, ], na.rm = TRUE)
  )

  for (k1 in seq_along(ids_sets)) {
    check_bsevap_coeffs(
      bsevap_coeff[ids_sets[[k1]], , drop = FALSE],
      ld = tldm[ids_sets[[k1]][[1]], seq_len(length(tldvs[[k1]]))],
      md = min(tldm[1, ], na.rm = TRUE),
      Ns = length(ids_sets[[k1]]),
      Nl = 1
    )
  }


  #--- * Argument layer has only one layer ------
  check_bsevap_coeffs(
    calc_BareSoilEvapCoefs(tldv1[[1]], tspm, tcpm, tldv1[[1]]),
    ld = tldv1[[1]],
    md = tldv1[[1]],
    Ns = nrow(tspm),
    Nl = ncol(tspm)
  )

  bsevap_coeff <- calc_BareSoilEvapCoefs(
    tldm[, 1, drop = FALSE],
    tspm, tcpm,
    min(tldm[, 1])
  )

  for (k1 in seq_along(ids_sets)) {
    check_bsevap_coeffs(
      bsevap_coeff[ids_sets[[k1]], , drop = FALSE],
      ld = tldm[ids_sets[[k1]][[1]], 1, drop = FALSE],
      md = min(tldm[, 1]),
      Ns = length(ids_sets[[k1]]),
      Nl = ncol(tspm)
    )
  }


  #--- * Some sites have missing values but pass bad sites through ------
  bsevap_coeff <- calc_BareSoilEvapCoefs(
    tldv1, tspmna, tcpm, tmd,
    method_bad_soils = "pass"
  )

  expect_true(all(is.na(bsevap_coeff[ids_bad_sites, ])))

  check_bsevap_coeffs(
    bsevap_coeff[-ids_bad_sites, , drop = FALSE],
    ld = tldv1,
    md = tmd,
    Ns = nrow(tspmna) - length(ids_bad_sites),
    Nl = ncol(tspmna)
  )


  bsevap_coeff <- calc_BareSoilEvapCoefs(
    tldm, tspmna, tcpm, tmd,
    method_bad_soils = "pass"
  )

  expect_true(all(is.na(bsevap_coeff[ids_bad_sites, ])))

  for (k1 in seq_along(ids_sets)) {
    ids <- setdiff(ids_sets[[k1]], ids_bad_sites)
    if (length(ids) > 0) {
      check_bsevap_coeffs(
        bsevap_coeff[ids, , drop = FALSE],
        ld = tldm[ids[[1]], seq_len(length(tldvs[[k1]]))],
        md = tmd,
        Ns = length(ids),
        Nl = ncol(tspm)
      )
    }
  }


  #--- * One site and it has missing values but pass bad sites through ------
  bsevap_coeff <- calc_BareSoilEvapCoefs(
    tldv1,
    tspmna[ids_bad_sites, ],
    tcpm[ids_bad_sites, ],
    tmd,
    method_bad_soils = "pass"
  )

  expect_true(all(is.na(bsevap_coeff)))


  bsevap_coeff <- calc_BareSoilEvapCoefs(
    tldm[ids_bad_sites, , drop = FALSE],
    tspmna[ids_bad_sites, ],
    tcpm[ids_bad_sites, ],
    tmd,
    method_bad_soils = "pass"
  )

  expect_true(all(is.na(bsevap_coeff)))


  #--- * md is shallower than the shallowest layer ------
  check_bsevap_coeffs(
    calc_BareSoilEvapCoefs(tldv1, tspm, tcpm, tldv1[[1]] - 1),
    ld = tldv1[[1]] - 1,
    md = tmd,
    Ns = nrow(tspm),
    Nl = ncol(tspm)
  )


  bsevap_coeff <- calc_BareSoilEvapCoefs(
    tldm, tspm, tcpm,
    min(tldm, na.rm = TRUE) - 1
  )

  for (k1 in seq_along(ids_sets)) {
    check_bsevap_coeffs(
      bsevap_coeff[ids_sets[[k1]], , drop = FALSE],
      ld = tldm[ids_sets[[k1]][[1]], seq_len(length(tldvs[[k1]]))],
      md = min(tldm, na.rm = TRUE) - 1,
      Ns = length(ids_sets[[k1]]),
      Nl = ncol(tspm)
    )
  }


  #--- * md is deeper than the deepest layer that is available ------
  mdd <- max(tldv1[1:2]) + 1

  bsevap_coeff <- calc_BareSoilEvapCoefs(
    c(tldv1[1:2], NA), tspm, tcpm,
    depth_max_bs_evap_cm = mdd,
    method_bad_soils = "pass"
  )

  check_bsevap_coeffs(
    bsevap_coeff,
    ld = c(tldv1[1:2], NA),
    md = mdd,
    Ns = nrow(tspm),
    Nl = ncol(tspm)
  )

  mdd <- max(tldm, na.rm = TRUE) + 1
  bsevap_coeff <- calc_BareSoilEvapCoefs(
    tldm, tspm, tcpm,
    depth_max_bs_evap_cm = mdd
  )

  for (k1 in seq_along(ids_sets)) {
    check_bsevap_coeffs(
      bsevap_coeff[ids_sets[[k1]], , drop = FALSE],
      ld = tldm[ids_sets[[k1]][[1]], seq_len(length(tldvs[[k1]]))],
      md = mdd,
      Ns = length(ids_sets[[k1]]),
      Nl = ncol(tspm)
    )
  }

})
