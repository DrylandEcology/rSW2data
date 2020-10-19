context("Soil calculations")


test_that("deduce_complete_soil_texture", {
  x <- data.frame(
    sand_pct = c(45.7, NA, 68.5, NA, 0, 2, NA),
    clay_pct = c(12.5, NA, NA, NA, 0, NA, 10),
    silt_pct = c(41.8, NA, 21.5, 15, NA, 1, 15)
  )

  expect_equal(which(complete.cases(x)), 1)


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

  expect_equal(which(complete.cases(x2)), c(1, 3, 7))


  x3 <- deduce_complete_soil_texture(
    x = x,
    var_stxt = c("sand_pct", "clay_pct", "silt_pct"),
    val_total = 100,
    ignore_le = 0
  )

  expect_equal(which(complete.cases(x3)), c(1, 3, 6:7))
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
  expect_equal(is.na(res[, var]), is_missing)
  expect_equal(res[, !grepl(var, colnames(res))], x[, cns_wo_var])

  res <- set_missing_soils_to_value(x, var, where = "all")
  expect_true(all(is.finite(res[, var])))
  expect_equal(res[, !grepl(var, colnames(res))], x[, cns_wo_var])



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
    expect_equal(!is.finite(res[, var]), is_missing_but_shallow)
    expect_equal(res[, !grepl(var, colnames(res))][, -1], x[, cns_wo_var])


    res <- set_missing_soils_to_value(
      x,
      variable = var,
      where = "at_surface",
      horizon = horizons
    )
    expect_true(all(is.finite(res[is_shallowest, var])))
    expect_equal(!is.finite(res[, var]), is_missing_but_shallow)
    expect_equal(res[, !grepl(var, colnames(res))], x[, cns_wo_var])

    expect_message(set_missing_soils_to_value(
      x,
      variable = var,
      where = "at_surface",
      horizon = horizons,
      verbose = TRUE
    ))
  }
})


test_that("estimate_bulkdensity", {
  tol <- sqrt(.Machine$double.eps)

  for (theta_sat in seq(0, 1, by = 0.1)) for (fragvol in seq(0, 1, by = 0.1)) {
    x <- estimate_bulkdensity(
      theta_saturated = theta_sat,
      gravel_volume = fragvol
    )

    expect_gt(x, 0 - tol)
    expect_lt(x, 2.65 + tol)
  }
})



test_that("Bare-soil evaporation coefficients", {
  #--- INPUTS
  print_debug <- FALSE
  vars <- c("ld", "sp", "cp", "md")
  get_siteN <- function(x) if (is.null(dim(x))) 1L else dim(x)[1]
  get_layerN <- function(x) if (is.null(dim(x))) length(x) else dim(x)[2]

  layers_depth <- list(
    5,
    c(5, 10, 15, 30),
    c(5, 10, 30, 50),
    c(15, 50),
    50,
    200,
    c(5, NA, 30, 50),
    c(0, 5, 30, 50),
    c(-5, 5, 30),
    c(1.5, 10, 30))

  sites_Nmax <- 5
  lyrs_N <- sapply(layers_depth, get_layerN)
  lyrs_Nmax <- max(lyrs_N)

  depth_max_bs_evap_cm <- c(-5, 0, 1.5, 5, 15, 200, NA)

  sand <- list(
    rep(NA, lyrs_Nmax),
    rep(0, lyrs_Nmax),
    rep(0, lyrs_Nmax - 1),
    rep(1, lyrs_Nmax),
    rep(0.75, lyrs_Nmax),
    rep(0.1, lyrs_Nmax),
    matrix(0.5, nrow = sites_Nmax, ncol = lyrs_Nmax))

  clay <- list(
    rep(NA, lyrs_Nmax),
    rep(0, lyrs_Nmax),
    rep(0, lyrs_Nmax - 1),
    rep(1, lyrs_Nmax),
    rep(0.75, lyrs_Nmax),
    rep(0.1, lyrs_Nmax),
    matrix(0.2, nrow = sites_Nmax, ncol = lyrs_Nmax))

  #--- TESTS
  k <- 1

  for (k1 in seq_along(layers_depth)) {
    for (k2 in seq_along(sand)) {
      for (k3 in seq_along(depth_max_bs_evap_cm)) {

        ld <- layers_depth[[k1]]
        sp <- sand[[k2]][min(get_siteN(sand[[k2]]), seq_len(lyrs_N[k1]))]
        cp <- clay[[k2]][min(get_siteN(clay[[k2]]), seq_len(lyrs_N[k1]))]
        md <- depth_max_bs_evap_cm[k3]
        Ns <- get_siteN(sp)
        Nl <- get_layerN(sp)

        info <- paste0(
          "Test #", k, ": ", k1, k2, k3, ": input = ",
          paste(
            lapply(
              vars,
              function(x) {
                tmp <- get(x)
                paste(x, "=", paste(tmp, collapse = "-"))
              }
            ),
            collapse = " / "
          )
        )

        if (
          anyNA(ld) || anyNA(sp) || anyNA(cp) || anyNA(md) ||
          Nl < length(ld) || Ns != get_siteN(cp) || Nl != get_layerN(cp) ||
          any(md < 0) || any(ld <= 0) || any(sp < 0) || any(cp < 0) ||
          any(sp > 1) || any(cp > 1) || any(sp + cp > 1)
        ) {

          if (print_debug) {
            print(paste0(k1, k2, k3, ": ", info, ": expect error"))
          }

          expect_error(
            calc_BareSoilEvapCoefs(ld, sp, cp, md),
            info = info
          )

        } else {
          bsevap_coeff <- calc_BareSoilEvapCoefs(ld, sp, cp, md)

          # Coeffs of each site sum to one
          expect_equal(apply(bsevap_coeff, 1, sum), rep(1, Ns), info = info)

          # Coeffs are between 0 and 1
          expect_equal(
            as.vector(bsevap_coeff <= 1), rep(TRUE, Ns * Nl),
            info = info
          )

          expect_equal(
            as.vector(bsevap_coeff >= 0), rep(TRUE, Ns * Nl),
            info = info
          )

          # If max is shallower than first layer, then first layer is 1
          if (ld[1] >= md) {
            expect_equal(bsevap_coeff[, 1], rep(1, Ns))
          }

          # Monotonic decrease with soil depth
          if (Ns * Nl > 1) {
            temp <- sweep(bsevap_coeff, 2, ld, FUN = "/")
            deltas <- as.vector(apply(temp, 1, diff))
            expect_equal(deltas <= 0, rep(TRUE, Ns * Nl - 1L))
          }

          # No bare-soil evaporation from depths greater than
          # 'depth_max_bs_evap_cm'
          lmax <- max(1, min(Nl, findInterval(md, c(0, ld))))
          expect_equal(
            apply(
              X = bsevap_coeff,
              MARGIN = 1,
              FUN = function(x) sum(x > 0)
            ) <= rep(lmax, Ns),
            rep(TRUE, Ns),
            info = info
          )

          if (print_debug) {
            print(paste0(
              k1, k2, k3, ": ", info,
              ": bsevap = ", paste(bsevap_coeff, collapse = ":")
            ))
          }
        }

        k <- k + 1
      }
    }
  }

})
