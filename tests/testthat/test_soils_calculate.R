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
