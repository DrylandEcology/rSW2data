
test_that("check_depth_table", {
  soils <- matrix(
    data = c(
      5, 10, 50, 150,
      5, 10, 50, 200,
      5, 10, 50, 150,
      5, NA, 50, 150,
      5, 5, 50, 150,
      NA, NA, NA, NA
    ),
    nrow = 6,
    byrow = TRUE
  )

  sd_cm <- c(150, 150, 150, 150, 150, 0)
  nlyrs <- c(4, 4, 3, 3, 4, 0)

  # This illustrates the warnings
  expected_names <- c(
    "soil_depth", "n_layers", "ids_sites_without_soils", "sl_1region",
    "sl_monotonic"
  )

  soil_checks1 <- suppressWarnings(check_depth_table(
    table_depths = soils,
    soil_depth = sd_cm,
    n_layers = nlyrs
  ))

  expect_named(soil_checks1, expected_names)


  # This should be ok
  soil_checks2 <- check_depth_table(
    table_depths = soils[1, , drop = FALSE],
    soil_depth = sd_cm[[1]],
    n_layers = nlyrs[[1]]
  )

  expect_true(soil_checks2)
})


test_that("check_texture_table", {
  soils <- data.matrix(data.frame(
    sand_L1 = c(0.828, 0.963, NA),
    clay_L1 = c(0.065, 0.03, 0.03),
    sand_L2 = c(0.57, NA, NA),
    clay_L2 = c(0.25, 0.03, 0.03),
    sand_L3 = c(0, NA, NA),
    clay_L3 = c(0, NA, NA)
  ))
  N_horizons <- rep(3, 3)

  texture_checks <- check_texture_table(
    table_texture = soils,
    n_layers = N_horizons,
    vars = c("sand", "clay"),
    vars_notzero = c("sand", "clay")
  )

  # Does our soils table have any issues?
  expect_false(texture_checks[["checks_passed"]])

  # Types of issues
  expected_issuetypes <- c("checks_passed", "missing", "zero")
  expect_named(texture_checks, expected_issuetypes)

  # Names of issues
  expected_issuenames <- c(
    "cond_N", "is_cond_anylayer", "is_cond_pctlayer",
    "ids_sites_cond_anylayer", "ids_sites_cond_alllayers",
    "ids_sites_cond_somelayers"
  )

  expect_named(texture_checks[["missing"]], expected_issuenames)
  expect_named(texture_checks[["zero"]], expected_issuenames)
})
