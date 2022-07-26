
test_that("add_soil_layer", {
  soil_layers <- c(8, 20, 150)
  N <- length(soil_layers)

  xsand <- t(data.frame(
    site1 = c(0.5, 0.55, NA),
    site2 = c(0.3, 0.35, 0.4)
  ))

  xtrco <- t(data.frame(
    site1 = c(0.9, 0.1, NA),
    site2 = c(0.7, 0.1, 0.2)
  ))

  test_depths <- c(
    # Add new shallow 0-5 cm layer, i.e., split the existing 0-8 cm layer into
    # two layers of 0-5 cm and 5-8 cm
    5,
    # Add new 20-80 cm layer, i.e., split the existing 20-150 cm layer into
    # two layers of 20-80 cm and 80-150 cm
    80,
    ## Add new 150-200 cm layer, i.e., extend the soil profile
    200
  )

  for (k in seq_along(test_depths)) {
    il <- findInterval(test_depths[k], soil_layers)
    ilt <- if (il == N) il - 1 else il

    #--- test method = "interpolate" using `sand`
    tmp <- add_soil_layer(
      x = xsand,
      target_cm = test_depths[k],
      depths_cm = soil_layers,
      method = "interpolate"
    )

    expect_identical(nrow(tmp), nrow(xsand))
    expect_identical(ncol(tmp), N + 1L)

    if (il %in% c(0, N)) {
      # Adding a new shallowest or deepest layer carries existing values forward
      expect_identical(tmp[, ilt + 1], xsand[, ilt + 1])
      expect_identical(tmp[, ilt + 2], xsand[, ilt + 1])

    } else {
      # Check that interpolated values are in-between previous values
      expect_true(
        all(
          tmp[, ilt + 1] >= xsand[, ilt] & tmp[, ilt + 1] <= xsand[, ilt + 1] |
          tmp[, ilt + 1] <= xsand[, ilt] & tmp[, ilt + 1] >= xsand[, ilt + 1],
          na.rm = TRUE
        )
      )
    }


    #--- test method = "exhaust" using `TranspCoeff`
    tmp <- add_soil_layer(
      x = xtrco,
      target_cm = test_depths[k],
      depths_cm = soil_layers,
      method = "exhaust"
    )

    expect_identical(nrow(tmp), nrow(xtrco))
    expect_identical(ncol(tmp), N + 1L)

    # Check that sum of exhausted values equals the previous value
    expect_equal(
      apply(tmp[, (ilt + 1):(ilt + 2)], 1, sum),
      xtrco[, ilt + 1],
      tolerance = sqrt(.Machine[["double.eps"]])
    )

    # Check that exhausted values are larger than 0 and smaller then previously
    expect_true(
      all(
        is.na(xtrco[, ilt + 1]) |
        xtrco[, ilt + 1] == 0 |
        tmp[, ilt + 1] > 0 & tmp[, ilt + 2] > 0 &
        tmp[, ilt + 1] < xtrco[, ilt + 1] & tmp[, ilt + 2] < xtrco[, ilt + 1]
      )
    )
  }
})


test_that("dissolve_soil_layer", {
  soil_layers <- c(8, 20, 150)
  N <- length(soil_layers)

  xsand <- t(data.frame(
    site1 = c(0.5, 0.55, NA),
    site2 = c(0.3, 0.35, 0.4)
  ))

  xtrco <- t(data.frame(
    site1 = c(0.9, 0.1, NA),
    site2 = c(0.7, 0.1, 0.2)
  ))



  test_depths <- c(
    # Dissolve non-existing layer, but error
    5,
    # Dissolve the shallowest 0-8 cm layer, i.e., combine layers 0-8 cm and
    # 8-20 cm into a new 0-20 cm layer
    8,
    # Dissolve the 8-20 cm layer, i.e., combine layers 8-20 cm and
    # 20-150 cm into a new 8-150 cm layer
    20,
    # Attempt to dissolve the deepest 20-150 cm layer, but not possible
    150
  )

  for (k in seq_along(test_depths)) {
    il <- findInterval(test_depths[k], soil_layers)

    if (il == 0) {
      # Layer to dissolve does not exist
      expect_error(
        dissolve_soil_layer(
          x = xsand,
          target_cm = test_depths[k],
          depths_cm = soil_layers,
          method = "interpolate"
        )
      )

    } else if (il == N) {
      # Layer to dissolve is deepest and cannot be dissolved
      expect_warning(
        tmp <- dissolve_soil_layer(
          x = xsand,
          target_cm = test_depths[k],
          depths_cm = soil_layers,
          method = "interpolate"
        )
      )

      expect_identical(tmp, xsand)

    } else {

      #--- test method = "interpolate" using `sand`
      tmp <- dissolve_soil_layer(
        x = xsand,
        target_cm = test_depths[k],
        depths_cm = soil_layers,
        method = "interpolate"
      )

      expect_identical(nrow(tmp), nrow(xsand))
      expect_identical(ncol(tmp), N - 1L)

      # Check that interpolated values are in-between previous values
      expect_true(
        all(
          tmp[, il] >= xsand[, il] & tmp[, il] <= xsand[, il + 1] |
          tmp[, il] <= xsand[, il] & tmp[, il] >= xsand[, il + 1],
          na.rm = TRUE
        )
      )


      #--- test method = "exhaust" using `TranspCoeff`
      tmp <- dissolve_soil_layer(
        x = xtrco,
        target_cm = test_depths[k],
        depths_cm = soil_layers,
        method = "exhaust"
      )

      expect_identical(nrow(tmp), nrow(xtrco))
      expect_identical(ncol(tmp), N - 1L)

      # Check that sum of exhausted values equals the previous value
      expect_identical(
        tmp[, il],
        apply(xtrco[, il:(il + 1)], 1, sum)
      )

      # Check that exhausted values are larger then previously
      expect_true(
        all(
          is.na(tmp[, il]) |
          tmp[, il] > xtrco[, il] & tmp[, il] > xtrco[, il + 1]
        )
      )
    }
  }

})


test_that("update_soil_profile", {
  new_slyrs <- list(
    # Add and dissolve layers
    c(5, 10, 20, 100, 200),
    # Only dissolve layers
    20
  )

  soil_layers <- t(data.frame(
    site1 = c(20, 81, NA),
    site2 = c(8, 20, 150)
  ))

  N_sites <- nrow(soil_layers)
  N_layers <- ncol(soil_layers)
  colnames(soil_layers) <- paste0("depth_L", seq_len(N_layers))

  soil_data <- cbind(
    sand = t(data.frame(
      site1 = c(0.5, 0.55, NA),
      site2 = c(0.3, NA, 0.4)
    )),
    TranspCoeff = t(data.frame(
      site1 = c(0.9, 0.1, NA),
      site2 = c(0.7, 0.1, 0.2)
    ))
  )
  colnames(soil_data) <- paste0(
    rep(c("sand", "TranspCoeff"), each = N_layers),
    "_L",
    seq_len(N_layers)
  )


  #--- Check with previous soil depth and previous layers
  for (k1 in 1:4) {
    for (k2 in seq_along(new_slyrs)) {
      new_soils <- update_soil_profile(
        soil_layers = soil_layers,
        requested_soil_layers = new_slyrs[[k2]],
        soil_data = soil_data,
        keep_prev_soildepth = k1 %in% c(1, 2),
        keep_prev_soillayers = k1 %in% c(1, 3)
      )

      if (all(new_slyrs[[k2]] %in% soil_layers) && k1 %in% c(1, 3)) {
        # no updates: requested layers exists and previous ones are kept
        expect_false(new_soils[["updated"]])
      } else {
        expect_true(new_soils[["updated"]])
      }


      for (k3 in seq_len(N_sites)) {
        tmp <- new_slyrs[[k2]] < max(soil_layers[k3, ], na.rm = TRUE)
        tmp_new_slyrs <- new_slyrs[[k2]][tmp]

        expected_soillayers <- switch(
          EXPR = k1,
          # Check with previous soil depth and with previous layers
          sort(unique(c(soil_layers[k3, ], tmp_new_slyrs))),

          # Check with previous soil depth and without previous layers
          c(tmp_new_slyrs, max(soil_layers[k3, ], na.rm = TRUE)),

          # Check without previous soil depth and with previous layers
          sort(unique(c(soil_layers[k3, ], new_slyrs[[k2]]))),

          # Check without previous soil depth and without previous layers
          # but cannot dissolve deepest layer
          sort(unique(c(
            new_slyrs[[k2]],
            max(new_slyrs[[k2]], soil_layers[k3, ], na.rm = TRUE)
          )))
        )

        expect_equal(
          na.exclude(new_soils[["soil_layers"]][k3, ]),
          expected_soillayers,
          ignore_attr = TRUE
        )
      }

      # Check that re-calling function with previous output produces no changes
      new_soils2 <- update_soil_profile(
        soil_layers = new_soils[["soil_layers"]],
        requested_soil_layers = new_slyrs[[k2]],
        soil_data = new_soils[["soil_data"]],
        keep_prev_soildepth = k1 %in% c(1, 2),
        keep_prev_soillayers = k1 %in% c(1, 3)
      )

      expect_false(new_soils2[["updated"]])
      expect_identical(new_soils2[["soil_layers"]], new_soils[["soil_layers"]])
      expect_identical(new_soils2[["soil_data"]], new_soils[["soil_data"]])
    }
  }
})


test_that("reshape_soilproperties", {
  var_soilproperties <- c("db", "fsand", "fclay")

  x_wide <- data.frame(
    location = c("SiteA", "SiteB"),
    db_L1 = c(1.5, 1.6),
    fsand_L1 = c(0.7, 0.2),
    fclay_L1 = c(0.1, 0.2),
    db_L2 = c(1.6, 1.7),
    fsand_L2 = c(0.75, 0.3),
    fclay_L2 = c(0.1, 0.15)
  )


  for (k in seq_len(nrow(x_wide))) {
    x_wide_used <- x_wide[seq_len(k), , drop = FALSE]

    #--- Check reshaping from wide to long
    x_long <- reshape_soilproperties_to_long(
      x_wide_used,
      type_to = "long",
      id_site = "location",
      soilproperties = var_soilproperties
    )
    expect_s3_class(x_long, "data.frame")
    expect_named(x_long, c("location", "soillayer", "variable", "value"))


    #--- Check inversion from long back to wide
    x_wide_from_long <- reshape_soilproperties_to_wide(
      x_long,
      type_from = "long",
      id_site = "location",
      soilproperties = var_soilproperties
    )
    expect_equal(
      x_wide_from_long,
      x_wide_used,
      ignore_attr = "reshapeWide"
    )


    #--- Check reshaping from wide to semi-long
    x_semilong <- reshape_soilproperties_to_long(
      x_wide_used,
      type_to = "long_by_properties",
      id_site = "location",
      soilproperties = var_soilproperties
    )
    expect_s3_class(x_semilong, "data.frame")
    expect_named(x_semilong, c("location", "soillayer", var_soilproperties))


    #--- Check inversion from semi-long back to wide
    x_wide_from_semilong <- reshape_soilproperties_to_wide(
      x_semilong,
      type_from = "long_by_properties",
      id_site = "location",
      soilproperties = var_soilproperties
    )
    expect_equal(
      x_wide_from_semilong,
      x_wide_used,
      ignore_attr = "reshapeWide"
    )
  }
})
