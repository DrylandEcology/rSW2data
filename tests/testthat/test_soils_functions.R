context("Manipulate soil layers")

#---INPUTS
if (FALSE) {
  set.seed(1234L)
}

nrows <- 10L
test_space <- 1000L

make_test_x <- function(ncol, nrow = nrows, space = test_space) {
  as.data.frame(matrix(sample(space, nrow * ncol), nrow = nrow, ncol = ncol,
      dimnames = list(NULL, paste0("L", seq_len(ncol)))))
}

test_data <- list(
  make_test_x(1),
  make_test_x(2),
  make_test_x(3),
  make_test_x(4),
  make_test_x(10)
)

test_weights <- list(c(NA, 1), c(0, 1), c(0.5, 0.5), c(1, 0), c(1, NA),
  c(NA, NA))


#---TESTS
test_that("add_layer_to_soil", {

  #--- Loop through test data set, layers, weight options, and methods
  for (k in seq_along(test_data))
    for (il in c(0, seq_len(ncol(test_data[[k]]))))
      for (iw in seq_along(test_weights))
        for (im in c("interpolate", "exhaust")) {
          loop_info <- paste("test", k, "/ layer", il, "/ w", iw,
            "/ method", im)

          res <- add_layer_to_soil(x = test_data[[k]], il = il,
            w = test_weights[[iw]], method = im)

          # Test dimensions
          expect_equal(ncol(res), ncol(test_data[[k]]) + 1L, info = loop_info)
          expect_equal(nrow(res), nrow(test_data[[k]]), info = loop_info)

          # Test data consistency of new layer
          lnew <- res[, il + 1]
          ilo <- if (isTRUE(all.equal(il, 0L))) il + 1L else il
          weights_not_used <- im == "interpolate" &&
            (isTRUE(all.equal(il, 0)) ||
                isTRUE(all.equal(il, ncol(test_data[[k]]))))

          expect_equal(all(is.finite(lnew)),
            all(is.finite(test_data[[k]][, ilo])) &&
            if (weights_not_used) TRUE else all(is.finite(test_weights[[iw]])),
            info = loop_info)
  }


  #--- Test cases that fail
  # weights vector 'w' is not of length 2
  expect_error(add_layer_to_soil(x = test_data[[1]], il = 1, w = c(0, 1, 0),
    method = "interpolate"))
  expect_error(add_layer_to_soil(x = test_data[[1]], il = 1, w = 1,
    method = "interpolate"))
  # but doesn't fail if a row is named 'depth_cm'
  # Add a deeper layer
  x <- test_data[[1]]
  dimnames(x)[[1]][1] <- "depth_cm"
  res <- add_layer_to_soil(x = x, il = 1, w = 5, method = "interpolate")
  expect_equal(res[-1, 1], res[-1, 2])
  expect_equal(res[1, 2], 2 * res[1, 1] - 5)
  # Add a more shallow layer
  res <- add_layer_to_soil(x = x, il = 0, w = 5, method = "interpolate")
  expect_equal(res[-1, 1], res[-1, 2])
  expect_equal(res[1, 1], 5)


  # data matrix has no columns
  expect_error(add_layer_to_soil(x = matrix(NA, nrow = nrows, ncol = 0), il = 1,
    w = c(0.5, 0.5), method = "interpolate"))

  # requested layer cannot be added
  expect_error(add_layer_to_soil(x = test_data[[3]], il = -1, w = c(0.5, 0.5),
    method = "interpolate"))
  expect_error(add_layer_to_soil(x = test_data[[3]], il = test_data[[3]] + 1L,
    w = c(0.5, 0.5), method = "interpolate"))

})



test_that("update_soil_profile", {
  new_slyrs <- c(5, 10, 20, 100, 200)

  soil_layers <- t(data.frame(
    site1 = c(20, 81, NA, NA, NA, NA),
    site2 = c(8, 20, 150, NA, NA, NA)
  ))

  N_sites <- nrow(soil_layers)
  N_layers <- ncol(soil_layers)
  colnames(soil_layers) <- paste0("depth_L", seq_len(N_layers))

  soil_data <- cbind(
    sand = t(data.frame(
      site1 = c(0.5, 0.55, NA, NA, NA, NA),
      site2 = c(0.3, NA, 0.4, NA, NA, NA)
    )),
    TranspCoeff = t(data.frame(
      site1 = c(0.9, 0.1, NA, NA, NA, NA),
      site2 = c(0.7, 0.1, 0.2, NA, NA, NA)
    ))
  )
  colnames(soil_data) <- paste0(
    rep(c("sand", "TranspCoeff"), each = N_layers),
    "_L",
    seq_len(N_layers)
  )

  new_soils <- update_soil_profile(
    soil_layers = soil_layers,
    requested_soil_layers = new_slyrs,
    soil_data = soil_data
  )

  expect_true(new_soils[["updated"]])

  for (k in seq_len(N_sites)) {
    expected_soillayers <- sort(unique(c(
      soil_layers[k, ],
      new_slyrs[new_slyrs < max(soil_layers[k, ], na.rm = TRUE)]
    )))

    expect_equivalent(
      na.exclude(new_soils[["soil_layers"]][k, ]),
      expected_soillayers
    )
  }

})
