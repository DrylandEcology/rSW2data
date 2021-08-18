test_that("Vapor pressure", {
  tol <- 1e-6

  expect_equal(vp0(0), 0.6108, tolerance = tol)

  expect_equal(vp0(-10), 0.316988, tolerance = tol)
  expect_equal(vp0(20), 1.983074, tolerance = tol)
  expect_equal(vp0(40), 5.539641, tolerance = tol)

  expect_equal(vpd(Tmin = -10, Tmax = -5), 0.062864, tolerance = tol)
  expect_equal(
    vpd(Tmin = -10, Tmax = -5, RHmean = 0),
    0.3798519,
    tolerance = tol
  )
  expect_equal(
    vpd(Tmin = -10, Tmax = -5, RHmean = 25),
    0.284889,
    tolerance = tol
  )
  expect_equal(
    vpd(Tmin = -10, Tmax = -5, RHmean = 50),
    0.189926,
    tolerance = tol
  )
  expect_equal(
    vpd(Tmin = -10, Tmax = -5, RHmean = 75),
    0.09496299,
    tolerance = tol
  )
  expect_equal(
    vpd(Tmin = -10, Tmax = -5, RHmean = 100),
    0,
    tolerance = tol
  )

  expect_equal(vpd(Tmin = 0, Tmax = 0), 0, tolerance = tol)

  expect_equal(vpd(Tmin = 10, Tmax = 20), 0.4296933, tolerance = tol)
  expect_equal(vpd(Tmin = 30, Tmax = 40), 1.084296, tolerance = tol)
  expect_equal(vpd(Tmin = 10, Tmax = 40), 2.207977, tolerance = tol)
})
