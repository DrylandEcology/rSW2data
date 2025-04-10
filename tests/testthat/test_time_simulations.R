
#--- INPUTS
input_sim_time <- list(
  # test object 1: startyr is leap year
  startyr_leapyear = list(
    useyrs = yrs <- 1979:2010, # nolint: implicit_assignment_linter.
    no.usedy = length(rSW2utils::days_in_years(yrs[[1]], yrs[[length(yrs)]])),
    no.usemo = length(yrs) * 12L
  ),
  # test object 2: startyr is not leap year
  startyr_noleapyear = list(
    useyrs = yrs <- 1969:2000, # nolint: implicit_assignment_linter.
    no.usedy = length(rSW2utils::days_in_years(yrs[[1]], yrs[[length(yrs)]])),
    no.usemo = length(yrs) * 12L
  )
)


#--- TESTS
test_that("Obtain time information", {
  # Spinup of simulation
  expect_identical(getStartYear(1980), 1981L)
  expect_identical(getStartYear(0), 1L)
  expect_identical(getStartYear(0, 10), 10L)


  # Describe simulation time
  st1 <- setup_time_simulation_run(
    list(simstartyr = 1979, startyr = 1980, endyr = 2010)
  )
  ns <- names(st1)
  expect_identical(
    st1,
    setup_time_simulation_run(
      sim_time = list(spinup_N = 1, startyr = 1980, endyr = 2010)
    )[ns]
  )
  expect_identical(
    st1,
    setup_time_simulation_run(
      sim_time = list(simstartyr = 1979, spinup_N = 1, endyr = 2010)
    )[ns]
  )


  # Simulation time aggregation lists
  st2 <- list(N = list(), S = list())


  for (k in seq_along(input_sim_time)) {
    # nolint start: implicit_assignment_linter.
    expect_silent(
      st2[["N"]] <- simTiming_ForEachUsedTimeUnit(
        useyrs = input_sim_time[[k]][["useyrs"]],
        latitude = 90
      )
    )

    expect_silent(
      st2[["S"]] <- simTiming_ForEachUsedTimeUnit(
        useyrs = input_sim_time[[k]][["useyrs"]],
        latitude = -90
      )
    )
    # nolint end.


    for (h in seq_along(st2)) {
      dvals <- grep(
        "ForEachUsedDay",
        names(st2[["N"]]),
        value = TRUE,
        fixed = TRUE
      )

      for (d in dvals) {
        info <- paste(
          "For test =", names(input_sim_time)[k], "/ d =",
          shQuote(d), "/ hemisphere =", names(st2)[[h]]
        )

        expect_identical(
          length(st2[[h]][[d]]),
          input_sim_time[[k]][["no.usedy"]],
          info = info
        )
      }

      dvals <- grep(
        "ForEachUsedMonth",
        names(st2[["N"]]),
        value = TRUE,
        fixed = TRUE
      )

      for (d in dvals) {
        info <- paste(
          "For test =", names(input_sim_time)[k], "/ d =",
          shQuote(d), "/ hemisphere =", names(st2)[[h]]
        )

        expect_identical(
          length(st2[[h]][[d]]),
          input_sim_time[[k]][["no.usemo"]],
          info = info
        )
      }
    }
  }
})


test_that("Check years", {
  # nolint start: implicit_assignment_linter.
  expect_silent(
    x <- update_requested_years(2000, 2010, 1950, 2010, verbose = FALSE)
  )
  expect_identical(x[["start_year"]], 2000L)
  expect_identical(x[["end_year"]], 2010L)

  expect_message(
    x <- update_requested_years(1940, 2010, 1950, 2010, verbose = TRUE),
    regexp = "requested start year"
  )
  expect_identical(x[["start_year"]], 1950L)
  expect_identical(x[["end_year"]], 2010L)

  expect_message(
    x <- update_requested_years(2000, 2020, 1950, 2010, verbose = TRUE),
    regexp = "requested end year"
  )
  expect_identical(x[["start_year"]], 2000L)
  expect_identical(x[["end_year"]], 2010L)
  # nolint end.
})
