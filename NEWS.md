# rSW2data v0.1.6-9000
* `calc_BareSoilEvapCoefs()` gained arguments `"noSoilValue"` and `"tolerance"`.

# rSW2data v0.1.5
* `check_depth_table()` now returns a vector of failing sites
  for each implemented check.
* Updated package infrastructure and dependency requirements including
  `yaml` scripts for Github Actions, `lintr`, `spelling`, `roxygen2`.


# rSW2data v0.1.4
* New `reshape_soilproperties_to_long()` and `reshape_soilproperties_to_wide()`
  reshape data frames containing soil properties between wide and (semi-) long
  formats.
* Linting updated to `lintr` >= 3 and
  lint workflow switched from package tests to Github Action (#8).
* Github Actions are triggered for `release/**` branches in addition to `main`.


# rSW2data v0.1.3
* `calc_BareSoilEvapCoefs()` now works correctly for shallow soil profiles
  even if inputs include missing depth
  (but they must not be intermixed with real depth values)


# rSW2data v0.1.2
* `calc_BareSoilEvapCoefs()` can now work with soils with
  different soil depth profiles; correctly works in edge cases including
  one soil layer or one site; improved passing bad sites through; and
  consider soil texture of layers within `depth_max_bs_evap_cm`.


# rSW2data v0.1.1
* `impute_soils()` imputes now independently of order of sites and soil layers.


# rSW2data v0.1.0
* Initial release.
