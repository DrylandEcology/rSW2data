# rSW2data v0.1.4-9000

# rSW2data v0.1.3
* `calc_calc_BareSoilEvapCoefs()` now works correctly for shallow soil profiles
  even if inputs include missing depth
  (but they must not be intermixed with with real depth values)

# rSW2data v0.1.2
* `calc_calc_BareSoilEvapCoefs()` can now work with soils with
  different soil depth profiles; correctly works in edge cases including
  one soil layer or one site; improved passing bad sites through; and
  consider soil texture of layers within `depth_max_bs_evap_cm`.

# rSW2data v0.1.1
* `impute_soils()` imputes now independently of order of sites and soil layers.

# rSW2data v0.1.0
* Initial release.
