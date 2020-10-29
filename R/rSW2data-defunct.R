#' Defunct functions in package \pkg{rSW2data}
#'
#' Executing a defunct function will fail and tell you which function
#' replaces them.
#'
#' @param ... Previous function arguments.
#'
#' @name rSW2data-defunct
NULL


#' @rdname rSW2data-defunct
#' @export
calc_weights_from_depths <- function(...) {
  .Defunct(
    new = "",
    package = "",
    msg = paste(
      "`rSW2data::calc_weights_from_depths()`",
      "is defunct without replacement;",
      "it was necessary for using function `rSW2data::add_layer_to_soil()`",
      "which is superceded by `rSW2data::add_soil_layer()`."
    )
  )
}

#' @rdname rSW2data-defunct
#' @export
add_layer_to_soil <- function(...) {
  .Defunct(
    new = "add_soil_layer",
    package = "rSW2data",
    msg = paste(
      "`rSW2data::add_layer_to_soil()`",
      "is defunct;",
      "please use",
      "`rSW2data::add_soil_layer()`",
      "instead."
    )
  )
}
