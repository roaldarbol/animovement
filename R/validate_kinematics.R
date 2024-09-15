#' Title
#'
#' @param x Vector of values
#'
#' @keywords internal
ensure_not_zero <- function(x) {
  if (!all(x != 0)) {
    cli::cli_abort("Vector still has zero-values present")
  }
}

#' Ensure that values are circular
#' @inheritParams ensure_not_zero
#' @importFrom circular is.circular
#' @keywords internal
ensure_circular <- function(x) {
  if (!circular::is.circular(x)) {
    cli::cli_abort("Values have to be circular (convert with `circular::circular()`")
  }
}
