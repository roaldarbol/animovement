#' Calculate the derivative (dx/dt)
#' Calculate the derivative (dx/dt) with four arguments
#' @param from_x Current x value
#' @param to_x Lagging x value
#' @param from_t Current timestamp
#' @param to_t Lagging timestamp
#' @keywords internal
calculate_derivative <- function(from_x, to_x, from_t, to_t) {
  (from_x - to_x) / (from_t - to_t)
}
