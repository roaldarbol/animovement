#' Calculate distance (Pythagoras)
#' Calculate distance from an x and y distance, using Pythagoras theorem.
#' @param dx dx
#' @param dy dy
#' @keywords internal
calculate_distance <- function(dx, dy) {
  sqrt(dx^2 + dy^2)
}
