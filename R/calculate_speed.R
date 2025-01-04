#' Calculate Speed from Position Data
#'
#' @description
#' Calculates the instantaneous speed from x, y coordinates and time data.
#' Speed is computed as the absolute magnitude of velocity (change in position over time).
#'
#' @param x Numeric vector of x coordinates
#' @param y Numeric vector of y coordinates
#' @param time Numeric vector of time values
#'
#' @return Numeric vector of speeds. The first value will be NA since speed
#'   requires two positions to calculate.
#'
#' @examples
#' \dontrun{
#' # Inside dplyr pipeline
#' data |>
#'   group_by(keypoint) |>
#'   mutate(speed = calculate_speed(x, y, time))
#' }
#'
#' @export
calculate_speed <- function(x, y, time) {
  # Calculate position differences
  dx <- diff(x)
  dy <- diff(y)
  dt <- diff(time)

  # Calculate speed: sqrt((dx/dt)^2 + (dy/dt)^2)
  speed <- sqrt((dx/dt)^2 + (dy/dt)^2)

  # Add NA at the start since we can't calculate speed for first point
  c(NA, speed)
}
