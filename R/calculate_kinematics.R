#' Calculate kinematics
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Calculate kinematics.
#'
#' @param data Data frame
#'
#' @return A data frame with kinematics calculated
#' @export
#' @import dplyr
#' @importFrom rlang :=
#' @importFrom rlang .data
#'
calculate_kinematics <- function(
    data) {
  # We first temporarily back-calculate from our xy coordinates to the distances (dx, dy) covered between each observation (which is what we got from the sensors initially)
  data <- data |>
    dplyr::mutate(
      dx = .data$x - lag(.data$x),
      dy = .data$y - lag(.data$y),
      # dt = .data$time - lag(.data$time)
    )

  # Find the sampling rate
  # sampling_rate <- round(1 / stats::median(data$dt, na.rm = TRUE))

  # Calculate kinematics
  data <- data |>
    dplyr::mutate(
      distance = calculate_distance(.data$dx, .data$dy),
      v_translation = calculate_derivative(.data$distance, 0, .data$time, lag(.data$time)),
      a_translation = calculate_derivative(.data$v_translation, lag(.data$v_translation), .data$time, lag(.data$time)),
      direction = calculate_direction(.data$dx, .data$dy),
      rotation = calculate_angular_difference(.data$direction, lag(.data$direction)),
      v_rotation = calculate_derivative(0, .data$rotation, .data$time, lag(.data$time)),
      a_rotation = calculate_derivative(.data$v_rotation, lag(.data$v_rotation), .data$time, lag(.data$time)),
      # We change the directions to stay within 2pi only here, otherwise rotation becomes harder to alculate
      direction = adjust_direction(.data$direction) # Keep direction between 0 and 2*pi
    )

  # Remove leftover columns
  data <- data |>
    select(-c("dx", "dy"))
  # select(-c("dx", "dy", "dt"))

  return(data)
}

#' Calculate distance (Pythagoras)
#' Calculate distance from an x and y distance, using Pythagoras theorem.
#' @param dx dx
#' @param dy dy
#' @keywords internal
calculate_distance <- function(dx, dy) {
  sqrt(dx^2 + dy^2)
}

#' Calculate direction
#' Calculate direction (angle) from x and y distance using the (two-argument) arc-tangent. Converts to `circular`.
#' @inheritParams calculate_distance
#' @importFrom circular circular
#' @keywords internal
calculate_direction <- function(dx, dy) {
  if_else(dx == 0 & dy == 0, NA, circular::circular(atan2(dy, dx), modulo = "asis"))
}

#' Calculate angular difference
#' @param from_angle From angle
#' @param to_angle To angle
#' @keywords internal
calculate_angular_difference <- function(from_angle, to_angle) {
  ensure_circular(from_angle)
  ensure_circular(to_angle)
  diff_angle <- from_angle - to_angle
  case_when(diff_angle > pi ~ diff_angle - 2 * pi,
    diff_angle < -pi ~ diff_angle + 2 * pi,
    .default = diff_angle
  )
}

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

#' Adjust direction
#' Constrains the direction to be between 0 and 2pi
#' @param direction Direction
#' @importFrom circular circular
#' @keywords internal
adjust_direction <- function(direction) {
  circular::circular(direction, modulo = "2pi")
}
