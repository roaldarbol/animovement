#' Calculate kinematics
#'
#' @description
#' Calculate kinematics.
#'
#' @param data Data frame
#'
#' @return An data frame data frame with kinematics calculated
#' @export
#' @import dplyr
#' @importFrom rlang :=
#' @importFrom rlang .data
#'
calculate_kinematics <- function(
    data
) {
  # We first temporarily back-calculate from our xy coordinates to the distances (dx, dy) covered between each observation (which is what we got from the sensors initially)
  data <- data |>
    dplyr::mutate(dx = .data$x - lag(.data$x),
                  dy = .data$y - lag(.data$y),
                  dt = .data$time - lag(.data$time))

  # Find the sampling rate
  sampling_rate <- round(1 / median(data$dt, na.rm = TRUE))

  # Calculate kinematics
  data <- data |>
    dplyr::mutate(distance = if_else(.data$dx^2 > 0 & .data$dy^2 > 0, sqrt(.data$dx^2 + .data$dy^2), 0),
                  v_translation = .data$distance * sampling_rate,
                  direction = if_else(.data$dx^2 > 0 & .data$dy^2 > 0, atan2(.data$dx, .data$dy), 0),
                  direction = if_else(.data$dy == 0 | .data$dy == 0, NA, direction),
                  rotation = direction - lag(direction),
                  v_rotation = .data$rotation * sampling_rate,
                  # We change the directions to stay within 2pi only here, otherwise rotation becomes harder to alculate
                  direction = if_else(.data$direction < 0, .data$direction + 2*pi, .data$direction), # Keep direction between 0 and 2*pi
                  direction = zoo::na.locf(.data$direction, na.rm = FALSE),
    )

  # Remove leftover columns
  data <- data |>
    select(-c("dx", "dy", "dt"))

  return(data)
}
