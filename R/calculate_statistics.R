#' Calculate summary statistics
#'
#' @description
#' Calculate summary statistics for tracks
#'
#' @param data A kinematics data frame
#' @param threshold_velocity Choose which observations to use based on the velocity. A number, "auto" or "none". Can take a number (e.g. estimated from histograms) or "auto". "auto" fits a density function to the velocities and tries to identify a local minimum between the first and second local maxima, and uses that as the threshold. "none" keeps all observations.
#'
#' @return An data frame data frame with kinematics calculated
#' @export
#' @import dplyr
#' @importFrom rlang :=
#' @importFrom rlang .data
#'
calculate_statistics <- function(
    data,
    threshold_velocity
) {
  # Make some tests to ensure that `calculate_kinematics` has been run first

  # Make sure to remove observations with almost no movement (figure out a robust method for this)

  # Calculate translational and rotational separately (maybe?) and gather at the end
  data <- data |>
    dplyr::summarise()

    # dplyr::mutate(distance = sqrt(.data$dx^2 + .data$dy^2),
    #               v_translation = .data$distance * sampling_rate,
    #               direction = atan2(.data$dx, .data$dy),
    #               direction = if_else(.data$dy == 0 | .data$dy == 0, NA, direction),
    #               direction = if_else(.data$direction < 0, .data$direction + 2*pi, .data$direction), # Keep direction between 0 and 2*pi
    #               direction = zoo::na.locf(.data$direction, na.rm = FALSE),
    #               rotation = direction - lag(direction),
    #               # rotation = dplyr::if_else(abs(.data$dx) > 1 & abs(.data$dy) > 1,
    #               #                           abs(dplyr::lag(.data$direction) - .data$direction),
    #               #                           0),
    #               # rotation = dplyr::if_else(.data$rotation > pi, 2*pi - .data$rotation, .data$rotation),
    #               v_rotation = .data$rotation * sampling_rate,
    # )

  return(data)
}
