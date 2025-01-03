#' Rotate coordinates in Cartesian space
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Rotates coordinates in Cartesian space based on two alignment points.
#' The rotation aligns these points either with the 0-degree axis (parallel)
#' or makes them perpendicular to it. This is particularly useful for
#' creating egocentric reference frames or standardizing orientation across
#' multiple frames or individuals.
#'
#' @param data movement data frame with columns: time, individual, keypoint, x, y
#' @param alignment_points character vector of length 2 specifying the keypoint names
#'        to use for alignment
#' @param align_perpendicular logical; if TRUE, alignment_points will be rotated to be
#'        perpendicular to the 0-degree axis. If FALSE (default), alignment_points
#'        will be rotated to align with the 0-degree axis
#'
#' @details
#' The function processes each individual separately and maintains their independence.
#' For each time point, it:
#' 1. Calculates the vector between the alignment points
#' 2. Determines the current angle of this vector
#' 3. Rotates all points to achieve the desired alignment
#'
#' @return movement data frame with rotated coordinates
#' @export
rotate_coords <- function(
    data,
    alignment_points,     # Two keypoint names to use for alignment
    align_perpendicular = FALSE  # If TRUE, alignment_points will be made perpendicular to 0°
) {
  # Input validation
  if (length(alignment_points) != 2) {
    cli::cli_abort("alignment_points must contain exactly 2 keypoint names")
  }
  if (!all(alignment_points %in% unique(data$keypoint))) {
    cli::cli_abort("Some specified keypoints not found in data")
  }

  # Process each individual separately
  individuals <- unique(data$individual)
  out_data <- data.frame()

  for (ind in individuals) {
    ind_data <- data |>
      dplyr::filter(.data$individual == ind)

    # Get all coordinates of alignment points for this individual
    p1 <- ind_data |>
      dplyr::filter(.data$keypoint == alignment_points[1]) |>
      dplyr::select(.data$time, .data$x, .data$y) |>
      dplyr::rename(x1 = .data$x, y1 = .data$y)

    p2 <- ind_data |>
      dplyr::filter(.data$keypoint == alignment_points[2]) |>
      dplyr::select(.data$time, .data$x, .data$y) |>
      dplyr::rename(x2 = .data$x, y2 = .data$y)

    # Calculate rotation angles for each time point
    angles <- p1 |>
      dplyr::left_join(p2, by = "time") |>
      dplyr::mutate(
        # Calculate vector between alignment points
        vec_x = .data$x2 - .data$x1,
        vec_y = .data$y2 - .data$y1,
        # Calculate current angle and needed rotation
        current_angle = atan2(.data$vec_y, .data$vec_x),
        target_angle = dplyr::if_else(isTRUE(align_perpendicular), pi/2, 0),
        rotation_angle = .data$target_angle - .data$current_angle
      ) |>
      dplyr::select(.data$time, .data$rotation_angle)

    # Apply rotation to all points for this individual
    ind_rotated <- ind_data |>
      dplyr::left_join(angles, by = "time") |>
      dplyr::mutate(
        x_new = .data$x * cos(.data$rotation_angle) - .data$y * sin(.data$rotation_angle),
        y_new = .data$x * sin(.data$rotation_angle) + .data$y * cos(.data$rotation_angle)
      ) |>
      dplyr::select(-.data$rotation_angle, -.data$x, -.data$y) |>
      dplyr::rename(x = .data$x_new, y = .data$y_new)

    out_data <- dplyr::bind_rows(out_data, ind_rotated)
  }

  return(out_data |> dplyr::arrange(.data$time, .data$individual, .data$keypoint))
}
