#' Translate coordinates (Cartesian)
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Translates coordinates in Cartesian space. Takes either a single point
#' (`to_x` and `to_y`), a vector with the same length as the time dimension or a
#' keypoint (`to_keypoint`), which can be used to transform the data into an
#' egocentric reference frame.
#'
#' @param data movement data frame with columns: time, individual, keypoint, x, y
#' @param to_x x coordinates; either a single value or a time-length vector
#' @param to_y y coordinates; either a single value or a time-length vector
#' @param to_z z coordinates (only if 3D); either a single value or a time-length vector
#' @param to_keypoint all other coordinates becomes relative to this keypoint
#'
#' @return movement data frame with translated coordinates
#' @export
translate_coords <- function(data, to_x=0, to_y=0, to_z=NULL, to_keypoint=NULL){
  # Takes a keypoint
  if (!is.null(to_keypoint)){
    if (is.character(to_keypoint)){
      if (length(to_keypoint) > 1){
        cli::cli_abort("Only 1 keypoint can be supplied to `to_keypoint`, received {length(to_keypoint)}.")
      }
      if (!to_keypoint %in% unique(data$keypoint)){
        cli::cli_abort("Keypoint {to_keypoint} is not among the keypoints in the data, which are {unique(data$keypoint)}.")
      }
      data <- translate_coords_keypoint(data, to_keypoint)
    }
  }
  # Takes numeric vectors
  else if (all(length(to_x) == 1 & length(to_y) == 1)){
    if (!all(is.numeric(to_x) & is.numeric(to_y))){
      cli::cli_abort("Expected to_x and to_y must be numeric. Got {class(to_x)} and {class(to_y)}.")
    }
      # Takes a single point
      data <- translate_coords_vector(data, to_x, to_y)
  } else if (all(length(to_x) == length(unique(data$time)) &
                 length(to_y) == length(unique(data$time)))){
      # Takes a time-length vector
      data <- translate_coords_vector(data, to_x, to_y)
  }

  return(data)
}

#' Translate coordinates relative to a keypoint
#'
#' @inheritParams translate_coords
#' @keywords internal
translate_coords_keypoint <- function(data, to_keypoint){
  out_data <- data.frame()
  individuals <- unique(data$individual)
  for (i in 1:length(individuals)){
    ref_coords <- data |>
      dplyr::filter(.data$individual == individuals[i]) |>
      dplyr::filter(.data$keypoint == to_keypoint)
    data_individual <- data |>
      dplyr::filter(.data$individual == individuals[i]) |>
      dplyr::group_by(.data$individual, .data$keypoint) |>
      dplyr::mutate(x = .data$x - ref_coords$x,
                    y = .data$y - ref_coords$y) |>
      # dplyr::bind_rows(ref_coords) |>
      dplyr::arrange(.data$time, .data$individual, .data$keypoint)
    out_data <- bind_rows(out_data, data_individual)
  }

  return(out_data)
}

#' Translate coordinates relative to coordinates
#'
#' @inheritParams translate_coords
#' @keywords internal
translate_coords_vector <- function(data, to_x, to_y, to_z=NULL){
  ensure_coord_cartesian(data)
  data <- data |>
    dplyr::mutate(x = .data$x - to_x,
                  y = .data$y - to_y)
  # For 3D
  if (!is.null(to_z)){
    data <- data |>
      dplyr::mutate(z = .data$z - to_z)
  }

  return(data)
}

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
    stop("alignment_points must contain exactly 2 keypoint names")
  }
  if (!all(alignment_points %in% unique(data$keypoint))) {
    stop("Some specified keypoints not found in data")
  }

  # Process each individual separately
  individuals <- unique(data$individual)
  out_data <- data.frame()

  for (ind in individuals) {
    ind_data <- data |> filter(individual == ind)

    # Get all coordinates of alignment points for this individual
    p1 <- ind_data |>
      filter(keypoint == alignment_points[1]) |>
      select(time, x, y) |>
      rename(x1 = x, y1 = y)

    p2 <- ind_data |>
      filter(keypoint == alignment_points[2]) |>
      select(time, x, y) |>
      rename(x2 = x, y2 = y)

    # Calculate rotation angles for each time point
    angles <- p1 |>
      left_join(p2, by = "time") |>
      mutate(
        # Calculate vector between alignment points
        vec_x = x2 - x1,
        vec_y = y2 - y1,
        # Calculate current angle and needed rotation
        current_angle = atan2(vec_y, vec_x),
        target_angle = if_else(isTRUE(align_perpendicular), pi/2, 0),
        rotation_angle = target_angle - current_angle
      ) |>
      select(time, rotation_angle)

    # Apply rotation to all points for this individual
    ind_rotated <- ind_data |>
      left_join(angles, by = "time") |>
      mutate(
        x_new = x * cos(rotation_angle) - y * sin(rotation_angle),
        y_new = x * sin(rotation_angle) + y * cos(rotation_angle)
      ) |>
      select(-rotation_angle, -x, -y) |>
      rename(x = x_new, y = y_new)

    out_data <- bind_rows(out_data, ind_rotated)
  }

  return(out_data |> arrange(time, individual, keypoint))
}

#' Transform coordinates to egocentric reference frame
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Transforms Cartesian coordinates into an egocentric reference frame through
#' a two-step process: translation followed by rotation. First translates all
#' coordinates relative to a reference keypoint, then rotates the coordinate
#' system based on specified alignment points.
#'
#' @param data movement data frame with columns: time, individual, keypoint, x, y
#' @param to_keypoint character; keypoint to use as the new origin
#' @param alignment_points character vector of length 2 specifying the keypoint names
#'        to use for alignment
#' @param align_perpendicular logical; if TRUE, alignment_points will be rotated to be
#'        perpendicular to the 0-degree axis. If FALSE (default), alignment_points
#'        will be rotated to align with the 0-degree axis
#'
#' @details
#' This function combines translation and rotation to create an egocentric reference
#' frame. It:
#' 1. Translates all coordinates relative to the specified keypoint (to_keypoint)
#' 2. Rotates the coordinate system based on the alignment points
#'
#' The translation makes the reference keypoint the new origin (0,0), while the
#' rotation standardizes the orientation. This is particularly useful for:
#' * Creating egocentric reference frames
#' * Standardizing pose data across frames or individuals
#' * Analyzing relative motion patterns
#'
#' @examples
#' \dontrun{
#' # Transform coordinates to make nose the origin and align body axis
#' transformed_data <- transform_to_egocentric(
#'   data,
#'   to_keypoint = "nose",
#'   alignment_points = c("nose", "tail"),
#'   align_perpendicular = FALSE
#' )
#'
#' # Transform to make nose origin and ears perpendicular to forward axis
#' transformed_data <- transform_to_egocentric(
#'   data,
#'   to_keypoint = "nose",
#'   alignment_points = c("ear_left", "ear_right"),
#'   align_perpendicular = TRUE
#' )
#' }
#'
#' @return movement data frame in egocentric reference frame
#' @export
transform_to_egocentric <- function(
    data,
    to_keypoint,         # Reference point for translation
    alignment_points,     # Two keypoint names to use for alignment
    align_perpendicular = FALSE  # If TRUE, alignment_points will be made perpendicular to 0°
) {
  # First translate
  translated_data <- translate_coords(data, to_keypoint = to_keypoint)

  # Then rotate
  transformed_data <- rotate_coords(translated_data, alignment_points, align_perpendicular)

  return(transformed_data)
}

#' Map from Cartesian to polar coordinates
#'
#' @param data movement data frame with Cartesian coordinates
#'
#' @return movement data frame with polar coordinates
#' @export
map_to_polar <- function(data){
  ensure_coord_cartesian(data)
  data <- data |>
    mutate(
      rho = cartesian_to_rho(.data$x, .data$y),
      theta = cartesian_to_theta(.data$x, .data$y)) |>
    dplyr::select(-c("x", "y")) |>
    dplyr::relocate("rho", .after = "keypoint") |>
    dplyr::relocate("theta", .after = "rho")
  return(data)
}

#' Map from polar to Cartesian coordinates
#'
#' @param data movement data frame with polar coordinates
#'
#' @return movement data frame with Cartesian coordinates
#' @export
map_to_cartesian <- function(data){
  ensure_coord_polar(data)
  data <- data |>
    dplyr::mutate(
      x = polar_to_x(.data$rho, .data$theta),
      y = polar_to_y(.data$rho, .data$theta)) |>
    dplyr::select(-c("rho", "theta")) |>
    dplyr::relocate("x", .after = "keypoint") |>
    dplyr::relocate("y", .after = "x")
  return(data)
}
