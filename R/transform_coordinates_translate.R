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
