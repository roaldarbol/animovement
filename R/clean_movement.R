#' Smooth movement
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Filtering/smoothing tracks is standard practice to root out noise in movement data.
#' Here we provide some filter functions to do this. The function expects the data to be in the standard format,
#' containing at least x, y and time variables.
#'
#'
#' @param data Data frame
#' @param method Which smoothing method to use. options:  "rolling_median (default), "rolling_mean".
#' @param window_width How many observations to use for rolling window filters (e.g. "rolling_mean" or "rolling_median").
#' @param use_derivatives whether to use the derivatives (difference between frames) to perform the smoothing. Useful for trackball data.
#'
#' @return A movement data frame
#' @export
#' @import dplyr
#' @importFrom roll roll_mean roll_median
#'
smooth_movement <- function(
    data,
    method = c("rolling_median"),
    window_width = 5,
    use_derivatives = FALSE) {

  # Quick checks on the data
  ensure_output_header_names(data)
  ensure_output_header_class(data)

  data <- data |>
    dplyr::group_by(.data$individual, .data$keypoint)

  # Back-transform to dx and dy
  if (use_derivatives == TRUE){
    data <- smooth_derivatives(data, method, window_width)
  }

  # Rolling mean
  else if (method == "rolling_mean"){
    data <- data |>
      dplyr::mutate(
        x = roll::roll_mean(.data$x, width = window_width),
        y = roll::roll_mean(.data$y, width = window_width)
      )
  }

  # Rolling median
  else if (method == "rolling_median") {
    data <- data |>
      dplyr::mutate(
        x = roll::roll_median(.data$x, width = window_width),
        y = roll::roll_median(.data$y, width = window_width)
      )
  }

  return(data)
}

#' Smooth derivatives (when original data measured differences between movements)
#'
#' @inheritParams smooth_movement
#'
#' @keywords internal
smooth_derivatives <- function(data, method, window_width){

  # Back-transform to derivatives
  data <- data |>
    dplyr::mutate(
      dx = .data$x - lag(.data$x),
      dy = .data$y - lag(.data$y)
    )

  # Filter the dx/dy values
  if (method == "rolling_mean") {
    data <- data |>
      dplyr::mutate(
        dx = roll::roll_mean(.data$dx, width = window_width),
        dy = roll::roll_mean(.data$dy, width = window_width)
      )
  } else if (method == "rolling_median") {
    data <- data |>
      dplyr::mutate(
        dx = roll::roll_median(.data$dx, width = window_width),
        dy = roll::roll_median(.data$dy, width = window_width)
      )
  }

  # Re-compute xy-coordinates
  data <- data |>
    dplyr::mutate(
      x = cumsum(coalesce(.data$dx, 0)) + .data$dx * 0,
      y = cumsum(coalesce(.data$dy, 0)) + .data$dy * 0
    ) |>
    select(-"dx", -"dy")

  return(data)
}
