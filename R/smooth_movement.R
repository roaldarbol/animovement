#' Smooth tracks
#'
#' @description
#' Filtering/smoothing tracks is standard practice to root out noise in movement data.
#' Here we provide some filter functions to do this. The function expects the data to be in the standard format,
#' containing at least x, y and time variables.
#'
#'
#' @param data Data frame
#' @param method Which smoothing method to use. Currently only rolling mean is implemented ("rolling_mean").
#' @param window_width How many observations to use for rolling window filters (e.g. "rolling_mean" or "rolling_median").
#'
#' @return A movement data frame
#' @export
#' @import dplyr
#' @importFrom zoo rollmean
#' @importFrom rlang :=
#'
smooth_track <- function(
    data,
    method = c("rolling_median"),
    window_width = 5
) {

  # Back-transform to dx and dy
  data <- data |>
    dplyr::mutate(dx = x - lag(.data$x),
                  dy = y - lag(.data$y))

  # Filter the dx/dy values
  if (method == "rolling_mean"){
    data <- data |>
    dplyr::mutate(
      dx = zoo::rollmean(.data$dx, k = window_width, fill = NA),
      dy = zoo::rollmean(.data$dy, k = window_width, fill = NA)
    )
  } else if (method == "rolling_median"){
    data <- data |>
      dplyr::mutate(
        dx = zoo::rollmedian(.data$dx, k = window_width, fill = NA),
        dy = zoo::rollmedian(.data$dy, k = window_width, fill = NA)
      )
  }

  # Re-compute xy-coordinates
  data <- data |>
    dplyr::mutate(x = cumsum(coalesce(.data$dx, 0)) + .data$dx*0,
                  y = cumsum(coalesce(.data$dy, 0)) + .data$dy*0) |>
    select(-.data$dx, -.data$dy)
  return(data)
}
