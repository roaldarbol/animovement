#' Smooth Movement Data
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Applies smoothing filters to movement tracking data to reduce noise. The function
#' supports different smoothing methods and can operate on either positions directly
#' or their derivatives (frame-to-frame differences).
#'
#' @param data A data frame containing movement tracking data with the following
#'   required columns:
#'   - `individual`: Identifier for each tracked subject
#'   - `keypoint`: Identifier for each tracked point
#'   - `x`: x-coordinates
#'   - `y`: y-coordinates
#'   - `time`: Time values
#' @param method Character string specifying the smoothing method. Options:
#'   - `"rolling_median"`: Rolling median filter (default)
#'   - `"rolling_mean"`: Rolling mean filter
#' @param window_width Integer specifying how many observations to include in the
#'   rolling window (default: 5)
#' @param min_obs Integer specifying the minimum number of non-NA values required
#'   to calculate the rolling statistics (default: 1)
#' @param use_derivatives Logical. If TRUE, smoothing is applied to frame-to-frame
#'   differences rather than positions directly. Useful for trackball data. (default: FALSE)
#'
#' @return A data frame with the same structure as the input, but with smoothed
#'   x and y coordinates.
#'
#' @details
#' The function first validates the input data structure using `ensure_output_header_names()`
#' and `ensure_output_header_class()`. It then applies the selected smoothing method
#' separately to each individual and keypoint combination.
#'
#' When `use_derivatives = TRUE`, the function calls `smooth_derivatives()` which
#' applies smoothing to the differences between consecutive positions before
#' reconstructing the smoothed trajectory.
#'
#' @examples
#' \dontrun{
#' # Apply default rolling median smoothing
#' smooth_movement(tracking_data, window_width = 5)
#'
#' # Use rolling mean with a larger window
#' smooth_movement(tracking_data, method = "rolling_mean", window_width = 7)
#'
#' # Smooth derivatives instead of positions
#' smooth_movement(tracking_data, use_derivatives = TRUE)
#' }
#'
#' @seealso
#' - `smooth_derivatives()` for details on derivative-based smoothing
#' - `ensure_output_header_names()` and `ensure_output_header_class()` for data validation
#'
#' @export
#' @import dplyr
#' @importFrom roll roll_mean roll_median
smooth_movement <- function(
    data,
    method = c("rolling_median"),
    window_width = 5,
    min_obs = 1,
    use_derivatives = FALSE) {

  # Quick checks on the data
  ensure_output_header_names(data)
  ensure_output_header_class(data)


  data <- data |>
    dplyr::group_by(.data$individual, .data$keypoint)

  # Back-transform to dx and dy
  if (use_derivatives == TRUE){
    data <- smooth_derivatives(data, method, window_width, min_obs)
  }

  # Rolling mean
  else if (method == "rolling_mean"){
    data <- data |>
      dplyr::mutate(
        x = roll::roll_mean(.data$x, width = window_width, min_obs = min_obs),
        y = roll::roll_mean(.data$y, width = window_width, min_obs = min_obs)
      )
  }

  # Rolling median
  else if (method == "rolling_median") {
    data <- data |>
      dplyr::mutate(
        x = roll::roll_median(.data$x, width = window_width, min_obs = min_obs),
        y = roll::roll_median(.data$y, width = window_width, min_obs = min_obs)
      )
  }

  return(data)
}

#' Smooth derivatives (when original data measured differences between movements)
#'
#' @inheritParams smooth_movement
#'
#' @keywords internal
smooth_derivatives <- function(data, method, window_width, min_obs){

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
        dx = roll::roll_mean(.data$dx, width = window_width, min_obs = min_obs),
        dy = roll::roll_mean(.data$dy, width = window_width, min_obs = min_obs)
      )
  } else if (method == "rolling_median") {
    data <- data |>
      dplyr::mutate(
        dx = roll::roll_median(.data$dx, width = window_width, min_obs = min_obs),
        dy = roll::roll_median(.data$dy, width = window_width, min_obs = min_obs)
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
