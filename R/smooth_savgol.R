#' Smooth Movement Data Using Savitzky-Golay Filter
#'
#' @description
#' Applies a Savitzky-Golay smoothing filter to x and y coordinates in movement data.
#' Adapted from the `TrajSmoothSG` function in the trajr package (McLean & Skowron Volponi, MIT License).
#'
#' @param data A data frame containing movement data with the following required columns:
#'   - `individual`: Identifier for each tracked subject
#'   - `keypoint`: Identifier for each tracked point
#'   - `x`: x-coordinates
#'   - `y`: y-coordinates
#' @param p Integer specifying the polynomial order for the filter (default: 3)
#' @param n Integer specifying the filter window size. Must be odd and less than
#'   the number of observations (default: p + 3 - p%%2)
#' @param ... Additional arguments passed to signal::sgolayfilt()
#'
#' @return A data frame with the same structure as the input, but with smoothed
#'   x and y coordinates.
#'
#' @details
#' The Savitzky-Golay filter smooths data by fitting successive sub-sets of adjacent data
#' points with a low-degree polynomial using linear least squares regression. This method
#' is particularly effective at preserving higher moments of the data while reducing noise.
#'
#' When working with data that contains temporal gaps, consider whether these gaps need
#' to be filled before smoothing, especially if the smoothed trajectories will be used
#' to calculate speed or acceleration.
#'
#' @examples
#' \dontrun{
#' # Basic usage with default parameters
#' smooth_by_savgol(movement_data)
#'
#' # Specify polynomial order and window size
#' smooth_by_savgol(movement_data, p = 3, n = 31)
#'
#' # Using a larger window for more smoothing
#' smooth_by_savgol(movement_data, p = 3, n = 51)
#' }
#'
#' @seealso
#' - signal::sgolayfilt() for details on the Savitzky-Golay filter implementation
#' - Original trajr package: McLean & Skowron Volponi (2018)
#'
#' @importFrom dplyr group_by mutate
#' @importFrom signal sgolayfilt
#' @importFrom cli cli_abort
#'
#' @export
smooth_by_savgol <- function(data, p = 3, n = p + 3 - p%%2, ...) {
  if (n %% 2 != 1)
    cli::cli_abort("Invalid smoothing parameter n ({n}): n must be odd")
  if (n > nrow(data))
    cli::cli_abort("Invalid smoothing parameter n ({n}): n must be less than the number of points in the trajectory ({nrow(data)})")
  data <- data |>
    dplyr::group_by(.data$individual, .data$keypoint) |>
    dplyr::mutate(x = signal::sgolayfilt(.data$x, p, n, ...),
                  y = signal::sgolayfilt(.data$y, p, n, ...))
  return(data)
}
