#' Smooth a trajectory using a Savitzky-Golay filter
#'
#' Smooths a trajectory using a Savitzky-Golay smoothing filter.
#'
#' Consider carefully the effects of smoothing a trajectory with temporal gaps
#' in the data. If the smoothed trajectory is used to derive speed and/or
#' acceleration, it may be advisable to fill in the gaps before smoothing,
#' possibly by calling \code{TrajResampleTime}.
#'
#' @param data a movement data frame
#' @param p polynomial order (passed to \code{\link[signal]{sgolayfilt}}).
#' @param n Filter length (or window size), must be an odd number.  Passed to
#'   \code{\link[signal]{sgolayfilt}}.
#' @param ... Additional arguments are passed to
#'   \code{\link[signal]{sgolayfilt}}.
#' @return A new trajectory which is a smoothed version of the input trajectory.
#'
#' @seealso \code{\link[signal]{sgolayfilt}}
#' @examples
#' set.seed(3)
#' trj <- TrajGenerate(500, random = TRUE, angularErrorSd = .25)
#' smoothed <- TrajSmoothSG(trj, 3, 31)
#' plot(trj)
#' plot(smoothed, col = "red", add = TRUE)
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
