#' Find optimal time lag between two time series using cross-correlation
#'
#' This function calculates the optimal lag between two time series by finding the
#' lag that maximizes their cross-correlation. It's particularly useful for
#' synchronizing recordings from different sources, such as physiological and
#' behavioral data.
#'
#' @param signal Time series to align (numeric vector)
#' @param reference Reference time series to align against (numeric vector)
#' @param max_lag Maximum lag to consider in both directions, in number of samples.
#'   If NULL, uses (length of series - 1)
#' @param normalize Logical; if TRUE, z-score normalizes both series before
#'   computing cross-correlation (recommended for series with different scales)
#'
#' @return Integer indicating the optimal lag. A positive value means the signal
#'   needs to be shifted forward in time to align with the reference. A negative
#'   value means the signal needs to be shifted backward.
#'
#' @examples
#' # Create two artificially shifted sine waves
#' t <- seq(0, 10, 0.1)
#' reference <- sin(t)
#' signal <- sin(t - 0.5)  # Signal delayed by 0.5 units
#' lag <- find_lag(signal, reference)
#' print(lag)  # Should be approximately 5 samples (0.5 units)
#'
#' @seealso \code{\link{align_timeseries}} for applying the computed lag
#'
#' @importFrom stats complete.cases ccf
#'
#' @export
find_lag <- function(signal, reference, max_lag = 5000, normalize = TRUE) {
  complete_cases <- stats::complete.cases(signal, reference)
  signal <- signal[complete_cases]
  reference <- reference[complete_cases]

  if (normalize) {
    signal <- as.vector(scale(signal))
    reference <- as.vector(scale(reference))
  }

  # Use length of series if max_lag not specified
  if (is.null(max_lag)) {
    max_lag = length(signal) - 1
  }

  ccf_result <- stats::ccf(signal, reference, plot = FALSE, lag.max = max_lag)
  best_lag <- ccf_result$lag[which.max(abs(ccf_result$acf))]

  # Subtract one observation, which seems to be needed in tests
  # best_lag <- best_lag - 1

  return(-best_lag)
}

#' Align a time series with a reference series using cross-correlation
#'
#' This function aligns two time series by shifting one series relative to the
#' reference based on their cross-correlation. It first finds the optimal lag
#' using \code{find_lag}, then applies the shift by padding with NA values
#' as needed.
#'
#' @inheritParams find_lag
#' @param signal Time series to align (numeric vector)
#' @param reference Reference time series to align against (numeric vector)
#'
#' @return A numeric vector of the same length as the input signal, shifted to
#'   align with the reference series. NA values are used to pad the beginning or
#'   end depending on the direction of the shift.
#'
#' @examples
#' # Create two artificially shifted sine waves
#' t <- seq(0, 10, 0.1)
#' reference <- sin(t)
#' signal <- sin(t - 0.5)  # Signal delayed by 0.5 units
#'
#' # Align the delayed signal with the reference
#' aligned <- align_timeseries(signal, reference)
#'
#' # Plot to verify alignment
#' plot(t, reference, type = "l", col = "black")
#' lines(t, aligned, col = "red", lty = 2)
#'
#' @export
align_timeseries <- function(signal, reference, max_lag = 5000, normalize = TRUE) {
  lag <- find_lag(signal, reference, max_lag, normalize)

  if (lag > 0) {
    aligned <- c(rep(NA, lag), signal[1:(length(signal)-lag)])
  } else if (lag < 0) {
    aligned <- c(signal[(-lag+1):length(signal)], rep(NA, -lag))
  } else {
    aligned <- signal
  }

  return(aligned)
}
