#' Apply Savitzky-Golay Filter to Movement Data
#'
#' This function applies a Savitzky-Golay filter to smooth movement data while preserving
#' higher moments (peaks, valleys) better than moving average filters. The implementation
#' uses zero-phase filtering to prevent temporal shifts in the data.
#'
#' @param x Numeric vector containing the movement data to be filtered
#' @param sampling_rate Sampling rate of the data in Hz. Must match your data
#'        collection rate (e.g., 60 for 60 FPS motion capture).
#' @param window_size Window size in samples (must be odd). Controls the amount of
#'        smoothing. Larger windows give more smoothing but may over-attenuate
#'        genuine movement features. Default is automatically calculated as
#'        sampling_rate/10 (rounded up to nearest odd number).
#' @param order Polynomial order (default = 3). Controls how well the filter preserves
#'        higher-order moments in the data:
#'        - order=2: Preserves position, velocity (good for smooth movements)
#'        - order=3: Also preserves acceleration (good for most movement data)
#'        - order=4: Also preserves jerk (good for quick movements)
#'        - order=5: Maximum preservation (may retain too much noise)
#' @param preserve_edges Logical indicating whether to use progressively smaller windows
#'        at the beginning and end of the signal to reduce edge effects (default = FALSE).
#'        Note: This only affects the signal endpoints, not internal discontinuities.
#' @param na_action Method to handle NA values before filtering. One of:
#'        - "linear": Linear interpolation (default)
#'        - "spline": Spline interpolation for smoother curves
#'        - "locf": Last observation carried forward
#'        - "value": Replace with a constant value
#'        - "error": Raise an error if NAs are present
#' @param keep_na Logical indicating whether to restore NAs to their original positions
#'        after filtering (default = FALSE)
#' @param ... Additional arguments passed to replace_na()
#'
#' @details
#' The Savitzky-Golay filter fits successive polynomials to sliding windows of the data.
#' This approach preserves higher moments of the data better than simple moving averages
#' or Butterworth filters, making it particularly suitable for movement data where
#' preserving features like peaks and valleys is important.
#'
#' Edge Handling:
#' When preserve_edges = TRUE, the function uses progressively smaller windows near the
#' beginning and end of the signal to reduce endpoint distortion. This only affects
#' the signal endpoints - it does not detect or handle internal discontinuities or
#' sharp events within the data.
#'
#' Parameter Selection Guidelines:
#' * window_size:
#'   - For 60 FPS: 5-15 frames (83-250ms) for quick movements, 15-31 for slow movements
#'   - For 120 FPS: 7-21 frames (58-175ms) for quick movements, 21-51 for slow movements
#'   - For 500 FPS: 25-75 frames (50-150ms) for quick movements, 75-151 for slow movements
#'   The default window_size = sampling_rate/10 works well for typical human movement.
#'
#' * order:
#'   - order=2: Smooth movements, position analysis
#'   - order=3: Most movement analysis (default)
#'   - order=4: Quick movements, sports analysis
#'   - order=5: Very quick movements, impact analysis
#'   Note: order must be less than window_size
#'
#' Common values by application:
#' * Gait analysis (60 FPS): window_size=15, order=3
#' * Sports biomechanics (120 FPS): window_size=21, order=4
#' * Impact analysis (500 FPS): window_size=51, order=4
#' * Posture analysis (60 FPS): window_size=31, order=2
#'
#' @return Numeric vector containing the filtered movement data
#'
#' @examples
#' # Generate example movement data: smooth motion + noise
#' t <- seq(0, 5, by = 1/60)  # 60 FPS data
#' x <- sin(2*pi*0.5*t) + rnorm(length(t), 0, 0.1)
#'
#' # Basic filtering with default parameters (60 FPS)
#' filtered <- filter_sgolay(x, sampling_rate = 60)
#'
#' # Adjusting parameters for quick movements
#' filtered_quick <- filter_sgolay(x, sampling_rate = 60,
#'                                window_size = 11, order = 4)
#'
#' # High-speed camera data (500 FPS) with larger window
#' filtered_high <- filter_sgolay(x, sampling_rate = 500,
#'                               window_size = 51, order = 3)
#'
#' @seealso
#' \code{\link{filter_lowpass}} for frequency-based filtering
#' \code{\link{sgolayfilt}} for the base Savitzky-Golay implementation
#' \code{\link{replace_na}} for details on NA handling methods
#'
#' @references
#' Savitzky, A., & Golay, M.J.E. (1964). Smoothing and Differentiation of Data by
#' Simplified Least Squares Procedures. Analytical Chemistry, 36(8), 1627-1639.
#'
#' @importFrom signal sgolayfilt
#' @importFrom cli cli_abort cli_warn
#'
#' @export
filter_sgolay <- function(
  x,
  sampling_rate,
  window_size = ceiling(sampling_rate / 10) * 2 + 1,
  order = 3,
  preserve_edges = FALSE,
  na_action = "linear",
  keep_na = FALSE,
  ...
) {
  # Input validation
  if (!is.numeric(x)) {
    cli::cli_abort("Data must be numeric")
  }
  if (!is.numeric(sampling_rate) || sampling_rate <= 0) {
    cli::cli_abort("Sampling rate must be a positive number")
  }
  if (window_size %% 2 != 1) {
    cli::cli_abort("Window size must be odd")
  }
  if (window_size > length(x)) {
    cli::cli_abort("Window size cannot be larger than data length")
  }
  if (order >= window_size) {
    cli::cli_abort("Polynomial order must be less than window size")
  }

  # Handle NAs
  na_positions <- is.na(x)
  if (any(na_positions)) {
    if (na_action == "error") {
      cli::cli_abort("NA values present in data")
    }
    x <- replace_na(x, method = na_action, ...)
  }

  if (preserve_edges) {
    # Handle edges with progressively smaller windows
    result <- x
    half_window <- (window_size - 1) / 2

    # Center portion
    result[(half_window + 1):(length(x) - half_window)] <-
      signal::sgolayfilt(x, p = order, n = window_size)[
        (half_window + 1):(length(x) - half_window)
      ]

    # Edges
    for (i in 1:half_window) {
      edge_window <- 2 * i + 1
      edge_order <- min(order, max(2, i - 1))

      # Left edge
      if (i >= 3) {
        # Need at least 5 points for meaningful smoothing
        result[i] <- signal::sgolayfilt(
          x[1:(2 * i + 1)],
          p = edge_order,
          n = edge_window
        )[i]
      }

      # Right edge
      if (i >= 3) {
        right_idx <- length(x) - i + 1
        result[right_idx] <- signal::sgolayfilt(
          x[(length(x) - 2 * i):length(x)],
          p = edge_order,
          n = edge_window
        )[i + 1]
      }
    }
  } else {
    # Standard Savitzky-Golay filter
    result <- signal::sgolayfilt(x, p = order, n = window_size)
  }

  # Restore NAs if requested
  if (keep_na && any(na_positions)) {
    result[na_positions] <- NA
  }

  return(result)
}
