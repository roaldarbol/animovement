#' Smooth Movement Data
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Applies smoothing filters to movement tracking data to reduce noise.
#'
#' @param data A data frame containing movement tracking data with the following
#'   required columns:
#'   - `individual`: Identifier for each tracked subject
#'   - `keypoint`: Identifier for each tracked point
#'   - `x`: x-coordinates
#'   - `y`: y-coordinates
#'   - `time`: Time values
#'   Optional columns:
#'   - `z`: z-coordinates
#' @param method Character string specifying the smoothing method. Options:
#'   - `"kalman"`: Kalman filter (see [filter_kalman()])
#'   - `"sgolay"`: Savitzky-Golay filter (see [filter_sgolay()])
#'   - `"lowpass"`: Low-pass filter (see [filter_lowpass()])
#'   - `"highpass"`: High-pass filter (see [filter_highpass()])
#'   - `"lowpass_fft"`: FFT-based low-pass filter (see [filter_lowpass_fft()])
#'   - `"highpass_fft"`: FFT-based high-pass filter (see [filter_highpass_fft()])
#'   - `"rollmean"`: Rolling mean filter (see [filter_rollmean()])
#'   - `"rollmedian"`: Rolling median filter (see [filter_rollmedian()])
#' @param use_derivatives Filter on the derivative values instead of coordinates
#'   (important for e.g. trackball or accelerometer data)
#' @param ... Additional arguments passed to the specific filter function
#'
#' @details
#' This function is a wrapper that applies various filtering methods to x and y
#' (and z if present) coordinates. Each filtering method has its own specific
#' parameters - see the documentation of individual filter functions for details:
#'
#' * [filter_kalman()]: Kalman filter parameters
#' * [filter_sgolay()]: Savitzky-Golay filter parameters
#' * [filter_lowpass()]: Low-pass filter parameters
#' * [filter_highpass()]: High-pass filter parameters
#' * [filter_lowpass_fft()]: FFT-based low-pass filter parameters
#' * [filter_highpass_fft()]: FFT-based high-pass filter parameters
#' * [filter_rollmean()]: Rolling mean parameters (window_width, min_obs)
#' * [filter_rollmedian()]: Rolling median parameters (window_width, min_obs)
#'
#' @return A data frame with the same structure as the input, but with smoothed
#'   coordinates.
#'
#' @examples
#' \dontrun{
#' # Apply rolling median with window of 5
#' filter_movement(tracking_data, "rollmedian", window_width = 5, min_obs = 1)
#' }
#'
#' @seealso
#' * [filter_kalman()]
#' * [filter_sgolay()]
#' * [filter_lowpass()]
#' * [filter_highpass()]
#' * [filter_lowpass_fft()]
#' * [filter_highpass_fft()]
#' * [filter_rollmean()]
#' * [filter_rollmedian()]
#'
#' @export
#' @import dplyr
filter_movement <- function(
    data,
    method = c("rollmedian", "rollmean", "kalman", "sgolay", "lowpass",
               "highpass", "lowpass_fft", "highpass_fft"),
    use_derivatives = FALSE,
    ...) {

  method <- match.arg(method)

  # Input validation
  ensure_output_header_names(data)
  ensure_output_header_class(data)

  # Select appropriate filter function
  filter_fn <- switch(method,
                      rollmean = filter_rollmean,
                      rollmedian = filter_rollmedian,
                      kalman = filter_kalman,
                      sgolay = filter_sgolay,
                      lowpass = filter_lowpass,
                      highpass = filter_highpass,
                      lowpass_fft = filter_lowpass_fft,
                      highpass_fft = filter_highpass_fft,
                      cli::cli_abort("Invalid method: {method}")
  )

  if (isFALSE(use_derivatives)){
    # Apply filter to coordinates
    data <- data |>
      dplyr::group_by(.data$individual, .data$keypoint) |>
      dplyr::mutate(
        x = filter_fn(.data$x, ...),
        y = filter_fn(.data$y, ...)
      )

    # If z coordinate exists, filter it too
    if ("z" %in% names(data)) {
      data <- data |>
        dplyr::mutate(
          z = filter_fn(.data$z, ...)
        )
    }
  } else {
    # Apply filter to derivatives
    data <- data |>
      dplyr::group_by(.data$individual, .data$keypoint) |>
      dplyr::mutate(
        dx = .data$x - dplyr::lag(.data$x),
        dy = .data$y - dplyr::lag(.data$y)
      ) |>
      dplyr::mutate(
        dx = filter_fn(.data$dx, ...),
        dy = filter_fn(.data$dy, ...)
      ) |>
      dplyr::mutate(
        x = cumsum(dplyr::coalesce(.data$dx, 0)) + .data$dx * 0,
        y = cumsum(dplyr::coalesce(.data$dy, 0)) + .data$dy * 0
      ) |>
      dplyr::select(-"dx", -"dy")

    # If z coordinate exists, filter it too
    if ("z" %in% names(data)) {
      data <- data |>
        dplyr::mutate(
          dz = .data$z - dplyr::lag(.data$z)
        ) |>
        dplyr::mutate(
          dz = filter_fn(.data$dz, ...)
        ) |>
        dplyr::mutate(
          z = cumsum(dplyr::coalesce(.data$dz, 0)) + .data$dz * 0
        ) |>
        dplyr::select(-"dz")
    }
  }

  data
}
