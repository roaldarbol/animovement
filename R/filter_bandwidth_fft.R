#' Apply FFT-based Lowpass Filter to Signal
#'
#' This function implements a lowpass filter using the Fast Fourier Transform (FFT).
#' It provides a sharp frequency cutoff but may introduce ringing artifacts (Gibbs phenomenon).
#'
#' @param x Numeric vector containing the signal to be filtered
#' @param cutoff_freq Cutoff frequency in Hz. Frequencies below this value are passed,
#'        while frequencies above are attenuated. Should be between 0 and sampling_rate/2.
#' @param sampling_rate Sampling rate of the signal in Hz. Must be at
#'        least twice the highest frequency component in the signal (Nyquist criterion).
#' @param na_action Method to handle NA values before filtering. One of:
#'        - "linear": Linear interpolation (default)
#'        - "spline": Spline interpolation for smoother curves
#'        - "stine": Stineman interpolation preserving data shape
#'        - "locf": Last observation carried forward
#'        - "value": Replace with a constant value
#'        - "error": Raise an error if NAs are present
#' @param keep_na Logical indicating whether to restore NAs to their original positions
#'        after filtering (default = FALSE)
#' @param ... Additional arguments passed to replace_na(). Common options include:
#'        - value: Numeric value for replacement when na_action = "value"
#'        - min_gap: Minimum gap size to interpolate/fill
#'        - max_gap: Maximum gap size to interpolate/fill
#'
#' @details
#' FFT-based filtering applies a hard cutoff in the frequency domain. This can be
#' advantageous for:
#' * Precise frequency selection
#' * Batch processing of long signals
#' * Cases where sharp frequency cutoffs are desired
#'
#' Limitations:
#' * May introduce ringing artifacts
#' * Assumes periodic signal (can cause edge effects)
#' * Less suitable for real-time processing
#'
#' Missing Value Handling:
#' The function uses replace_na() internally for handling missing values. See ?replace_na
#' for detailed information about each method and its parameters. NAs can optionally be
#' restored to their original positions after filtering using keep_na = TRUE.
#'
#' @return Numeric vector containing the filtered signal
#'
#' @examples
#' # Generate example signal with mixed frequencies
#' t <- seq(0, 1, by = 0.001)
#' x <- sin(2*pi*2*t) + sin(2*pi*50*t)
#'
#' # Add some NAs
#' x[sample(length(x), 10)] <- NA
#'
#' # Basic filtering with linear interpolation for NAs
#' filtered <- filter_lowpass_fft(x, cutoff_freq = 5, sampling_rate = 1000)
#'
#' # Using spline interpolation with max gap constraint
#' filtered <- filter_lowpass_fft(x, cutoff_freq = 5, sampling_rate = 1000,
#'                               na_action = "spline", max_gap = 3)
#'
#' # Replace NAs with zeros before filtering
#' filtered <- filter_lowpass_fft(x, cutoff_freq = 5, sampling_rate = 1000,
#'                               na_action = "value", value = 0)
#'
#' # Filter but keep NAs in their original positions
#' filtered <- filter_lowpass_fft(x, cutoff_freq = 5, sampling_rate = 1000,
#'                               na_action = "linear", keep_na = TRUE)
#'
#' # Compare with Butterworth filter
#' butter_filtered <- filter_lowpass(x, 5, 1000)
#'
#' @seealso
#' \code{\link{replace_na}} for details on NA handling methods
#' \code{\link{filter_highpass_fft}} for FFT-based high-pass filtering
#' \code{\link{filter_lowpass}} for Butterworth-based filtering
#'
#' @importFrom stats fft
#'
#' @export
filter_lowpass_fft <- function(
  x,
  cutoff_freq,
  sampling_rate,
  na_action = c("linear", "spline", "stine", "locf", "value", "error"),
  keep_na = FALSE,
  ...
) {
  # Input validation
  if (!is.numeric(x)) {
    cli::cli_abort("Input signal must be numeric")
  }
  if (cutoff_freq <= 0 || cutoff_freq >= sampling_rate / 2) {
    cli::cli_abort("Cutoff frequency must be between 0 and sampling_rate/2")
  }

  na_action <- match.arg(na_action)

  # Store original NA positions if needed
  na_positions <- if (keep_na) which(is.na(x))

  # Handle NAs
  if (any(is.na(x))) {
    if (na_action == "error") {
      cli::cli_abort("Signal contains NA values")
    } else {
      x <- replace_na(x, method = na_action, ...)
    }
  }

  N <- length(x)

  # Add reflection padding to reduce edge effects
  n_pad <- ceiling(N / 10) # 10% padding
  start_pad <- rev(x[1:n_pad])
  end_pad <- rev(x[(length(x) - n_pad + 1):length(x)])
  x_padded <- c(start_pad, x, end_pad)

  # Compute FFT
  X <- stats::fft(x_padded)
  N_padded <- length(x_padded)

  # Create frequency vector
  freq <- seq(
    0,
    sampling_rate - sampling_rate / N_padded,
    by = sampling_rate / N_padded
  )

  # Create filter mask
  mask <- freq <= cutoff_freq
  mask[N_padded:2] <- rev(mask[2:(N_padded / 2 + 1)]) # Mirror for negative frequencies

  # Apply filter
  X_filtered <- X * mask

  # Inverse FFT
  filtered_padded <- Re(stats::fft(X_filtered, inverse = TRUE) / N_padded)

  # Remove padding
  filtered <- filtered_padded[(n_pad + 1):(n_pad + N)]

  # Restore NAs if requested
  if (keep_na && length(na_positions) > 0) {
    filtered[na_positions] <- NA
  }

  return(filtered)
}

#' Apply FFT-based Highpass Filter to Signal
#'
#' This function implements a highpass filter using the Fast Fourier Transform (FFT).
#' It provides a sharp frequency cutoff but may introduce ringing artifacts (Gibbs phenomenon).
#'
#' @param x Numeric vector containing the signal to be filtered
#' @param cutoff_freq Cutoff frequency in Hz. Frequencies above this value are passed,
#'        while frequencies below are attenuated. Should be between 0 and sampling_rate/2.
#' @param sampling_rate Sampling rate of the signal in Hz. Must be at
#'        least twice the highest frequency component in the signal (Nyquist criterion).
#' @param na_action Method to handle NA values before filtering. One of:
#'        - "linear": Linear interpolation (default)
#'        - "spline": Spline interpolation for smoother curves
#'        - "stine": Stineman interpolation preserving data shape
#'        - "locf": Last observation carried forward
#'        - "value": Replace with a constant value
#'        - "error": Raise an error if NAs are present
#' @param keep_na Logical indicating whether to restore NAs to their original positions
#'        after filtering (default = FALSE)
#' @param ... Additional arguments passed to replace_na(). Common options include:
#'        - value: Numeric value for replacement when na_action = "value"
#'        - min_gap: Minimum gap size to interpolate/fill
#'        - max_gap: Maximum gap size to interpolate/fill
#'
#' @details
#' FFT-based filtering applies a hard cutoff in the frequency domain. This can be
#' advantageous for:
#' * Precise frequency selection
#' * Batch processing of long signals
#' * Cases where sharp frequency cutoffs are desired
#'
#' Common Applications:
#' * Removing baseline drift: Use low cutoff (0.1-1 Hz)
#' * EMG analysis: Use moderate cutoff (10-20 Hz)
#' * Motion artifact removal: Use application-specific cutoff
#'
#' Limitations:
#' * May introduce ringing artifacts
#' * Assumes periodic signal (can cause edge effects)
#' * Less suitable for real-time processing
#'
#' Missing Value Handling:
#' The function uses replace_na() internally for handling missing values. See ?replace_na
#' for detailed information about each method and its parameters. NAs can optionally be
#' restored to their original positions after filtering using keep_na = TRUE.
#'
#' @return Numeric vector containing the filtered signal
#'
#' @examples
#' # Generate example signal with drift
#' t <- seq(0, 1, by = 0.001)
#' drift <- 0.5 * t  # Linear drift
#' signal <- sin(2*pi*10*t)  # 10 Hz signal
#' x <- signal + drift
#'
#' # Add some NAs
#' x[sample(length(x), 10)] <- NA
#'
#' # Basic filtering with linear interpolation for NAs
#' filtered <- filter_highpass_fft(x, cutoff_freq = 2, sampling_rate = 1000)
#'
#' # Using spline interpolation with max gap constraint
#' filtered <- filter_highpass_fft(x, cutoff_freq = 2, sampling_rate = 1000,
#'                                na_action = "spline", max_gap = 3)
#'
#' # Replace NAs with zeros before filtering
#' filtered <- filter_highpass_fft(x, cutoff_freq = 2, sampling_rate = 1000,
#'                                na_action = "value", value = 0)
#'
#' # Filter but keep NAs in their original positions
#' filtered <- filter_highpass_fft(x, cutoff_freq = 2, sampling_rate = 1000,
#'                                na_action = "linear", keep_na = TRUE)
#'
#' # Compare with Butterworth filter
#' butter_filtered <- filter_highpass(x, 2, 1000)
#'
#' @seealso
#' \code{\link{replace_na}} for details on NA handling methods
#' \code{\link{filter_lowpass_fft}} for FFT-based low-pass filtering
#' \code{\link{filter_highpass}} for Butterworth-based filtering
#'
#' @importFrom stats fft
#'
#' @export
filter_highpass_fft <- function(
  x,
  cutoff_freq,
  sampling_rate,
  na_action = c("linear", "spline", "stine", "locf", "value", "error"),
  keep_na = FALSE,
  ...
) {
  # Input validation
  if (!is.numeric(x)) {
    cli::cli_abort("Input signal must be numeric")
  }
  if (cutoff_freq <= 0 || cutoff_freq >= sampling_rate / 2) {
    cli::cli_abort("Cutoff frequency must be between 0 and sampling_rate/2")
  }

  na_action <- match.arg(na_action)

  # Store original NA positions if needed
  na_positions <- if (keep_na) which(is.na(x))

  # Handle NAs
  if (any(is.na(x))) {
    if (na_action == "error") {
      cli::cli_abort("Signal contains NA values")
    } else {
      x <- replace_na(x, method = na_action, ...)
    }
  }

  N <- length(x)

  # Add reflection padding to reduce edge effects
  n_pad <- ceiling(N / 10) # 10% padding
  start_pad <- rev(x[1:n_pad])
  end_pad <- rev(x[(length(x) - n_pad + 1):length(x)])
  x_padded <- c(start_pad, x, end_pad)

  # Compute FFT
  X <- stats::fft(x_padded)
  N_padded <- length(x_padded)

  # Create frequency vector
  freq <- seq(
    0,
    sampling_rate - sampling_rate / N_padded,
    by = sampling_rate / N_padded
  )

  # Create filter mask
  mask <- freq >= cutoff_freq
  mask[N_padded:2] <- rev(mask[2:(N_padded / 2 + 1)]) # Mirror for negative frequencies

  # Apply filter
  X_filtered <- X * mask

  # Inverse FFT
  filtered_padded <- Re(stats::fft(X_filtered, inverse = TRUE) / N_padded)

  # Remove padding
  filtered <- filtered_padded[(n_pad + 1):(n_pad + N)]

  # Restore NAs if requested
  if (keep_na && length(na_positions) > 0) {
    filtered[na_positions] <- NA
  }

  return(filtered)
}
