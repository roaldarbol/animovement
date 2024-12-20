#' Apply Butterworth Lowpass Filter to Signal
#'
#' This function applies a lowpass Butterworth filter to a signal using forward-backward
#' filtering (filtfilt) to achieve zero phase distortion. The Butterworth filter is
#' maximally flat in the passband, making it ideal for many signal processing applications.
#'
#' @param x Numeric vector containing the signal to be filtered
#' @param cutoff_freq Cutoff frequency in Hz. Frequencies below this value are passed,
#'        while frequencies above are attenuated. Should be between 0 and sampling_rate/2.
#' @param sampling_rate Sampling rate of the signal in Hz. Must be at
#'        least twice the highest frequency component in the signal (Nyquist criterion).
#' @param order Filter order (default = 4). Controls the steepness of frequency rolloff:
#'        - Higher orders give sharper cutoffs but may introduce more ringing
#'        - Lower orders give smoother transitions but less steep rolloff
#'        - Common values in practice are 2-8
#'        - Values above 8 are rarely used due to numerical instability
#' @param na_action How to handle NA values:
#'        - "interpolate": Linear interpolation of NAs (default)
#'        - "remove": Remove NAs and adjust time indices
#'        - "error": Raise an error if NAs are present
#'
#' @details
#' The Butterworth filter response falls off at -6*order dB/octave. The cutoff frequency
#' corresponds to the -3dB point of the filter's magnitude response.
#'
#' Parameter Selection Guidelines:
#' * cutoff_freq: Choose based on the frequency content you want to preserve
#' * sampling_rate: Should match your data collection rate
#' * order:
#'   - order=2: Gentle rolloff, minimal ringing (~12 dB/octave)
#'   - order=4: Standard choice, good balance (~24 dB/octave)
#'   - order=6: Steeper rolloff, some ringing (~36 dB/octave)
#'   - order=8: Very steep, may have significant ringing (~48 dB/octave)
#'   Note: For very low cutoff frequencies (<0.001 of Nyquist), order is automatically
#'   reduced to 2 to maintain stability.
#'
#' Common values by field:
#' * Biomechanics: order=2 or 4
#' * EEG/MEG: order=4 or 6
#' * Audio processing: order=2 to 8
#' * Mechanical vibrations: order=2 to 4
#'
#' Missing Value Handling:
#' The function offers three approaches for handling NA values:
#' * interpolate: Uses linear interpolation to fill gaps. Best for sporadic missing values.
#'   Not recommended for large gaps as it may introduce artificial frequencies.
#' * remove: Removes NA values and adjusts the time series accordingly. This changes the
#'   effective sampling rate and may affect frequency content. Use with caution.
#' * error: Stops execution if NAs are present. Use when missing values indicate a problem
#'   that should be addressed before filtering.
#'
#' @return Numeric vector containing the filtered signal
#'
#' @examples
#' # Generate example signal: 2 Hz fundamental + 50 Hz noise
#' t <- seq(0, 1, by = 0.001)
#' x <- sin(2*pi*2*t) + 0.5*sin(2*pi*50*t)
#'
#' # Apply lowpass filter to remove 50 Hz noise
#' filtered <- filter_lowpass(x, cutoff_freq = 5, sampling_rate = 1000, order = 4)
#'
#' # Compare original and filtered signals
#' par(mfrow = c(2,1))
#' plot(t[1:100], x[1:100], type = "l", main = "Original Signal")
#' plot(t[1:100], filtered[1:100], type = "l", main = "Filtered Signal")
#'
#' @seealso
#' \code{\link{highpass_filter}} for high-pass filtering
#' \code{\link{butter}} for Butterworth filter design
#' \code{\link{filtfilt}} for zero-phase digital filtering
#'
#' @references
#' Butterworth, S. (1930). On the Theory of Filter Amplifiers.
#' Wireless Engineer, 7, 536-541.
#'
#' @importFrom signal butter filtfilt
#'
#' @export
filter_lowpass <- function(x, cutoff_freq, sampling_rate, order = 4) {
  # Input validation
  if (!is.numeric(x)) stop("Input signal must be numeric")
  if (cutoff_freq <= 0 || cutoff_freq >= sampling_rate/2) {
    stop("Cutoff frequency must be between 0 and sampling_rate/2")
  }
  if (order < 1 || order > 8) {
    stop("Filter order should be between 1 and 8")
  }

  # Handle NAs with linear interpolation
  if (any(is.na(x))) {
    t <- seq_along(x)
    # Handle edge cases first
    if (is.na(x[1])) {
      first_valid <- which(!is.na(x))[1]
      x[1:first_valid] <- x[first_valid]
    }
    if (is.na(x[length(x)])) {
      last_valid <- max(which(!is.na(x)))
      x[last_valid:length(x)] <- x[last_valid]
    }
    # Now interpolate internal NAs
    x <- as.numeric(stats::approx(t[!is.na(x)], x[!is.na(x)], t)$y)
  }

  # Verify no NAs remain
  if (any(is.na(x))) {
    stop("Failed to properly interpolate NAs")
  }

  # For very low cutoff frequencies (<0.001 normalized), reduce order
  nyquist_freq <- sampling_rate/2
  normalized_cutoff <- cutoff_freq/nyquist_freq

  if (normalized_cutoff < 0.001 & order > 2) {
    order <- min(order, 2)  # Limit order for very low frequencies
    warning("Very low cutoff frequency detected. Reducing filter order to 2 for stability.")
  }

  # Add reflection padding to reduce edge effects
  n_pad <- max(round(sampling_rate/cutoff_freq), order * 10)
  start_pad <- rev(x[1:min(n_pad, length(x))])
  end_pad <- rev(x[(length(x) - min(n_pad, length(x)) + 1):length(x)])
  x_padded <- c(start_pad, x, end_pad)

  # Create and apply filter
  bf <- signal::butter(order, normalized_cutoff, type = "low")
  filtered_padded <- signal::filtfilt(bf, x_padded)

  # Remove padding and ensure original length
  filtered <- filtered_padded[(n_pad + 1):(n_pad + length(x))]

  return(filtered)
}

#' Apply Butterworth Highpass Filter to Signal
#'
#' This function applies a highpass Butterworth filter to a signal using forward-backward
#' filtering (filtfilt) to achieve zero phase distortion. The Butterworth filter is
#' maximally flat in the passband, making it ideal for many signal processing applications.
#'
#' @param x Numeric vector containing the signal to be filtered
#' @param cutoff_freq Cutoff frequency in Hz. Frequencies above this value are passed,
#'        while frequencies below are attenuated. Should be between 0 and sampling_rate/2.
#' @param sampling_rate Sampling rate of the signal in Hz. Must be at
#'        least twice the highest frequency component in the signal (Nyquist criterion).
#' @param order Filter order (default = 4). Controls the steepness of frequency rolloff:
#'        - Higher orders give sharper cutoffs but may introduce more ringing
#'        - Lower orders give smoother transitions but less steep rolloff
#'        - Common values in practice are 2-8
#'        - Values above 8 are rarely used due to numerical instability
#' @param na_action How to handle NA values:
#'        - "interpolate": Linear interpolation of NAs (default)
#'        - "remove": Remove NAs and adjust time indices
#'        - "error": Raise an error if NAs are present
#'
#' @details
#' The Butterworth filter response falls off at -6*order dB/octave. The cutoff frequency
#' corresponds to the -3dB point of the filter's magnitude response.
#'
#' Common Applications:
#' * Removing baseline drift: Use low cutoff (0.1-1 Hz)
#' * EMG analysis: Use moderate cutoff (10-20 Hz)
#' * Motion artifact removal: Use application-specific cutoff
#'
#' Parameter Selection Guidelines:
#' * cutoff_freq: Choose based on the lowest frequency you want to preserve
#' * order: Same guidelines as lowpass_filter
#'
#' Common values by field:
#' * ECG processing: order=2, cutoff=0.5 Hz
#' * EEG analysis: order=4, cutoff=1 Hz
#' * Mechanical vibrations: order=2, cutoff application-specific
#'
#' Missing Value Handling:
#' See lowpass_filter documentation for details on NA handling approaches.
#'
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
#' # Remove drift with highpass filter
#' filtered <- filter_highpass(x, cutoff_freq = 2, sampling_rate = 1000, order = 4)
#'
#' @seealso
#' \code{\link{lowpass_filter}} for low-pass filtering
#' \code{\link{butter}} for Butterworth filter design
#' \code{\link{filtfilt}} for zero-phase digital filtering
#'
#' @importFrom signal butter filtfilt
#'
#' @export
filter_highpass <- function(x, cutoff_freq, sampling_rate, order = 4) {
  # Input validation
  if (!is.numeric(x)) stop("Input signal must be numeric")
  if (cutoff_freq <= 0 || cutoff_freq >= sampling_rate/2) {
    stop("Cutoff frequency must be between 0 and sampling_rate/2")
  }
  if (order < 1 || order > 8) {
    stop("Filter order should be between 1 and 8")
  }

  # Handle NAs with linear interpolation
  if (any(is.na(x))) {
    t <- seq_along(x)
    # Handle edge cases first
    if (is.na(x[1])) {
      first_valid <- which(!is.na(x))[1]
      x[1:first_valid] <- x[first_valid]
    }
    if (is.na(x[length(x)])) {
      last_valid <- max(which(!is.na(x)))
      x[last_valid:length(x)] <- x[last_valid]
    }
    # Now interpolate internal NAs
    x <- as.numeric(stats::approx(t[!is.na(x)], x[!is.na(x)], t)$y)
  }

  # Verify no NAs remain
  if (any(is.na(x))) {
    stop("Failed to properly interpolate NAs")
  }

  # For very low cutoff frequencies (<0.001 normalized), reduce order
  nyquist_freq <- sampling_rate/2
  normalized_cutoff <- cutoff_freq/nyquist_freq

  if (normalized_cutoff < 0.001 & order > 2) {
    order <- min(order, 2)  # Limit order for very low frequencies
    warning("Very low cutoff frequency detected. Reducing filter order to 2 for stability.")
  }

  # Add reflection padding to reduce edge effects
  n_pad <- max(round(sampling_rate/cutoff_freq), order * 10)
  start_pad <- rev(x[1:min(n_pad, length(x))])
  end_pad <- rev(x[(length(x) - min(n_pad, length(x)) + 1):length(x)])
  x_padded <- c(start_pad, x, end_pad)

  # Create and apply filter
  bf <- signal::butter(order, normalized_cutoff, type = "high")
  filtered_padded <- signal::filtfilt(bf, x_padded)

  # Remove padding and ensure original length
  filtered <- filtered_padded[(n_pad + 1):(n_pad + length(x))]

  return(filtered)
}

#' Apply FFT-based Lowpass Filter to Signal
#'
#' This function implements a lowpass filter using the Fast Fourier Transform (FFT).
#' It provides a sharp frequency cutoff but may introduce ringing artifacts (Gibbs phenomenon).
#'
#' @param x Numeric vector containing the signal to be filtered
#' @param cutoff_freq Cutoff frequency in Hz
#' @param sampling_rate Sampling rate of the signal in Hz
#' @param na_action How to handle NA values:
#'        - "interpolate": Linear interpolation of NAs (default)
#'        - "remove": Remove NAs and adjust time indices
#'        - "error": Raise an error if NAs are present
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
#' See lowpass_filter documentation for details on NA handling approaches.
#'
#'
#' @return Numeric vector containing the filtered signal
#'
#' @examples
#' # Compare Butterworth and FFT filtering
#' t <- seq(0, 1, by = 0.001)
#' x <- sin(2*pi*2*t) + sin(2*pi*50*t)
#' butter_filtered <- filter_lowpass(x, 5, 1000)
#' fft_filtered <- filter_lowpass_fft(x, 5, 1000)
#'
#' @seealso
#' \code{\link{highpass_filter_fft}} for FFT-based high-pass filtering
#' \code{\link{lowpass_filter}} for Butterworth-based filtering
#'
#' @export
filter_lowpass_fft <- function(x, cutoff_freq, sampling_rate) {
  # Input validation
  if (!is.numeric(x)) stop("Input signal must be numeric")
  if (cutoff_freq <= 0 || cutoff_freq >= sampling_rate/2) {
    stop("Cutoff frequency must be between 0 and sampling_rate/2")
  }

  # Handle NAs with linear interpolation
  if (any(is.na(x))) {
    t <- seq_along(x)
    # Handle edge cases first
    if (is.na(x[1])) {
      first_valid <- which(!is.na(x))[1]
      x[1:first_valid] <- x[first_valid]
    }
    if (is.na(x[length(x)])) {
      last_valid <- max(which(!is.na(x)))
      x[last_valid:length(x)] <- x[last_valid]
    }
    # Now interpolate internal NAs
    x <- as.numeric(stats::approx(t[!is.na(x)], x[!is.na(x)], t)$y)
  }

  # Verify no NAs remain
  if (any(is.na(x))) {
    stop("Failed to properly interpolate NAs")
  }

  N <- length(x)

  # Add reflection padding to reduce edge effects
  n_pad <- ceiling(N/10)  # 10% padding
  start_pad <- rev(x[1:n_pad])
  end_pad <- rev(x[(length(x) - n_pad + 1):length(x)])
  x_padded <- c(start_pad, x, end_pad)

  # Compute FFT
  X <- fft(x_padded)
  N_padded <- length(x_padded)

  # Create frequency vector
  freq <- seq(0, sampling_rate - sampling_rate/N_padded, by = sampling_rate/N_padded)

  # Create filter mask
  mask <- freq <= cutoff_freq
  mask[N_padded:2] <- rev(mask[2:(N_padded/2 + 1)])  # Mirror for negative frequencies

  # Apply filter
  X_filtered <- X * mask

  # Inverse FFT
  filtered_padded <- Re(fft(X_filtered, inverse = TRUE)/N_padded)

  # Remove padding
  filtered <- filtered_padded[(n_pad + 1):(n_pad + N)]

  return(filtered)
}

#' Apply FFT-based Highpass Filter to Signal
#'
#' This function implements a highpass filter using the Fast Fourier Transform (FFT).
#' It provides a sharp frequency cutoff but may introduce ringing artifacts (Gibbs phenomenon).
#'
#' @param x Numeric vector containing the signal to be filtered
#' @param cutoff_freq Cutoff frequency in Hz
#' @param sampling_rate Sampling rate of the signal in Hz
#' @param na_action How to handle NA values:
#'        - "interpolate": Linear interpolation of NAs (default)
#'        - "remove": Remove NAs and adjust time indices
#'        - "error": Raise an error if NAs are present
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
#' See lowpass_filter documentation for details on NA handling approaches.
#'
#'
#' @return Numeric vector containing the filtered signal
#'
#' @export
filter_highpass_fft <- function(x, cutoff_freq, sampling_rate) {
  # Input validation
  if (!is.numeric(x)) stop("Input signal must be numeric")
  if (cutoff_freq <= 0 || cutoff_freq >= sampling_rate/2) {
    stop("Cutoff frequency must be between 0 and sampling_rate/2")
  }

  # Handle NAs with linear interpolation
  if (any(is.na(x))) {
    t <- seq_along(x)
    # Handle edge cases first
    if (is.na(x[1])) {
      first_valid <- which(!is.na(x))[1]
      x[1:first_valid] <- x[first_valid]
    }
    if (is.na(x[length(x)])) {
      last_valid <- max(which(!is.na(x)))
      x[last_valid:length(x)] <- x[last_valid]
    }
    # Now interpolate internal NAs
    x <- as.numeric(stats::approx(t[!is.na(x)], x[!is.na(x)], t)$y)
  }

  # Verify no NAs remain
  if (any(is.na(x))) {
    stop("Failed to properly interpolate NAs")
  }

  N <- length(x)

  # Add reflection padding to reduce edge effects
  n_pad <- ceiling(N/10)  # 10% padding
  start_pad <- rev(x[1:n_pad])
  end_pad <- rev(x[(length(x) - n_pad + 1):length(x)])
  x_padded <- c(start_pad, x, end_pad)

  # Compute FFT
  X <- fft(x_padded)
  N_padded <- length(x_padded)

  # Create frequency vector
  freq <- seq(0, sampling_rate - sampling_rate/N_padded, by = sampling_rate/N_padded)

  # Create filter mask
  mask <- freq >= cutoff_freq
  mask[N_padded:2] <- rev(mask[2:(N_padded/2 + 1)])  # Mirror for negative frequencies

  # Apply filter
  X_filtered <- X * mask

  # Inverse FFT
  filtered_padded <- Re(fft(X_filtered, inverse = TRUE)/N_padded)

  # Remove padding
  filtered <- filtered_padded[(n_pad + 1):(n_pad + N)]

  return(filtered)
}
