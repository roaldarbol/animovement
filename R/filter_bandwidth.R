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
filter_lowpass <- function(x, cutoff_freq, sampling_rate, order = 4,
                           na_action = c("interpolate", "remove", "error")) {
    # Input validation
    if (!is.numeric(x)) stop("Input signal must be numeric")
    if (cutoff_freq <= 0 || cutoff_freq >= sampling_rate/2) {
      stop("Cutoff frequency must be between 0 and sampling_rate/2")
    }
    if (order < 1 || order > 8) {
      stop("Filter order should be between 1 and 8")
    }

  na_action <- match.arg(na_action)

    # Handle NAs
    has_na <- any(is.na(x))
    if (has_na) {
      switch(na_action,
             "error" = {
               stop("Input signal contains NA values")
             },
             "interpolate" = {
               # Use linear interpolation
               t <- seq_along(x)
               x <- stats::approx(t[!is.na(x)], x[!is.na(x)], t)$y
             },
             "remove" = {
               # Remove NAs and adjust signal
               x <- x[!is.na(x)]
               # Adjust sampling rate if significant data is removed
               if (length(x) < 0.9 * length(x)) {
                 warning("More than 10% of data points were NA. ",
                         "Effective sampling rate may be affected.")
               }
             }
      )
    }

    nyquist_freq <- sampling_rate/2
    normalized_cutoff <- cutoff_freq/nyquist_freq

    # Create and apply Butterworth filter
    bf <- signal::butter(order, normalized_cutoff, type = "low")
    filtered <- signal::filtfilt(bf, x)

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
filter_highpass <- function(x, cutoff_freq, sampling_rate, order = 4,
                            na_action = c("interpolate", "remove", "error")) {
  # Input validation
  if (!is.numeric(x)) stop("Input signal must be numeric")
  if (cutoff_freq <= 0 || cutoff_freq >= sampling_rate/2) {
    stop("Cutoff frequency must be between 0 and sampling_rate/2")
  }
  if (order < 1 || order > 8) {
    stop("Filter order should be between 1 and 8")
  }

  na_action <- match.arg(na_action)

  # Handle NAs
  has_na <- any(is.na(x))
  if (has_na) {
    switch(na_action,
           "error" = {
             stop("Input signal contains NA values")
           },
           "interpolate" = {
             # Use linear interpolation
             t <- seq_along(x)
             x <- stats::approx(t[!is.na(x)], x[!is.na(x)], t)$y
           },
           "remove" = {
             # Remove NAs and adjust signal
             x <- x[!is.na(x)]
             # Adjust sampling rate if significant data is removed
             if (length(x) < 0.9 * length(x)) {
               warning("More than 10% of data points were NA. ",
                       "Effective sampling rate may be affected.")
             }
           }
    )
  }

  nyquist_freq <- sampling_rate/2
  normalized_cutoff <- cutoff_freq/nyquist_freq

  # Create and apply Butterworth filter
  bf <- signal::butter(order, normalized_cutoff, type = "high")
  filtered <- signal::filtfilt(bf, x)

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
filter_lowpass_fft <- function(x, cutoff_freq, sampling_rate,
                               na_action = c("interpolate", "remove", "error")) {
  # Input validation
  if (!is.numeric(x)) stop("Input signal must be numeric")
  if (cutoff_freq <= 0 || cutoff_freq >= sampling_rate/2) {
    stop("Cutoff frequency must be between 0 and sampling_rate/2")
  }

  na.action <- match.arg(na_action)

  # Handle NAs
  has_na <- any(is.na(x))
  if (has_na) {
    switch(na_action,
           "error" = {
             stop("Input signal contains NA values")
           },
           "interpolate" = {
             # Use linear interpolation
             t <- seq_along(x)
             x <- stats::approx(t[!is.na(x)], x[!is.na(x)], t)$y
           },
           "remove" = {
             # Remove NAs and adjust signal
             x <- x[!is.na(x)]
             # Adjust sampling rate if significant data is removed
             if (length(x) < 0.9 * length(x)) {
               warning("More than 10% of data points were NA. ",
                       "Effective sampling rate may be affected.")
             }
           }
    )
  }
  N <- length(x)

  # Compute FFT
  X <- fft(x)

  # Create frequency vector
  freq <- seq(0, sampling_rate - sampling_rate/N, by = sampling_rate/N)

  # Create filter mask
  mask <- freq <= cutoff_freq
  mask[N:2] <- rev(mask[2:(N/2 + 1)])  # Mirror for negative frequencies

  # Apply filter
  X_filtered <- X * mask

  # Inverse FFT
  filtered <- Re(fft(X_filtered, inverse = TRUE)/N)

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
filter_highpass_fft <- function(x, cutoff_freq, sampling_rate,
                                na_action = c("interpolate", "remove", "error")) {
      # Input validation
    if (!is.numeric(x)) stop("Input signal must be numeric")
    if (cutoff_freq <= 0 || cutoff_freq >= sampling_rate/2) {
        stop("Cutoff frequency must be between 0 and sampling_rate/2")
    }

  na_action <- match.arg(na_action)

    # Handle NAs
    has_na <- any(is.na(x))
    if (has_na) {
        switch(na_action,
            "error" = {
                stop("Input signal contains NA values")
            },
            "interpolate" = {
                # Use linear interpolation
                t <- seq_along(x)
                x <- stats::approx(t[!is.na(x)], x[!is.na(x)], t)$y
            },
            "remove" = {
                # Remove NAs and adjust signal
                x <- x[!is.na(x)]
                # Adjust sampling rate if significant data is removed
                if (length(x) < 0.9 * length(x)) {
                    warning("More than 10% of data points were NA. ",
                           "Effective sampling rate may be affected.")
                }
            }
        )
    }
  N <- length(x)

  # Compute FFT
  X <- fft(x)

  # Create frequency vector
  freq <- seq(0, sampling_rate - sampling_rate/N, by = sampling_rate/N)

  # Create filter mask
  mask <- freq >= cutoff_freq
  mask[N:2] <- rev(mask[2:(N/2 + 1)])  # Mirror for negative frequencies

  # Apply filter
  X_filtered <- X * mask

  # Inverse FFT
  filtered <- Re(fft(X_filtered, inverse = TRUE)/N)

  return(filtered)
}
