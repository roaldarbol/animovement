% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_bandwidth.R
\name{filter_lowpass}
\alias{filter_lowpass}
\title{Apply Butterworth Lowpass Filter to Signal}
\usage{
filter_lowpass(
  x,
  cutoff_freq,
  sampling_rate,
  order = 4,
  na_action = c("linear", "spline", "stine", "locf", "value", "error"),
  keep_na = FALSE,
  ...
)
}
\arguments{
\item{x}{Numeric vector containing the signal to be filtered}

\item{cutoff_freq}{Cutoff frequency in Hz. Frequencies below this value are passed,
while frequencies above are attenuated. Should be between 0 and sampling_rate/2.}

\item{sampling_rate}{Sampling rate of the signal in Hz. Must be at
least twice the highest frequency component in the signal (Nyquist criterion).}

\item{order}{Filter order (default = 4). Controls the steepness of frequency rolloff:
- Higher orders give sharper cutoffs but may introduce more ringing
- Lower orders give smoother transitions but less steep rolloff
- Common values in practice are 2-8
- Values above 8 are rarely used due to numerical instability}

\item{na_action}{Method to handle NA values before filtering. One of:
- "linear": Linear interpolation (default)
- "spline": Spline interpolation for smoother curves
- "stine": Stineman interpolation preserving data shape
- "locf": Last observation carried forward
- "value": Replace with a constant value
- "error": Raise an error if NAs are present}

\item{keep_na}{Logical indicating whether to restore NAs to their original positions
after filtering (default = FALSE)}

\item{...}{Additional arguments passed to replace_na(). Common options include:
- value: Numeric value for replacement when na_action = "value"
- min_gap: Minimum gap size to interpolate/fill
- max_gap: Maximum gap size to interpolate/fill}
}
\value{
Numeric vector containing the filtered signal
}
\description{
This function applies a lowpass Butterworth filter to a signal using forward-backward
filtering (filtfilt) to achieve zero phase distortion. The Butterworth filter is
maximally flat in the passband, making it ideal for many signal processing applications.
}
\details{
The Butterworth filter response falls off at -6*order dB/octave. The cutoff frequency
corresponds to the -3dB point of the filter's magnitude response.

Parameter Selection Guidelines:
\itemize{
\item cutoff_freq: Choose based on the frequency content you want to preserve
\item sampling_rate: Should match your data collection rate
\item order:
\itemize{
\item order=2: Gentle rolloff, minimal ringing (~12 dB/octave)
\item order=4: Standard choice, good balance (~24 dB/octave)
\item order=6: Steeper rolloff, some ringing (~36 dB/octave)
\item order=8: Very steep, may have significant ringing (~48 dB/octave)
Note: For very low cutoff frequencies (<0.001 of Nyquist), order is automatically
reduced to 2 to maintain stability.
}
}

Common values by field:
\itemize{
\item Biomechanics: order=2 or 4
\item EEG/MEG: order=4 or 6
\item Audio processing: order=2 to 8
\item Mechanical vibrations: order=2 to 4
}

Missing Value Handling:
The function uses replace_na() internally for handling missing values. See ?replace_na
for detailed information about each method and its parameters. NAs can optionally be
restored to their original positions after filtering using keep_na = TRUE.
}
\examples{
# Generate example signal: 2 Hz fundamental + 50 Hz noise
t <- seq(0, 1, by = 0.001)
x <- sin(2*pi*2*t) + 0.5*sin(2*pi*50*t)

# Add some NAs
x[sample(length(x), 10)] <- NA

# Basic filtering with linear interpolation for NAs
filtered <- filter_lowpass(x, cutoff_freq = 5, sampling_rate = 1000)

# Using spline interpolation with max gap constraint
filtered <- filter_lowpass(x, cutoff_freq = 5, sampling_rate = 1000,
                          na_action = "spline", max_gap = 3)

# Replace NAs with zeros before filtering
filtered <- filter_lowpass(x, cutoff_freq = 5, sampling_rate = 1000,
                          na_action = "value", value = 0)

# Filter but keep NAs in their original positions
filtered <- filter_lowpass(x, cutoff_freq = 5, sampling_rate = 1000,
                          na_action = "linear", keep_na = TRUE)

}
\references{
Butterworth, S. (1930). On the Theory of Filter Amplifiers.
Wireless Engineer, 7, 536-541.
}
\seealso{
\code{\link{replace_na}} for details on NA handling methods
\code{\link{filter_highpass}} for high-pass filtering
\code{\link{butter}} for Butterworth filter design
\code{\link{filtfilt}} for zero-phase digital filtering
}
