# tests/testthat/test-filters.R

library(testthat)
library(signal)

# Helper function to generate test signals
generate_test_signal <- function(freq1 = 2, freq2 = 50, duration = 1,
                                 sampling_rate = 1000) {
  t <- seq(0, duration, by = 1/sampling_rate)
  sin(2*pi*freq1*t) + sin(2*pi*freq2*t)
}

# Test lowpass filter basic functionality
test_that("filter_lowpass correctly attenuates high frequencies", {
  # Generate signal with 2 Hz and 50 Hz components
  x <- generate_test_signal()

  # Apply filter
  filtered <- filter_lowpass(x, cutoff_freq = 5, sampling_rate = 1000)

  # Check output properties
  expect_equal(length(filtered), length(x))
  expect_true(all(is.finite(filtered)))

  # Check frequency content using FFT
  X <- abs(fft(filtered))
  freq <- seq(0, 999, length.out = length(X))

  # High frequencies should be attenuated
  high_freq_power <- mean(X[freq > 10])
  low_freq_power <- mean(X[freq < 5])
  expect_true(high_freq_power < 0.1 * low_freq_power)
})

# Theoretically driven test, needs to be looked through and changed to have the correct assumptions
# test_that("filter_lowpass correctly attenuates high frequencies", {
#   x <- generate_test_signal()
#   filtered <- filter_lowpass(x, cutoff_freq = 5, sampling_rate = 1000, order = 4)
#
#   # Get frequency response
#   X <- abs(fft(filtered))
#   freq <- seq(0, 999, length.out = length(X))
#
#   # Test specific frequencies
#   # At cutoff (5 Hz), should be -3dB (≈ 0.707 of original)
#   cutoff_response <- mean(X[freq >= 4.9 & freq <= 5.1])
#   passband_response <- mean(X[freq >= 1.9 & freq <= 2.1])  # around 2 Hz signal
#   expect_equal(cutoff_response/passband_response, 0.707, tolerance = 0.1)
#
#   # At twice cutoff (10 Hz), should be -24dB for 4th order filter
#   # -24dB ≈ 0.063 of original
#   double_cutoff_response <- mean(X[freq >= 9.9 & freq <= 10.1])
#   expect_equal(double_cutoff_response/passband_response, 0.063, tolerance = 0.02)
# })

# Test highpass filter basic functionality
test_that("filter_highpass correctly attenuates low frequencies", {
  # Generate signal with 2 Hz and 50 Hz components
  x <- generate_test_signal()

  # Apply filter
  filtered <- filter_highpass(x, cutoff_freq = 20, sampling_rate = 1000)

  # Check output properties
  expect_equal(length(filtered), length(x))
  expect_true(all(is.finite(filtered)))

  # Check frequency content
  X <- abs(fft(filtered))
  freq <- seq(0, 999, length.out = length(X))

  # Low frequencies should be attenuated
  low_freq_power <- mean(X[freq < 10])
  high_freq_power <- mean(X[freq > 30])
  expect_true(low_freq_power < high_freq_power)
})

# Test input validation
test_that("filters handle invalid inputs appropriately", {
  x <- generate_test_signal()

  # Test invalid cutoff frequencies
  expect_error(filter_lowpass(x, cutoff_freq = 0, sampling_rate = 1000))
  expect_error(filter_lowpass(x, cutoff_freq = 501, sampling_rate = 1000))
  expect_error(filter_highpass(x, cutoff_freq = -1, sampling_rate = 1000))

  # Test invalid filter orders
  expect_error(filter_lowpass(x, cutoff_freq = 10, order = 0, sampling_rate = 1000))
  expect_error(filter_lowpass(x, cutoff_freq = 10, order = 9, sampling_rate = 1000))

  # Test non-numeric input
  expect_error(filter_lowpass(as.character(x), cutoff_freq = 10, sampling_rate = 1000))
  expect_error(filter_highpass(as.character(x), cutoff_freq = 10, sampling_rate = 1000))
})

# Test NA handling
test_that("filters handle NA values correctly", {
  x <- generate_test_signal()

  # Create signal with NAs
  x_with_na <- x
  x_with_na[c(100, 200, 300:305)] <- NA

  # Test interpolation
  expect_no_error({
    filtered_interp <- filter_lowpass(x_with_na, cutoff_freq = 5, sampling_rate = 1000,
                                      na_action = "interpolate")
  })
  expect_false(any(is.na(filtered_interp)))

  # Test removal
  expect_no_error({
    filtered_remove <- filter_lowpass(x_with_na, cutoff_freq = 5, sampling_rate = 1000,
                                      na_action = "remove")
  })
  expect_false(any(is.na(filtered_remove)))
  expect_equal(length(filtered_remove), sum(!is.na(x_with_na)))

  # Test error option
  expect_error(filter_lowpass(x_with_na, cutoff_freq = 5, sampling_rate = 1000,
                              na_action = "error"))
})

# Test filter order effects
# test_that("filter order affects cutoff steepness", {
#   x <- generate_test_signal()
#
#   # Compare different orders
#   filtered_order2 <- filter_lowpass(x, cutoff_freq = 10, order = 2)
#   filtered_order4 <- filter_lowpass(x, cutoff_freq = 10, order = 4)
#   filtered_order6 <- filter_lowpass(x, cutoff_freq = 10, order = 6)
#
#   # Check frequency content
#   X2 <- abs(fft(filtered_order2))
#   X4 <- abs(fft(filtered_order4))
#   X6 <- abs(fft(filtered_order6))
#
#   freq <- seq(0, 999, length.out = length(X2))
#   transition_band <- freq > 10 & freq < 20
#
#   # Higher orders should have steeper rolloff
#   mean_transition2 <- mean(X2[transition_band])
#   mean_transition4 <- mean(X4[transition_band])
#   mean_transition6 <- mean(X6[transition_band])
#
#   expect_true(mean_transition6 < mean_transition4)
#   expect_true(mean_transition4 < mean_transition2)
# })

# Test edge cases
# test_that("filters handle edge cases appropriately", {
#   # Test very short signals
#   x_short <- generate_test_signal(duration = 0.1)
#   expect_no_error(filter_lowpass(x_short, cutoff_freq = 5))
#
#   # Test constant signal
#   x_const <- rep(1, 1000)
#   filtered_const <- filter_lowpass(x_const, cutoff_freq = 5)
#   expect_equal(mean(filtered_const), 1, tolerance = 1e-6)
#
#   # Test signals with extreme values
#   x_extreme <- generate_test_signal() * 1e6
#   filtered_extreme <- filter_lowpass(x_extreme, cutoff_freq = 5)
#   expect_true(all(is.finite(filtered_extreme)))
# })

# Test FFT-based filters
test_that("FFT-based filters perform similarly to Butterworth", {
  x <- generate_test_signal()

  # Compare lowpass filters
  butter_low <- filter_lowpass(x, cutoff_freq = 5, sampling_rate = 1000)
  fft_low <- filter_lowpass_fft(x, cutoff_freq = 5, sampling_rate = 1000)

  # Check correlation between results
  cor_val <- cor(butter_low, fft_low)
  expect_true(cor_val > 0.95)

  # Compare frequency content
  X_butter <- abs(fft(butter_low))
  X_fft <- abs(fft(fft_low))
  freq <- seq(0, 999, length.out = length(X_butter))

  # Both should attenuate high frequencies
  high_freq_butter <- mean(X_butter[freq > 10])
  high_freq_fft <- mean(X_fft[freq > 10])

  expect_true(high_freq_butter < 1)
  expect_true(high_freq_fft < 1)
})
