# tests/testthat/test-filters.R

library(testthat)
library(signal)

# Helper function to generate test signals
generate_test_signal <- function(
  freq1 = 2,
  freq2 = 50,
  duration = 1,
  sampling_rate = 1000
) {
  t <- seq(0, duration, by = 1 / sampling_rate)
  sin(2 * pi * freq1 * t) + sin(2 * pi * freq2 * t)
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
  expect_error(filter_lowpass(
    x,
    cutoff_freq = 10,
    order = 0,
    sampling_rate = 1000
  ))
  expect_error(filter_lowpass(
    x,
    cutoff_freq = 10,
    order = 9,
    sampling_rate = 1000
  ))

  # Test non-numeric input
  expect_error(filter_lowpass(
    as.character(x),
    cutoff_freq = 10,
    sampling_rate = 1000
  ))
  expect_error(filter_highpass(
    as.character(x),
    cutoff_freq = 10,
    sampling_rate = 1000
  ))
})

# Test NA handling
test_that("filters handle NA values correctly with different methods", {
  x <- generate_test_signal()

  # Create signal with NAs
  x_with_na <- x
  x_with_na[c(100, 200, 300:305)] <- NA

  # Test linear interpolation (default)
  expect_no_error({
    filtered_linear <- filter_lowpass(
      x_with_na,
      cutoff_freq = 5,
      sampling_rate = 1000
    )
  })
  expect_false(any(is.na(filtered_linear)))

  # Test spline interpolation
  expect_no_error({
    filtered_spline <- filter_lowpass(
      x_with_na,
      cutoff_freq = 5,
      sampling_rate = 1000,
      na_action = "spline"
    )
  })
  expect_false(any(is.na(filtered_spline)))

  # Test stine interpolation
  expect_no_error({
    filtered_stine <- filter_lowpass(
      x_with_na,
      cutoff_freq = 5,
      sampling_rate = 1000,
      na_action = "stine"
    )
  })
  expect_false(any(is.na(filtered_stine)))

  # Test locf
  expect_no_error({
    filtered_locf <- filter_lowpass(
      x_with_na,
      cutoff_freq = 5,
      sampling_rate = 1000,
      na_action = "locf"
    )
  })
  expect_false(any(is.na(filtered_locf)))

  # Test value replacement
  expect_no_error({
    filtered_value <- filter_lowpass(
      x_with_na,
      cutoff_freq = 5,
      sampling_rate = 1000,
      na_action = "value",
      value = 0
    )
  })
  expect_false(any(is.na(filtered_value)))

  # Test error option
  expect_error(
    filter_lowpass(
      x_with_na,
      cutoff_freq = 5,
      sampling_rate = 1000,
      na_action = "error"
    ),
    "Signal contains NA values"
  )
})

# Test keep_na functionality
test_that("keep_na parameter works correctly", {
  x <- generate_test_signal()

  # Create signal with NAs
  x_with_na <- x
  na_positions <- c(100, 200, 300:305)
  x_with_na[na_positions] <- NA

  # Test with keep_na = TRUE
  filtered_keep <- filter_lowpass(
    x_with_na,
    cutoff_freq = 5,
    sampling_rate = 1000,
    keep_na = TRUE
  )
  expect_true(all(is.na(filtered_keep[na_positions])))
  expect_equal(which(is.na(filtered_keep)), na_positions)

  # Test with keep_na = FALSE
  filtered_replace <- filter_lowpass(
    x_with_na,
    cutoff_freq = 5,
    sampling_rate = 1000,
    keep_na = FALSE
  )
  expect_false(any(is.na(filtered_replace)))

  # Test with different na_action methods
  methods <- c("linear", "spline", "stine", "locf", "value")
  for (method in methods) {
    args <- list(
      x = x_with_na,
      cutoff_freq = 5,
      sampling_rate = 1000,
      na_action = method,
      keep_na = TRUE
    )
    if (method == "value") {
      args$value <- 0
    }

    filtered <- do.call(filter_lowpass, args)
    expect_true(all(is.na(filtered[na_positions])))
    expect_equal(which(is.na(filtered)), na_positions)
  }
})

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

  expect_true(high_freq_butter < 2)
  expect_true(high_freq_fft < 2)
})

# Test NA handling in FFT filters
test_that("FFT filters handle NA values consistently", {
  x <- generate_test_signal()
  x_with_na <- x
  na_positions <- c(100, 200, 300:305)
  x_with_na[na_positions] <- NA

  # Test all na_action methods
  methods <- c("linear", "spline", "stine", "locf", "value")
  for (method in methods) {
    # Test FFT filters with and without keep_na
    args <- list(
      x = x_with_na,
      cutoff_freq = 5,
      sampling_rate = 1000,
      na_action = method
    )
    if (method == "value") {
      args$value <- 0
    }

    # Without keep_na
    filtered <- do.call(filter_lowpass_fft, args)
    expect_false(any(is.na(filtered)))

    # With keep_na
    args$keep_na <- TRUE
    filtered_keep <- do.call(filter_lowpass_fft, args)
    expect_true(all(is.na(filtered_keep[na_positions])))
    expect_equal(which(is.na(filtered_keep)), na_positions)
  }

  # Test error option
  expect_error(
    filter_lowpass_fft(
      x_with_na,
      cutoff_freq = 5,
      sampling_rate = 1000,
      na_action = "error"
    ),
    "Signal contains NA values"
  )
})
