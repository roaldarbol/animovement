library(testthat)

# Helper functions for tests
generate_sine_data <- function(n = 100, freq = 0.1, noise_sd = 0.1) {
  t <- seq(0, n-1)
  true_signal <- sin(2 * pi * freq * t)
  measurements <- true_signal + rnorm(n, 0, noise_sd)
  return(list(time = t, measurements = measurements, true_signal = true_signal))
}

test_that("Basic functionality - regular sampling", {
  # Basic filtering
  data <- c(1, 1.1, 0.9, 1.2, 0.8, 1.1)
  filtered <- filter_kalman(data, sampling_rate = 10)

  expect_equal(length(filtered), length(data))
  expect_true(all(!is.na(filtered)))
  expect_true(all(is.finite(filtered)))

  # Check if output is smoother than input
  input_variance <- var(data)
  output_variance <- var(filtered)
  expect_true(output_variance <= input_variance)
})

test_that("Basic functionality - irregular sampling", {
  data <- c(1, 1.1, 0.9, 1.2, 0.8, 1.1)
  times <- c(0, 0.1, 0.3, 0.35, 0.5, 0.8)
  filtered <- filter_kalman_irregular(data, times)

  expect_equal(length(filtered), length(data))
  expect_true(all(!is.na(filtered)))
  expect_true(all(is.finite(filtered)))
})

test_that("NA handling - regular sampling", {
  # Data with NAs
  data <- c(1, NA, 0.9, NA, NA, 1.1)
  filtered <- filter_kalman(data, sampling_rate = 10)

  expect_equal(length(filtered), length(data))
  expect_true(all(!is.na(filtered)))
  expect_true(all(is.finite(filtered)))

  # All NAs except first value
  data <- c(1, rep(NA, 5))
  filtered <- filter_kalman(data, sampling_rate = 10)
  expect_equal(length(filtered), length(data))
  expect_true(all(!is.na(filtered)))
})

test_that("NA handling - irregular sampling", {
  data <- c(1, NA, 0.9, NA, NA, 1.1)
  times <- c(0, 0.1, 0.3, 0.35, 0.5, 0.8)
  filtered <- filter_kalman_irregular(data, times)

  expect_equal(length(filtered), length(data))
  expect_true(all(!is.na(filtered)))
  expect_true(all(is.finite(filtered)))
})

test_that("Edge cases - regular sampling", {
  # Single value
  expect_error(filter_kalman(1, sampling_rate = 10), NA)

  # Empty vector
  expect_error(filter_kalman(numeric(0), sampling_rate = 10))

  # All NAs
  expect_error(filter_kalman(c(NA, NA, NA), sampling_rate = 10))

  # Negative sampling rate
  expect_error(filter_kalman(c(1, 2, 3), sampling_rate = -1))

  # Zero sampling rate
  expect_error(filter_kalman(c(1, 2, 3), sampling_rate = 0))
})

test_that("Edge cases - irregular sampling", {
  # Single value
  expect_error(filter_kalman_irregular(1, times = 0), NA)

  # Empty vectors
  expect_error(filter_kalman_irregular(numeric(0), numeric(0)))

  # Mismatched lengths
  expect_error(filter_kalman_irregular(c(1,2), c(0)))

  # Non-monotonic times
  expect_error(filter_kalman_irregular(c(1,2), c(1,0)))

  # Duplicate times
  expect_warning(filter_kalman_irregular(c(1,2), c(0,0)))

  # All NAs
  expect_error(filter_kalman_irregular(c(NA, NA), c(0, 1)))
})

test_that("Resampling functionality", {
  data <- c(1, 1.1, 0.9, 1.2, 0.8, 1.1)
  times <- c(0, 0.1, 0.3, 0.35, 0.5, 0.8)

  # Basic resampling
  result <- filter_kalman_irregular(data, times, resample = TRUE, resample_freq = 10)
  expect_true(is.list(result))
  expect_equal(names(result), c("time", "values", "original_time", "original_values"))

  # Check regular spacing
  time_diffs <- diff(result$time)
  expect_true(all(abs(time_diffs - 1/10) < 1e-10))

  # Warning for high resampling frequency
  expect_warning(
    filter_kalman_irregular(data, times, resample = TRUE, resample_freq = 1000)
  )
})

test_that("Parameter validation", {
  data <- c(1, 1.1, 0.9, 1.2, 0.8, 1.1)

  # Regular sampling
  expect_error(filter_kalman(data, sampling_rate = "10"))
  expect_error(filter_kalman(data, sampling_rate = NA))
  expect_error(filter_kalman(data, sampling_rate = Inf))

  # Irregular sampling
  times <- c(0, 0.1, 0.3, 0.35, 0.5, 0.8)
  expect_error(filter_kalman_irregular(data, times = "0"))
  expect_error(filter_kalman_irregular(data, times = NA))
  expect_error(filter_kalman_irregular(data, times = Inf))
})

# test_that("Signal tracking accuracy", {
#   # Generate sine wave with noise
#   data <- generate_sine_data(n = 1000, freq = 0.05, noise_sd = 0.1)
#
#   # Regular sampling
#   filtered_reg <- filter_kalman(data$measurements, sampling_rate = 1)
#   mse_reg <- mean((filtered_reg - data$true_signal)^2)
#   noise_var <- 0.1^2
#   expect_true(mse_reg < noise_var)
#
#   # Irregular sampling
#   filtered_irreg <- filter_kalman_irregular(data$measurements, data$time)
#   mse_irreg <- mean((filtered_irreg - data$true_signal)^2)
#   expect_true(mse_irreg < noise_var)
# })

test_that("Performance benchmarks", {
  # Large dataset
  n <- 10000
  data <- generate_sine_data(n = n, freq = 0.01, noise_sd = 0.1)

  # Regular sampling
  time_reg <- system.time({
    filtered_reg <- filter_kalman(data$measurements, sampling_rate = 1)
  })
  expect_true(time_reg["elapsed"] < 1)  # Should complete in under 1 second

  # Irregular sampling
  time_irreg <- system.time({
    filtered_irreg <- filter_kalman_irregular(data$measurements, data$time)
  })
  expect_true(time_irreg["elapsed"] < 1)
})

test_that("Numerical stability", {
  # Test with very large values
  data <- c(1e6, 1.1e6, 0.9e6) * 1e6
  times <- c(0, 1, 2)

  filtered_reg <- filter_kalman(data, sampling_rate = 1)
  filtered_irreg <- filter_kalman_irregular(data, times)

  expect_true(all(is.finite(filtered_reg)))
  expect_true(all(is.finite(filtered_irreg)))

  # Test with very small values
  data <- c(1, 1.1, 0.9) * 1e-12

  filtered_reg <- filter_kalman(data, sampling_rate = 1)
  filtered_irreg <- filter_kalman_irregular(data, times)

  expect_true(all(is.finite(filtered_reg)))
  expect_true(all(is.finite(filtered_irreg)))
})

test_that("Parameter sensitivity", {
  data <- generate_sine_data(n = 100)

  # Test different base_Q values
  q_values <- 10^seq(-6, 0, by = 2)
  filtered_q <- lapply(q_values, function(q) {
    filter_kalman(data$measurements, sampling_rate = 1, base_Q = q)
  })

  # Verify that larger Q leads to more responsive filtering
  var_q <- sapply(filtered_q, var)
  expect_true(all(diff(var_q) >= 0))  # Variance should increase with Q

  # Test different R values
  r_values <- 10^seq(-6, 0, by = 2)
  filtered_r <- lapply(r_values, function(r) {
    filter_kalman(data$measurements, sampling_rate = 1, R = r)
  })

  # Verify that larger R leads to more smoothing
  var_r <- sapply(filtered_r, var)
  expect_true(all(diff(var_r) <= 0))  # Variance should decrease with R
})

