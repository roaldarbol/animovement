library(testthat)

test_that("find_time_lag handles basic time shifts correctly", {
  # Create simple test signals
  t <- seq(0, 10, 0.1)
  reference <- sin(t)

  # Test positive lag (signal is delayed)
  delayed_signal <- sin(t - 0.5)  # 5 samples delay at 0.1 spacing
  expect_equal(find_time_lag(delayed_signal, reference, max_lag = 10), -5)

  # Test negative lag (signal is advanced)
  advanced_signal <- sin(t + 0.5)  # 5 samples advance
  expect_equal(find_time_lag(advanced_signal, reference, max_lag = 10), 4)

  # Test zero lag
  expect_equal(find_time_lag(reference, reference, max_lag = 10), 0)
})

test_that("find_time_lag handles normalization correctly", {
  t <- seq(0, 10, 0.1)
  reference <- sin(t)
  signal <- 5 * sin(t - 0.5) + 10  # Scaled and shifted in amplitude

  # Should find same lag regardless of scaling when normalize = TRUE
  expect_equal(find_time_lag(signal, reference, normalize = TRUE), 5)

  # Without normalization, might find incorrect lag due to scaling
  expect_false(
    identical(
      find_time_lag(signal, reference, normalize = TRUE),
      find_time_lag(signal, reference, normalize = FALSE)
    )
  )
})

test_that("find_time_lag respects max_lag parameter", {
  t <- seq(0, 10, 0.1)
  reference <- sin(t)
  signal <- sin(t - 1)  # 10 samples delay

  # Should find lag when max_lag is sufficient
  expect_equal(find_time_lag(signal, reference, max_lag = 15), 10)

  # Should return different result when max_lag is too small
  expect_false(
    identical(
      find_time_lag(signal, reference, max_lag = 5),
      find_time_lag(signal, reference, max_lag = 15)
    )
  )
})

test_that("find_time_lag handles missing values correctly", {
  t <- seq(0, 10, 0.1)
  reference <- sin(t)
  signal <- sin(t - 0.5)

  # Insert some NA values
  signal[c(5, 15, 25)] <- NA
  reference[c(10, 20, 30)] <- NA

  # Should still find approximate lag after removing NA values
  expect_equal(find_time_lag(signal, reference), 5)
})

test_that("align_time_series correctly shifts the signal", {
  t <- seq(0, 10, 0.1)
  reference <- sin(t)

  # Test positive lag
  delayed_signal <- sin(t - 0.5)
  aligned <- align_time_series(delayed_signal, reference)

  # Check length is preserved
  expect_equal(length(aligned), length(delayed_signal))

  # Check alignment by correlation with reference
  # (excluding NA padding)
  valid_idx <- !is.na(aligned)
  expect_gt(
    cor(aligned[valid_idx], reference[valid_idx]),
    cor(delayed_signal[valid_idx], reference[valid_idx])
  )

  # Test negative lag
  advanced_signal <- sin(t + 0.5)
  aligned <- align_time_series(advanced_signal, reference)

  # Check NA padding at end for negative lag
  expect_true(all(is.na(tail(aligned, 5))))
})

test_that("align_time_series handles edge cases", {
  # Test zero lag case
  signal <- 1:100
  aligned <- align_time_series(signal, signal)
  expect_identical(aligned, signal)

  # Test single value
  expect_equal(align_time_series(1, 1), 1)

  # Test empty vectors
  expect_error(align_time_series(numeric(0), numeric(0)))

  # Test different length vectors
  signal <- 1:100
  reference <- 1:90
  expect_error(align_time_series(signal, reference), NA)  # Should not error
})

test_that("functions handle non-numeric inputs appropriately", {
  # Test character vectors
  expect_error(find_time_lag(letters[1:10], letters[1:10]))
  expect_error(align_time_series(letters[1:10], letters[1:10]))

  # Test factors
  expect_error(find_time_lag(factor(1:10), factor(1:10)))
  expect_error(align_time_series(factor(1:10), factor(1:10)))
})
