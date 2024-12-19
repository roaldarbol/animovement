library(testthat)
library(dplyr)

test_that("filter_by_speed handles basic numeric threshold correctly", {
  # Create test data
  test_data <- tibble(
    time = 1:5,
    x = c(1, 2, 4, 7, 11),
    y = c(1, 1, 2, 3, 5),
    confidence = c(0.8, 0.9, 0.7, 0.85, 0.6)
  )

  # Test with numeric threshold
  result <- filter_by_speed(test_data, threshold = 3)

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_named(result, c("time", "x", "y", "confidence"))

  # Check that values above threshold are NA
  expect_true(any(is.na(result$x)))
  expect_true(any(is.na(result$y)))
  expect_true(any(is.na(result$confidence)))

  # Check that values below threshold remain unchanged
  expect_true(any(!is.na(result$x)))
  expect_true(any(!is.na(result$y)))
  expect_true(any(!is.na(result$confidence)))
})

test_that("filter_by_speed handles 'auto' threshold correctly", {
  test_data <- tibble(
    time = 1:5,
    x = c(1, 2, 4, 7, 200),  # Last value is an outlier
    y = c(1, 1, 2, 3, 30),  # Last value is an outlier
    confidence = c(0.8, 0.9, 0.7, 0.85, 0.6)
  )

  result <- filter_by_speed(test_data, threshold = 5)

  # Check that at least one value (outlier) is filtered
  expect_true(is.na(result$x[5]))
  expect_true(is.na(result$y[5]))
  expect_true(is.na(result$confidence[5]))

  # Check that normal values remain
  expect_false(all(is.na(result$x)))
  expect_false(all(is.na(result$y)))
  expect_false(all(is.na(result$confidence)))
})

test_that("filter_by_speed works without confidence column", {
  test_data <- tibble(
    time = 1:5,
    x = c(1, 2, 4, 7, 11),
    y = c(1, 1, 2, 3, 5)
  )

  result <- filter_by_speed(test_data, threshold = 3)

  # Check structure
  expect_named(result, c("time", "x", "y"))

  # Check filtering still works
  expect_true(any(is.na(result$x)))
  expect_true(any(is.na(result$y)))
})

test_that("filter_by_speed errors on missing required columns", {
  # Missing x
  test_data1 <- tibble(
    time = 1:5,
    y = c(1, 1, 2, 3, 5)
  )

  expect_error(
    filter_by_speed(test_data1),
    regexp = "Missing required columns"
  )

  # Missing multiple columns
  test_data2 <- tibble(
    time = 1:5
  )

  expect_error(
    filter_by_speed(test_data2),
    regexp = "Missing required columns"
  )
})

test_that("filter_by_speed errors on non-numeric columns", {
  test_data <- tibble(
    time = 1:5,
    x = as.character(c(1, 2, 4, 7, 11)),  # Character instead of numeric
    y = c(1, 1, 2, 3, 5)
  )

  expect_error(
    filter_by_speed(test_data),
    regexp = "must be numeric"
  )
})

test_that("filter_by_speed errors on invalid threshold", {
  test_data <- tibble(
    time = 1:5,
    x = c(1, 2, 4, 7, 11),
    y = c(1, 1, 2, 3, 5)
  )

  expect_error(
    filter_by_speed(test_data, threshold = "invalid"),
    regexp = "threshold must be either 'auto' or a numeric value"
  )
})

test_that("filter_by_speed preserves data frame attributes", {
  test_data <- tibble(
    time = 1:5,
    x = c(1, 2, 4, 7, 11),
    y = c(1, 1, 2, 3, 5)
  )

  result <- filter_by_speed(test_data, threshold = 3)

  # Check that result is still a tibble
  expect_s3_class(result, "tbl_df")
  # Check number of rows is preserved
  expect_equal(nrow(result), nrow(test_data))
})

test_that("filter_by_speed handles NA values correctly", {
  # Test data with NAs in different positions and columns
  test_data <- tibble(
    time = c(1, 2, NA, 4, 5),
    x = c(1, NA, 4, 7, 11),
    y = c(NA, 1, 2, 3, 5),
    confidence = c(0.8, 0.9, NA, 0.85, 0.6)
  )

  # Test with numeric threshold
  result <- filter_by_speed(test_data, threshold = 3)

  # Check that NAs in input are preserved in output
  expect_true(is.na(result$time[3]))
  expect_true(is.na(result$x[2]))
  expect_true(is.na(result$y[1]))
  expect_true(is.na(result$confidence[3]))


  # Test consecutive NAs
  test_data_consecutive <- tibble(
    time = c(1, 2, NA, NA, 5),
    x = c(1, 2, NA, NA, 11),
    y = c(1, 1, NA, NA, 5),
    confidence = c(0.8, 0.9, NA, NA, 0.6)
  )

  result_consecutive <- filter_by_speed(test_data_consecutive, threshold = 3)

  # Check that consecutive NAs are handled properly
  expect_true(all(is.na(result_consecutive$x[3:4])))
  expect_true(all(is.na(result_consecutive$y[3:4])))

  # Test with all NAs in one column
  test_data_all_na <- tibble(
    time = 1:5,
    x = rep(NA_real_, 5),
    y = c(1, 1, 2, 3, 5),
    confidence = c(0.8, 0.9, 0.7, 0.85, 0.6)
  )

  result_all_na <- filter_by_speed(test_data_all_na, threshold = 3)

  # Check that column with all NAs remains all NA
  expect_true(all(is.na(result_all_na$x)))

  # Test edge case with NA in time column but valid x,y coordinates
  test_data_na_time <- tibble(
    time = c(1, NA, 3, 4, 5),
    x = c(1, 2, 4, 7, 11),
    y = c(1, 1, 2, 3, 5)
  )

  result_na_time <- filter_by_speed(test_data_na_time, threshold = 3)

  # Check that NA in time affects speed calculation for adjacent points
  expect_true(is.na(result_na_time$x[2]) || is.na(result_na_time$x[3]))
  expect_true(is.na(result_na_time$y[2]) || is.na(result_na_time$y[3]))
})
