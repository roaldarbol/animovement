# tests/testthat/test-filter_na_roi.R

library(testthat)
library(dplyr)

test_that("filter_na_roi input validation works", {
  test_data <- tibble(x = 1:5, y = 1:5)

  # No parameters provided
  expect_error(
    filter_na_roi(test_data),
    "No ROI parameters provided"
  )

  # Incomplete circle parameters
  expect_error(
    filter_na_roi(test_data, x_center = 0, y_center = 0),
    "Incomplete circular ROI parameters"
  )

  expect_error(
    filter_na_roi(test_data, x_center = 0, radius = 1),
    "Incomplete circular ROI parameters"
  )

  expect_error(
    filter_na_roi(test_data, y_center = 0, radius = 1),
    "Incomplete circular ROI parameters"
  )
})

test_that("rectangular ROI filtering works", {
  test_data <- tibble(
    x = c(1, 2, 3, 4, 5),
    y = c(1, 2, 3, 4, 5)
  )

  # Test x_min
  result <- filter_na_roi(test_data, x_min = 3)
  expect_equal(
    result,
    tibble(
      x = c(NA, NA, 3, 4, 5),
      y = c(NA, NA, 3, 4, 5)
    )
  )

  # Test x_max
  result <- filter_na_roi(test_data, x_max = 3)
  expect_equal(
    result,
    tibble(
      x = c(1, 2, 3, NA, NA),
      y = c(1, 2, 3, NA, NA)
    )
  )

  # Test y_min
  result <- filter_na_roi(test_data, y_min = 3)
  expect_equal(
    result,
    tibble(
      x = c(NA, NA, 3, 4, 5),
      y = c(NA, NA, 3, 4, 5)
    )
  )

  # Test y_max
  result <- filter_na_roi(test_data, y_max = 3)
  expect_equal(
    result,
    tibble(
      x = c(1, 2, 3, NA, NA),
      y = c(1, 2, 3, NA, NA)
    )
  )

  # Test combined bounds
  result <- filter_na_roi(test_data, x_min = 2, x_max = 4, y_min = 2, y_max = 4)
  expect_equal(
    result,
    tibble(
      x = c(NA, 2, 3, 4, NA),
      y = c(NA, 2, 3, 4, NA)
    )
  )
})

test_that("circular ROI filtering works", {
  # Create a square grid of points
  grid <- expand.grid(x = seq(-5, 5, by = 1), y = seq(-5, 5, by = 1)) |>
    as_tibble()

  # Test circle centered at origin with radius 3
  result <- filter_na_roi(grid, x_center = 0, y_center = 0, radius = 3)

  # Check if points inside circle are preserved
  inside_circle <- function(x, y, x_center, y_center, radius) {
    ((x - x_center)^2 + (y - y_center)^2) <= radius^2
  }

  for (i in seq_len(nrow(result))) {
    if (!is.na(result$x[i])) {
      expect_true(
        inside_circle(result$x[i], result$y[i], 0, 0, 3),
        label = sprintf(
          "Point (%f, %f) should be inside circle",
          result$x[i],
          result$y[i]
        )
      )
    } else {
      expect_false(
        inside_circle(grid$x[i], grid$y[i], 0, 0, 3),
        label = sprintf(
          "Point (%f, %f) should be outside circle",
          grid$x[i],
          grid$y[i]
        )
      )
    }
  }

  # Test offset circle
  result_offset <- filter_na_roi(grid, x_center = 2, y_center = -1, radius = 2)

  for (i in seq_len(nrow(result_offset))) {
    if (!is.na(result_offset$x[i])) {
      expect_true(
        inside_circle(result_offset$x[i], result_offset$y[i], 2, -1, 2),
        label = sprintf(
          "Point (%f, %f) should be inside offset circle",
          result_offset$x[i],
          result_offset$y[i]
        )
      )
    } else {
      expect_false(
        inside_circle(grid$x[i], grid$y[i], 2, -1, 2),
        label = sprintf(
          "Point (%f, %f) should be outside offset circle",
          grid$x[i],
          grid$y[i]
        )
      )
    }
  }
})

test_that("edge cases are handled correctly", {
  test_data <- tibble(x = 1:5, y = 1:5)

  # Empty data frame
  empty_data <- tibble(x = double(), y = double())
  result <- filter_na_roi(empty_data, x_min = 0)
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("x", "y"))

  # Single point
  single_point <- tibble(x = 1, y = 1)

  # Test point exactly on boundary
  result <- filter_na_roi(single_point, x_min = 1)
  expect_equal(result$x, 1)
  expect_equal(result$y, 1)

  # Test point exactly on circle boundary
  result <- filter_na_roi(
    single_point,
    x_center = 0,
    y_center = 0,
    radius = sqrt(2)
  )
  expect_equal(result$x, 1)
  expect_equal(result$y, 1)

  # Test with NA input
  na_data <- tibble(
    x = c(1, NA, 3),
    y = c(NA, 2, 3)
  )

  result <- filter_na_roi(na_data, x_min = 0)
  expect_true(all(is.na(result$x[2])))
  expect_true(all(is.na(result$y[2])))
  expect_equal(result$x[3], 3)
  expect_equal(result$y[3], 3)
})

test_that("data frame structure is preserved", {
  # Test that additional columns are preserved
  test_data <- tibble(
    x = 1:3,
    y = 1:3,
    z = letters[1:3],
    value = runif(3)
  )

  result <- filter_na_roi(test_data, x_min = 2)
  expect_equal(names(result), names(test_data))
  expect_equal(result$z, test_data$z)
  expect_equal(result$value, test_data$value)

  # Test that column order is preserved
  reordered_data <- test_data |>
    select(z, value, x, y)

  result <- filter_na_roi(reordered_data, x_min = 2)
  expect_equal(names(result), names(reordered_data))
})
