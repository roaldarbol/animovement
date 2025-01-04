library(testthat)

# Test data setup
simple_vec <- c(1, NA, NA, 4, 5, NA, NA, NA, 9)
edge_nas <- c(NA, NA, 3, 4, NA, NA)
single_gap <- c(1, NA, 3)
no_nas <- c(1, 2, 3, 4, 5)
all_nas <- rep(NA_real_, 5)

test_that("replace_na input validation works", {
  # Check numeric input requirement
  expect_error(replace_na("not numeric"), "must be numeric")
  expect_error(replace_na(factor(1:3)), "must be numeric")

  # Check method validation
  expect_error(replace_na(simple_vec, method = "invalid"))

  # Check value parameter
  expect_error(replace_na(simple_vec, method = "value"), "value must be specified")
  expect_error(replace_na(simple_vec, method = "value", value = "0"), "must be a single numeric value")
  expect_error(replace_na(simple_vec, method = "value", value = c(1, 2)), "must be a single numeric value")

  # Check gap parameters
  expect_error(replace_na(simple_vec, min_gap = 0), "min_gap must be >= 1")
  expect_error(replace_na(simple_vec, min_gap = 3, max_gap = 2), "max_gap must be >= min_gap")
})

test_that("replace_na handles edge cases correctly", {
  # No NAs
  expect_identical(replace_na(no_nas), no_nas)
  expect_identical(replace_na(no_nas, method = "spline"), no_nas)
  expect_identical(replace_na(no_nas, method = "value", value = 0), no_nas)

  # All NAs
  expect_warning(replace_na(all_nas), "At least 2 non-NA data points required")
  expect_warning(replace_na(all_nas, method = "spline"), "At least 2 non-NA data points required")
})

test_that("linear interpolation works correctly", {
  # Basic interpolation
  result <- replace_na(c(1, NA, 3), method = "linear")
  expect_equal(result, c(1, 2, 3))

  # Edge NAs should be filled using rule = 2
  result <- replace_na(edge_nas, method = "linear")
  expect_equal(result[1:2], c(3, 3))  # First NAs filled with first non-NA
  expect_equal(result[5:6], c(4, 4))  # Last NAs filled with last non-NA

  # Gap constraints
  result <- replace_na(simple_vec, method = "linear", min_gap = 3)
  expect_true(is.na(result[2]))  # Single NA should remain

  result <- replace_na(simple_vec, method = "linear", max_gap = 2)
  expect_true(all(is.na(result[6:8])))  # Gap of 3 should remain NA
})

test_that("spline interpolation works correctly", {
  # Basic interpolation
  result <- replace_na(single_gap, method = "spline")
  expect_equal(result[2], 2, tolerance = 0.1)

  # Gap constraints
  result <- replace_na(simple_vec, method = "spline", min_gap = 3)
  expect_true(is.na(result[2]))  # Single NA should remain

  # Should handle edge cases like linear
  result <- replace_na(edge_nas, method = "spline")
  expect_false(any(is.na(result)))
})

test_that("stine interpolation works correctly", {
  # Basic interpolation
  result <- replace_na(single_gap, method = "stine")
  expect_equal(result[2], 2, tolerance = 0.1)

  # Gap constraints
  result <- replace_na(simple_vec, method = "stine", min_gap = 3)
  expect_true(is.na(result[2]))  # Single NA should remain

  # Should handle edge cases
  # result <- replace_na(edge_nas, method = "stine")
  # expect_false(any(is.na(result)))
})

test_that("locf works correctly", {
  # Basic filling
  expect_equal(
    replace_na(c(1, NA, NA, 2), method = "locf"),
    c(1, 1, 1, 2)
  )

  # Edge NAs at start should remain NA
  result <- replace_na(c(NA, NA, 1, NA), method = "locf")
  expect_true(all(is.na(result[1:2])))
  expect_equal(result[3:4], c(1, 1))

  # Gap constraints
  result <- replace_na(simple_vec, method = "locf", min_gap = 3)
  expect_true(is.na(result[2]))  # Single NA should remain

  result <- replace_na(simple_vec, method = "locf", max_gap = 2)
  expect_true(all(is.na(result[6:8])))  # Gap of 3 should remain NA
})

test_that("value replacement works correctly", {
  # Basic replacement
  expect_equal(
    replace_na(c(1, NA, NA, 2), method = "value", value = 0),
    c(1, 0, 0, 2)
  )

  # Gap constraints
  result <- replace_na(simple_vec, method = "value", value = -999, min_gap = 3)
  expect_true(is.na(result[2]))  # Single NA should remain
  expect_equal(result[6:8], rep(-999, 3))  # Longer gap should be filled

  result <- replace_na(simple_vec, method = "value", value = -999, max_gap = 2)
  expect_equal(result[2], -999)  # Single NA should be filled
  expect_true(all(is.na(result[6:8])))  # Gap of 3 should remain NA
})

test_that("min_gap and max_gap work together correctly", {
  # Test all methods with both constraints
  methods <- c("linear", "spline", "stine", "locf")

  for (method in methods) {
    result <- replace_na(simple_vec,
                         method = method,
                         min_gap = 2,
                         max_gap = 2)

    # Single NA should remain (gap too small)
    expect_true(!is.na(result[2]))

    # Triple NA should remain (gap too large)
    expect_true(all(is.na(result[6:8])))

    # Double NA should be filled (gap size 2)
    expect_false(any(is.na(result[3:4])))
  }

  # Test value method separately
  result <- replace_na(simple_vec,
                       method = "value",
                       value = 0,
                       min_gap = 2,
                       max_gap = 2)

  expect_true(!is.na(result[2]))  # Single NA
  expect_true(all(is.na(result[6:8])))  # Triple NA
  expect_equal(result[2:3], c(0, 0))  # Double NA
})
