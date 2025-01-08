library(testthat)

# # Example usage:
data1 <- data.frame(
  time = seq(from = 0, to = 10, length.out = 10),
  value1 = rnorm(10)
)

data2 <- data.frame(
  timestamp = seq(from = 0.05, to = 10, length.out = 8),
  value2 = rnorm(8)
)
data1
data2

jb <- dplyr::join_by(dplyr::closest(time > timestamp))
result1 <- dplyr::left_join(data1, data2, by = jb) |>
  dplyr::select(-time)

result2 <- join_closest(data2, data1, "timestamp", "time", direction = ">=")
all(identical(result1, result2))
result1
result2

microbenchmark::microbenchmark(
  dp = dplyr::left_join(data1, data2, by = jb),
  us = join_closest(data1, data2, "time", "timestamp", direction = ">=")
)



test_that("join_closest basic functionality works", {
  x <- data.frame(
    time = c(0, 1, 2, 3),
    value1 = 1:4
  )
  y <- data.frame(
    timestamp = c(0.5, 1.5, 2.5),
    value2 = letters[1:3]
  )

  # Test >= direction
  result_ge <- join_closest(x, y, "time", "timestamp", ">=")
  expect_equal(nrow(result_ge), 4)
  expect_equal(result_ge$value2, c(NA, "a", "a", "b"))

  # Test <= direction
  result_le <- join_closest(x, y, "time", "timestamp", "<=")
  expect_equal(nrow(result_le), 4)
  expect_equal(result_le$value2, c("a", "b", "c", NA))
})

test_that("join_closest handles edge cases", {
  x <- data.frame(
    time = c(-1, 0, 10, 11),
    value1 = 1:4
  )
  y <- data.frame(
    timestamp = c(0, 1, 2),
    value2 = letters[1:3]
  )

  # Test values outside range
  result <- join_closest(x, y, "time", "timestamp", ">=")
  expect_equal(result$value2, c(NA, "a", "c", NA))

  # Test empty data frames
  expect_warning(
    result <- join_closest(x[0,], y, "time", "timestamp"),
    "'x' has 0 rows"
  )
  expect_equal(nrow(result), 0)

  expect_warning(
    result <- join_closest(x, y[0,], "time", "timestamp"),
    "'y' has 0 rows"
  )
  expect_equal(ncol(result), ncol(x))
})

test_that("join_closest validates inputs", {
  x <- data.frame(time = 1)
  y <- data.frame(timestamp = 1)

  # Test invalid data frame
  expect_error(
    join_closest(list(), y, "time", "timestamp"),
    "'x' must be a data frame"
  )

  # Test missing column
  expect_error(
    join_closest(x, y, "wrong", "timestamp"),
    "Column 'wrong' not found in 'x' data frame"
  )

  # Test non-numeric column
  x$time <- "a"
  expect_error(
    join_closest(x, y, "time", "timestamp"),
    "Column 'time' in 'x' must be numeric"
  )

  # Test invalid direction
  expect_error(
    join_closest(x, y, "time", "timestamp", direction = ">"),
    "'direction' must be either '<=' or '>='"
  )
})

test_that("join_closest handles unsorted and duplicate timestamps", {
  x <- data.frame(
    time = 1:3,
    value1 = 1:3
  )
  y <- data.frame(
    timestamp = c(1.5, 0.5, 2.5),  # unsorted
    value2 = letters[1:3]
  )

  # Test unsorted timestamps
  expect_warning(
    result <- join_closest(x, y, "time", "timestamp"),
    "Timestamps in 'y' are not sorted"
  )
  expect_equal(result$value2, c("b", "a", "a"))

  # Test duplicate timestamps
  y2 <- data.frame(
    timestamp = c(1.5, 1.5, 2.5),  # duplicate
    value2 = letters[1:3]
  )
  expect_warning(
    result <- join_closest(x, y2, "time", "timestamp"),
    "Duplicate timestamps found in 'y'"
  )
})

test_that("join_closest handles grouped data", {
  x <- data.frame(
    group = c(1,1,1, 2,2,2),
    time = c(0,1,2, 0,1,2),
    value1 = 1:6
  )
  y <- data.frame(
    group = c(1,1, 2,2),
    timestamp = c(0.5,1.5, 0.5,1.5),
    value2 = letters[1:4]
  )

  # Test >= direction with groups
  result_ge <- join_closest(x, y, "time", "timestamp", ">=", by = "group")
  expect_equal(nrow(result_ge), 6)

  # Mathc with dplyr
  jb <- dplyr::join_by(group, dplyr::closest(time >= timestamp))
  result_dp <- dplyr::left_join(x, y, by = jb)
  expect_equal(result_ge$value2, result_dp$value2)

  # Test <= direction with groups
  result_le <- join_closest(x, y, "time", "timestamp", "<=", by = "group")
  expect_equal(nrow(result_le), 6)
  expect_equal(result_le$value2, c("a", "b", NA, "c", "d", NA))

  # Test multiple grouping columns
  x$group2 <- rep(c("A", "B"), each = 3)
  y$group2 <- rep("A", 4)
  result <- join_closest(x, y, "time", "timestamp", by = c("group", "group2"))
  expect_equal(sum(!is.na(result$value2)), 3) # Only group 1, A should match
})

test_that("join_closest validates grouping columns", {
  x <- data.frame(group = 1, time = 1)
  y <- data.frame(other = 1, timestamp = 1)

  expect_error(
    join_closest(x, y, "time", "timestamp", by = "group"),
    "Grouping columns .* not found in 'y'"
  )

  expect_error(
    join_closest(x, y, "time", "timestamp", by = "other"),
    "Grouping columns .* not found in 'x'"
  )
})
