# Test arguments
library(here)
here::i_am("tests/testthat/test-validate_trackball.R")
path_correct <- here("tests", "data", "single", "opticalflow_sensor_1.csv")
path_correct2 <- here("tests", "data", "single", "opticalflow_sensor_2.csv")
path_named_cols <- here("tests", "data", "single", "named_cols_opticalflow_sensor_1.csv")
path_wrong <- here("tests", "data", "single", "opticalflow_sensor_12.csv")
path_wrong_suffix <- here("tests", "data", "single", "opticalflow_sensor_12.txt")
paths_multiple <- c(path_correct, path_correct2)
paths_multiple_wrong <- c(path_correct, path_wrong_suffix)

# Valid setup
test_that("Valid setup", {
  expect_no_error(
    ensure_trackball_setup(setup = "of_free")
  )
  expect_no_error(
    ensure_trackball_setup(setup = "of_fixed")
  )
  expect_no_error(
    ensure_trackball_setup(setup = "fictrac")
  )
  expect_error(
    ensure_trackball_setup(setup = "fixed")
  )
})

# Number of files
test_that("Number of files", {
  expect_no_error(
    ensure_number_of_files(paths = paths_multiple, setup = "of_free")
  )
  expect_no_error(
    ensure_number_of_files(paths = paths_multiple, setup = "of_fixed")
  )
  expect_no_error(
    ensure_number_of_files(paths = path_correct, setup = "of_fixed")
  )
  expect_error(
    ensure_number_of_files(paths = path_correct, setup = "of_free")
  )
})

# Identical suffixes
test_that("Identical suffixes", {
  expect_no_error(
    ensure_identical_suffix(paths = paths_multiple)
  )
  expect_error(
    ensure_identical_suffix(paths = paths_multiple_wrong)
  )
})

# Headers
test_that("Header", {
  expect_no_error(
    ensure_header_match(path = path_correct, col_time = 4)
  )
  expect_error(
    ensure_header_match(path = path_correct, col_time = "time")
  )
  expect_no_error(
    ensure_header_match(path = path_named_cols, col_time = 4)
  )
  expect_no_error(
    ensure_header_match(path = path_named_cols, col_time = "time")
  )
})

# Full correct test
test_that("Full correct test", {
  expect_no_error(
    validate_trackball(
      paths = paths_multiple,
      setup = "of_free",
      col_time = 4
    )
  )
})
