# Test arguments
library(here)
here::i_am("tests/testthat/test-validate_files.R")

path_directory <- here("inst", "extdata", "single")
path_correct <- here("inst", "extdata", "single", "opticalflow_sensor_1.csv")
path_correct2 <- here("inst", "extdata", "single", "opticalflow_sensor_2.csv")
path_wrong <- here("inst", "extdata", "single", "opticalflow_sensor_12.csv")
path_wrong_suffix <- here("inst", "extdata", "single", "opticalflow_sensor_12.txt")
paths_multiple <- c(path_correct, path_correct2)

# Directory
test_that("Test whether file is not a directory", {
  expect_no_error(
    ensure_is_not_dir(path_correct)
  )
  expect_error(
    ensure_is_not_dir(path_directory)
  )
})

# Files exist
test_that("Test whether file exists", {
  expect_no_error(
    ensure_file_exists_when_expected(path_correct, expected_permission = "r")
  )
  expect_no_error(
    ensure_file_exists_when_expected(path_wrong, expected_permission = "w")
  )
  expect_error(
    ensure_file_exists_when_expected(path_correct, expected_permission = "w")
  )
  expect_error(
    ensure_file_exists_when_expected(path_wrong, expected_permission = "r")
  )
})

# Has permissions
test_that("Test whether the appropriate permissions are granted", {
  expect_no_error(
    ensure_file_has_access_permissions(path_correct, expected_permission = "r")
  )
  expect_no_error(
    ensure_file_has_access_permissions(path_correct, expected_permission = "w")
  )
  expect_error(
    ensure_file_has_access_permissions(path_wrong, expected_permission = "w")
  )
  expect_error(
    ensure_file_has_access_permissions(path_wrong, expected_permission = "r")
  )
})

# Suffix
test_that("Test whether files have the expected suffix", {
  expect_no_error(
    ensure_file_has_expected_suffix(path_correct, expected_suffix = "csv")
  )
  expect_error(
    ensure_file_has_expected_suffix(path_wrong_suffix, expected_suffix = "csv")
  )
})

# Full validation
test_that("Test whether the full validation function works", {
  expect_no_error(
    validate_files(path_correct, expected_suffix = "csv")
  )
  expect_no_error(
    validate_files(paths_multiple, expected_suffix = "csv")
  )
})
