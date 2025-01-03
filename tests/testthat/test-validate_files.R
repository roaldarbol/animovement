# Test arguments

path_directory <- testthat::test_path("data", "single")
path_correct <- testthat::test_path("data", "single", "opticalflow_sensor_1.csv")
path_correct2 <- testthat::test_path("data", "single", "opticalflow_sensor_2.csv")
path_wrong <- testthat::test_path("data", "single", "opticalflow_sensor_12.csv")
path_wrong_suffix <- testthat::test_path("data", "single", "opticalflow_sensor_12.txt")
paths_multiple <- c(path_correct, path_correct2)

path_sleap <- testthat::test_path("data", "sleap", "SLEAP_single-mouse_EPM.analysis.h5")

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
  expect_error(
    ensure_file_has_expected_suffix(path_sleap, expected_suffix = "csv")
  )
})

# Correct headers present
test_that("Test whether files have headers", {
  expect_no_error(
    does_file_have_expected_headers(path_correct, expected_headers = c("time", "x", "y"))
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

#
