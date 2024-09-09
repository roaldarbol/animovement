# Test arguments
library(here)
here::i_am("tests/testthat/test-read_trackball.R")
path_correct <- here("inst", "extdata", "single", "opticalflow_sensor_1.csv")
path_named_cols <- here("inst", "extdata", "single", "named_cols_opticalflow_sensor_1.csv")
path_correct2 <- here("inst", "extdata", "single", "opticalflow_sensor_2.csv")
path_wrong <- here("inst", "extdata", "single", "opticalflow_sensor_12.csv")
path_wrong_suffix <- here("inst", "extdata", "single", "opticalflow_sensor_12.txt")
paths_multiple <- c(path_correct, path_correct2)
paths_multiple_wrong <- c(path_correct, path_wrong_suffix)

# File headers
test_that("File headers", {
  expect_false(
    file_has_headers(path_correct)
  )
  expect_true(
    file_has_headers(path_named_cols)
  )
})

# Read file
test_that("Read file", {
  expect_no_error(
    read_opticalflow(path_correct, col_time = 4)
  )
  expect_contains(
    read_opticalflow(path_correct, col_time = 4) |>
      names(),
    c("dx", "dy", "time")
  )
})

# Join trackball files
# join_trackball_files

# Full correct test
# test_that("Correct setup", {
#   expect_no_error(
#     read_trackball(
#       filepaths = paths_multiple,
#       setup = "of_free",
#       sampling_rate = 60,
#       col_dx = "x",
#       col_dy = "y",
#       col_time = 4,
#       distance_scale = 394,
#       distance_unit = NULL
#     )
#   )
# })
