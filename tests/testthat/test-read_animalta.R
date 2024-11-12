# Test arguments
library(here)
here::i_am("tests/testthat/test-read_animalta.R")
path_roi <- here("tests", "data", "animalta", "single_individual_multi_arena.csv")
path_no_roi <- here("tests", "data", "animalta", "variable_individuals_single_arena.csv")

# File headers
test_that("File headers", {
  expect_no_error(
    ensure_file_has_headers(path_roi)
  )
  expect_no_error(
    ensure_file_has_headers(path_no_roi)
  )
})


# Read file
test_that("Read file", {
  expect_no_error(
    read_animalta(path_roi, with_roi = TRUE)
  )
  expect_no_error(
    read_animalta(path_no_roi, with_roi = FALSE)
  )
  expect_error(
    read_animalta(path_roi, with_roi = FALSE)
  )
  expect_error(
    read_animalta(path_no_roi, with_roi = TRUE)
  )
  expect_contains(
    read_animalta(path_roi, with_roi = TRUE) |>
      names(),
    c("time", "individual", "keypoint", "x", "y")
  )
  expect_contains(
    read_animalta(path_no_roi, with_roi = FALSE) |>
      names(),
    c("time", "individual", "keypoint", "x", "y")
  )
})
