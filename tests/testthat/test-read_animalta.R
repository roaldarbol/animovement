# Test arguments
library(here)
here::i_am("tests/testthat/test-read_animalta.R")
path_raw <- here("tests", "data", "animalta", "single_individual_multi_arena.csv")
path_detailed <- here("tests", "data", "animalta", "variable_individuals_single_arena.csv")

# File headers
test_that("File headers", {
  expect_no_error(
    ensure_file_has_headers(path_raw)
  )
  expect_no_error(
    ensure_file_has_headers(path_detailed)
  )
})


# Read file
test_that("Read file", {
  expect_no_error(
    read_animalta(path_raw, detailed = FALSE)
  )
  expect_no_error(
    read_animalta(path_detailed, detailed = TRUE)
  )
  expect_error(
    read_animalta(path_raw, detailed = TRUE)
  )
  expect_error(
    read_animalta(path_detailed, detailed = FALSE)
  )
  expect_contains(
    read_animalta(path_raw, detailed = FALSE) |>
      names(),
    c("time", "individual", "keypoint", "x", "y")
  )
  expect_contains(
    read_animalta(path_detailed, detailed = TRUE) |>
      names(),
    c("time", "individual", "keypoint", "x", "y")
  )
})
