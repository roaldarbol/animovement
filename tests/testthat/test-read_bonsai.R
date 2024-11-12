# Test arguments
library(here)
here::i_am("tests/testthat/test-read_bonsai.R")
path_correct <- here("tests", "data", "bonsai", "LI850.csv")

# File headers
test_that("File headers", {
  expect_no_error(
    ensure_file_has_headers(path_correct)
  )
})


# Read file
test_that("Read file", {
  expect_no_error(
    read_bonsai(path_correct)
  )
  expect_contains(
    read_bonsai(path_correct) |>
      names(),
    c("time", "keypoint", "x", "y")
  )
})

# Full correct test
test_that("Correct setup", {
  expect_no_error(
    read_bonsai(path = path_correct)
  )
})
