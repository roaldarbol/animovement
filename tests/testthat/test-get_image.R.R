# tests/testthat/test-get_image.R

library(testthat)
library(mockery)  # For mocking file operations

# Create a mock video file for testing
test_video <- tempfile(fileext = ".mp4")
writeLines("mock video content", test_video)

test_that("get_image handles single frame correctly", {
  skip_if_not_installed("av")

  result <- get_image(test_video, 20)

  expect_type(result, "character")
  expect_length(result, 1)
  expect_match(result, "frame_20\\.jpg$")
})

test_that("get_image handles consecutive frames correctly", {
  skip_if_not_installed("av")

  result <- get_image(test_video, 20:22)

  expect_type(result, "character")
  expect_length(result, 3)
  expect_match(result[1], "frame_20\\.jpg$")
  expect_match(result[2], "frame_21\\.jpg$")
  expect_match(result[3], "frame_22\\.jpg$")
})

test_that("get_image handles non-consecutive frames correctly", {
  skip_if_not_installed("av")

  result <- get_image(test_video, c(20, 30, 40))

  expect_type(result, "character")
  expect_length(result, 3)
  expect_match(result[1], "frame_20\\.jpg$")
  expect_match(result[2], "frame_30\\.jpg$")
  expect_match(result[3], "frame_40\\.jpg$")
})

test_that("get_image returns empty character vector on failure", {
  # Test with non-existent file
  result <- get_image("nonexistent.mp4", 20)
  expect_type(result, "character")
  expect_length(result, 0)
})

test_that("get_image cleans up existing files", {
  skip_if_not_installed("av")

  # Create some dummy files
  temp_dir <- tempdir()
  dummy_files <- file.path(temp_dir, c("frame_1.jpg", "frame_2.jpg"))
  lapply(dummy_files, writeLines, text = "dummy content")

  # Run get_image
  result <- get_image(test_video, 20)

  # Check that old files are gone
  expect_false(any(file.exists(dummy_files)))
})

test_that("silent parameter controls message output", {
  skip_if_not_installed("av")

  # Test with silent = FALSE
  expect_message(
    get_image(test_video, 20, silent = FALSE),
    "Extracting 1 frame"
  )

  # Test with silent = TRUE (default)
  expect_no_message(
    get_image(test_video, 20)
  )
})

test_that("get_image validates inputs", {
  skip_if_not_installed("av")

  # Test with invalid frames
  expect_error(get_image(test_video, "not a number"))
  expect_error(get_image(test_video, -1))
  expect_error(get_image(test_video, NULL))

  # Test with invalid file path
  expect_error(get_image(NULL, 20))
  expect_error(get_image(NA, 20))
})

# Clean up
unlink(test_video)
