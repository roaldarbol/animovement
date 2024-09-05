# Test arguments
library(stringr)
filepaths <- system.file("extdata/single", package = "trackballr") |>
  list.files(full.names = TRUE)

test_that("Correct configuration", {
  expect_no_error(read_trackball(
    filepaths[3:4],
    configuration = "free",
    time_col = 4,
    sampling_rate = 60)
    )
  expect_error(read_trackball(
    filepaths[3:4],
    configuration = "open",
    time_col = 4,
    sampling_rate = 60)
  )
})

test_that("Correct time specification", {
  expect_error(read_trackball(
    filepaths[3:4],
    configuration = "free",
    time_col = 3, # is a numeric but not seconds
    sampling_rate = 60)
  )
  expect_no_error(read_trackball(
    filepaths[3:4],
    configuration = "free",
    time_col = 4, # is a datetime stamp
    sampling_rate = 60)
  )
  # Test specification of time column by name
  expect_no_error(read_trackball(
    filepaths[1:2],
    configuration = "free",
    time_col = "datetime", # is a datetime stamp
    sampling_rate = 60)
  )
})
