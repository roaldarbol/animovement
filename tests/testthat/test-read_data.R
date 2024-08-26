# Test arguments
filepaths <- system.file("extdata", package = "trackballr") |> list.files(full.names = TRUE)

test_that("Correct configuration", {
  expect_no_error(read_trackball_data(filepaths, configuration = "free"))
  expect_error(read_trackball_data(filepaths, configuration = "open"))
})
