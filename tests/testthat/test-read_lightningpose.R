path_single <- here::here("tests", "data", "lightningpose", "mouse_single.csv")
path_twoview <- here::here("tests", "data", "lightningpose", "mouse_twoview.csv")

# Read file
## Single animal
test_that("Read single-animal file", {
  expect_no_error(
    read_lightningpose(path_single)
  )
  expect_contains(
    read_lightningpose(path_single) |>
      names(),
    c("time", "keypoint", "x", "y", "confidence")
  )
})

## Two-view
test_that("Read multi-animal file", {
  expect_no_error(
    read_lightningpose(path_twoview)
  )
  expect_contains(
    read_lightningpose(path_twoview) |>
      names(),
    c("time", "keypoint", "x", "y", "confidence")
  )
})
