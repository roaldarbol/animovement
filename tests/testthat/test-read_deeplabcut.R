path_single <- here::here("tests", "data", "deeplabcut", "mouse_single.csv")
path_multi <- here::here("tests", "data", "deeplabcut", "mouse_multi.csv")

# Read file
## Single animal
test_that("Read single-animal file", {
  expect_no_error(
    read_deeplabcut(path_single)
  )
  expect_contains(
    read_deeplabcut(path_single) |>
      names(),
    c("time", "keypoint", "x", "y", "confidence")
  )
})

## Multi-animal
test_that("Read multi-animal file", {
  expect_no_error(
    read_deeplabcut(path_multi)
  )
  expect_contains(
    read_deeplabcut(path_multi) |>
      names(),
    c("time", "individual", "keypoint", "x", "y", "confidence")
  )
})
