path_single <- here::here("tests", "data", "sleap", "SLEAP_single-mouse_EPM.analysis.h5")
path_multi <- here::here("tests", "data", "sleap", "SLEAP_three-mice_Aeon_mixed-labels.analysis.h5")

# Read file
## Single animal
test_that("Read single-animal file", {
  expect_no_error(
    read_sleap(path_single)
  )
  expect_contains(
    read_sleap(path_single) |>
      names(),
    c("time", "keypoint", "x", "y", "confidence")
  )
})

## Multi-animal
test_that("Read multi-animal file", {
  expect_no_error(
    read_sleap(path_multi)
  )
  expect_contains(
    read_sleap(path_multi) |>
      names(),
    c("time", "individual", "keypoint", "x", "y", "confidence")
  )
})

