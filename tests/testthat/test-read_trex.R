library(here)

path <- here::here("tests", "data", "trex", "beetle.csv")

# Read file
test_that("Read TRex file", {
  expect_no_error(
    read_trex(path)
  )
  expect_contains(
    read_trex(path) |>
      names(),
    c("time", "keypoint", "x", "y")
  )
})
