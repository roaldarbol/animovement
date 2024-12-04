path_csv <- here::here("tests", "data", "idtrackerai", "trajectories_csv", "trajectories.csv")
path_probabilities <- here::here("tests", "data", "idtrackerai", "trajectories_csv", "id_probabilities.csv")
path_h5 <- here::here("tests", "data", "idtrackerai", "trajectories.h5")

# Read file
## Single animal
test_that("Read file", {
  expect_no_error(
    read_idtracker(path_csv)
  )
  expect_no_error(
    read_idtracker(path_csv, path_probabilities)
  )
  expect_warning(
    read_idtracker(path_h5, path_probabilities)
  )
  expect_contains(
    read_idtracker(path_csv) |>
      names(),
    c("time", "individual", "keypoint", "x", "y")
  )
  expect_contains(
    read_idtracker(path_csv, path_probabilities) |>
      names(),
    c("time", "individual", "keypoint", "x", "y", "confidence")
  )
  expect_contains(
    read_idtracker(path_h5) |>
      names(),
    c("time", "individual", "keypoint", "x", "y", "confidence")
  )
})
