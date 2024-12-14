# Test arguments

# Paths to test data
path_animalta_raw <- testthat::test_path("data", "animalta", "single_individual_multi_arena.csv")
path_animalta_detailed <- testthat::test_path("data", "animalta", "variable_individuals_single_arena.csv")
path_bonsai <- testthat::test_path("data", "bonsai", "LI850.csv")
path_dlc_single <- testthat::test_path("data", "deeplabcut", "mouse_single.csv")
path_dlc_multi <- testthat::test_path("data", "deeplabcut", "mouse_multi.csv")
path_idtracker_csv <- testthat::test_path("data", "idtrackerai", "trajectories_csv", "trajectories.csv")
path_idtracker_csv_probabilities <- testthat::test_path("data", "idtrackerai", "trajectories_csv", "id_probabilities.csv")
path_lightningpose_single <- testthat::test_path("data", "lightningpose", "mouse_single.csv")
path_lightningpose_twoview <- testthat::test_path("data", "lightningpose", "mouse_twoview.csv")
path_trex <- testthat::test_path("data", "trex", "beetle.csv")

# File headers (not including h5 files)
test_that("Test output header names", {
  expect_no_error(ensure_file_has_headers(path_animalta_raw))
  expect_no_error(ensure_file_has_headers(path_animalta_detailed))
  expect_no_error(ensure_file_has_headers(path_bonsai))
  expect_no_error(ensure_file_has_headers(path_dlc_single))
  expect_no_error(ensure_file_has_headers(path_dlc_multi))
  expect_no_error(ensure_file_has_headers(path_idtracker_csv))
  expect_no_error(ensure_file_has_headers(path_lightningpose_single))
  expect_no_error(ensure_file_has_headers(path_lightningpose_twoview))
  expect_no_error(ensure_file_has_headers(path_trex))
})
