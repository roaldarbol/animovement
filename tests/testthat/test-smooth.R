# Test arguments

# Paths to test data
path_animalta_raw <- here::here("tests", "data", "animalta", "single_individual_multi_arena.csv")
path_animalta_detailed <- here::here("tests", "data", "animalta", "variable_individuals_single_arena.csv")
path_bonsai <- here::here("tests", "data", "bonsai", "LI850.csv")
path_dlc_single <- here::here("tests", "data", "deeplabcut", "mouse_single.csv")
path_dlc_multi <- here::here("tests", "data", "deeplabcut", "mouse_multi.csv")
path_idtracker_h5 <- here::here("tests", "data", "idtrackerai", "trajectories.h5")
path_idtracker_csv <- here::here("tests", "data", "idtrackerai", "trajectories_csv", "trajectories.csv")
path_idtracker_csv_probabilities <- here::here("tests", "data", "idtrackerai", "trajectories_csv", "id_probabilities.csv")
path_lightningpose_single <- here::here("tests", "data", "lightningpose", "mouse_single.csv")
path_lightningpose_twoview <- here::here("tests", "data", "lightningpose", "mouse_twoview.csv")
path_sleap_single <- here::here("tests", "data", "sleap", "SLEAP_single-mouse_EPM.analysis.h5")
path_sleap_multi <- here::here("tests", "data", "sleap", "SLEAP_three-mice_Aeon_mixed-labels.analysis.h5")
path_trex <- here::here("tests", "data", "trex", "beetle.csv")
path_trackball_1 <- here::here("tests", "data", "single", "opticalflow_sensor_1.csv")
path_trackball_2 <- here::here("tests", "data", "single", "opticalflow_sensor_2.csv")

# Read data snippets
df_animalta_raw <- read_animalta(path_animalta_raw, detailed = FALSE)
df_animalta_detailed <- read_animalta(path_animalta_detailed, detailed = TRUE)
df_bonsai <- read_bonsai(path_bonsai)
df_dlc_single <- read_deeplabcut(path_dlc_single)
df_dlc_multi <- read_deeplabcut(path_dlc_multi)
df_idtracker_h5 <- read_idtracker(path_idtracker_h5)
df_idtracker_csv <- read_idtracker(path_idtracker_csv, path_idtracker_csv_probabilities)
df_lightningpose_single <- read_lightningpose(path_lightningpose_single)
df_lightningpose_twoview <- read_lightningpose(path_lightningpose_twoview)
df_sleap_single <- read_sleap(path_sleap_single)
df_sleap_multi <- read_sleap(path_sleap_multi)
df_trex <- read_trex(path_trex)
df_trackball <- read_trackball(
  paths = c(path_trackball_1, path_trackball_2),
  setup = "of_free",
  sampling_rate = 60,
  col_time = 4,
  distance_scale = 394,
  distance_unit = NULL
)

# Check that smoothing functions work
test_that("Test output header names", {
  expect_no_error(smooth_movement(df_animalta_raw))
  expect_no_error(smooth_movement(df_animalta_detailed))
  expect_no_error(smooth_movement(df_bonsai))
  expect_no_error(smooth_movement(df_dlc_single))
  expect_no_error(smooth_movement(df_dlc_multi))
  expect_no_error(smooth_movement(df_idtracker_h5))
  expect_no_error(smooth_movement(df_idtracker_csv))
  expect_no_error(smooth_movement(df_lightningpose_single))
  expect_no_error(smooth_movement(df_lightningpose_twoview))
  expect_no_error(smooth_movement(df_sleap_single))
  expect_no_error(smooth_movement(df_sleap_multi))
  expect_no_error(smooth_movement(df_trex))
  expect_no_error(smooth_movement(df_trackball, use_derivatives = TRUE))
})
