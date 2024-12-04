# Test arguments

# Paths to test data
path_animalta_roi <- here::here("tests", "data", "animalta", "single_individual_multi_arena.csv")
path_animalta_no_roi <- here::here("tests", "data", "animalta", "variable_individuals_single_arena.csv")
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
df_animalta_roi <- read_animalta(path_animalta_roi, with_roi = TRUE)
df_animalta_no_roi <- read_animalta(path_animalta_no_roi, with_roi = FALSE)
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

# Check output header names
test_that("Test output header names", {
  expect_no_error(ensure_output_header_names(df_animalta_roi))
  expect_no_error(ensure_output_header_names(df_animalta_no_roi))
  expect_no_error(ensure_output_header_names(df_bonsai))
  expect_no_error(ensure_output_header_names(df_dlc_single))
  expect_no_error(ensure_output_header_names(df_dlc_multi))
  expect_no_error(ensure_output_header_names(df_idtracker_h5))
  expect_no_error(ensure_output_header_names(df_idtracker_csv))
  expect_no_error(ensure_output_header_names(df_lightningpose_single))
  expect_no_error(ensure_output_header_names(df_lightningpose_twoview))
  expect_no_error(ensure_output_header_names(df_sleap_single))
  expect_no_error(ensure_output_header_names(df_sleap_multi))
  expect_no_error(ensure_output_header_names(df_trex))
  expect_no_error(ensure_output_header_names(df_trackball))
})

# Check output header classes
test_that("Test output header classes", {
  expect_no_error(ensure_output_header_class(df_animalta_roi))
  expect_no_error(ensure_output_header_class(df_animalta_no_roi))
  expect_no_error(ensure_output_header_class(df_bonsai))
  expect_no_error(ensure_output_header_class(df_dlc_single))
  expect_no_error(ensure_output_header_class(df_dlc_multi))
  expect_no_error(ensure_output_header_class(df_idtracker_h5, expected_header_class = c("integer", "factor", "factor", "numeric", "numeric", "numeric")))
  expect_no_error(ensure_output_header_class(df_idtracker_csv))
  expect_no_error(ensure_output_header_class(df_lightningpose_single))
  expect_no_error(ensure_output_header_class(df_lightningpose_twoview))
  expect_no_error(ensure_output_header_class(df_sleap_single, expected_header_class = c("integer", "factor", "factor", "numeric", "numeric", "numeric")))
  expect_no_error(ensure_output_header_class(df_sleap_multi, expected_header_class = c("integer", "factor", "factor", "numeric", "numeric", "numeric")))
  expect_no_error(ensure_output_header_class(df_trex))
  expect_no_error(ensure_output_header_class(df_trackball))
})


# Check that metadata has been initiated
test_that("Test that metadata has been initiated", {
  expect_no_error(ensure_metadata_exists(df_animalta_roi))
  expect_no_error(ensure_metadata_exists(df_animalta_no_roi))
  expect_no_error(ensure_metadata_exists(df_bonsai))
  expect_no_error(ensure_metadata_exists(df_dlc_single))
  expect_no_error(ensure_metadata_exists(df_dlc_multi))
  expect_no_error(ensure_metadata_exists(df_idtracker_h5))
  expect_no_error(ensure_metadata_exists(df_idtracker_csv))
  expect_no_error(ensure_metadata_exists(df_lightningpose_single))
  expect_no_error(ensure_metadata_exists(df_lightningpose_twoview))
  expect_no_error(ensure_metadata_exists(df_sleap_single))
  expect_no_error(ensure_metadata_exists(df_sleap_multi))
  expect_no_error(ensure_metadata_exists(df_trex))
  expect_no_error(ensure_metadata_exists(df_trackball))
})

# Test that there are no NaN's in data
df_animalta_roi_NaN <- df_animalta_roi |>
  mutate(x = NaN)
test_that("Test that there aren't any NaNs created", {
  expect_no_warning(ensure_output_no_nan(df_animalta_roi))
  expect_no_warning(ensure_output_no_nan(df_animalta_no_roi))
  expect_no_warning(ensure_output_no_nan(df_bonsai))
  expect_no_warning(ensure_output_no_nan(df_dlc_single))
  expect_no_warning(ensure_output_no_nan(df_dlc_multi))
  expect_no_warning(ensure_output_no_nan(df_idtracker_h5))
  expect_no_warning(ensure_output_no_nan(df_idtracker_csv))
  expect_no_warning(ensure_output_no_nan(df_lightningpose_single))
  expect_no_warning(ensure_output_no_nan(df_lightningpose_twoview))
  expect_no_warning(ensure_output_no_nan(df_sleap_single))
  expect_no_warning(ensure_output_no_nan(df_sleap_multi))
  expect_no_warning(ensure_output_no_nan(df_trex))
  expect_no_warning(ensure_output_no_nan(df_trackball))
  expect_warning(ensure_output_no_nan(df_animalta_roi_NaN))
})
