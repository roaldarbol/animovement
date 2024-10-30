#' Read trackball data
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Read trackball data from a variety of setups and configurations.
#'
#' @param paths Two file paths, one for each sensor (although one is allowed for a fixed setup, `of_fixed`).
#' @param setup Which type of experimental setup was used. Expects either `of_free`, `of_fixed` or `fictrac` (soon).
#' @param sampling_rate Sampling rate tells the function how long time it should integrate over. A sampling rate of 60(Hz) will mean windows of 1/60 sec are used to integrate over.
#' @param col_time Which column contains the information about time. Can be specified either by the column number (numeric) or the name of the column if it has one (character). Should either be a datetime (POSIXt) or seconds (numeric).
#' @param col_dx Column name for x-axis values
#' @param col_dy Column name for y-axis values
#' @param ball_calibration When running an `of_fixed` experiment, you may (but it is not necessary) provide a calibration factor. This factor is the number recorded after a 360 degree spin. You can use the `calibrate_trackball` function to get this number. Alternatively, provide the `ball_diameter` and a `distance_scale` (e.g. mouse dpcm).
#' @param ball_diameter When running a `of_fixed` experiment, the ball diameter is needed together with either `ball_calibration` or `distance_scale`.
#' @param distance_scale If using computer mice, you might be getting unit-less data out. However, computer mice have a factor called "dots-per-cm", which you can use to convert your estimates into centimeters.
#' @param distance_unit Which unit should be used. If `distance_scale` is also used, the unit will be for the scaled data. E.g. for trackball data with optical flow sensors, you can use the mouse dots-per-cm (dpcm) of 394 by setting `distance_unit = "cm"` and `distance_scale = 394`.
#' @param verbose If `FALSE` (default), suppress most warning messages.
#'
#' @import dplyr
#' @importFrom vroom vroom
#' @importFrom collapse fmean
#' @importFrom cli cli_abort
#' @importFrom stringi stri_rand_strings
#'
#' @return a movement dataframe
#' @export
read_trackball <- function(
    paths,
    setup = c("of_free", "of_fixed", "fictrac"),
    sampling_rate,
    col_time = "time",
    col_dx = "x",
    col_dy = "y",
    ball_calibration = NULL,
    ball_diameter = NULL,
    distance_scale = NULL,
    distance_unit = NULL,
    verbose = FALSE) {
  validate_files(paths, expected_suffix = "csv", expected_headers = c("x", "y", "time"))
  validate_trackball(paths, setup, col_time)
  n_sensors <- length(paths)

  # Read data
  if (n_sensors == 2) {
    data_list <- list()
    for (i in 1:n_sensors) {
      data_list[[i]] <- read_opticalflow(paths[i], col_time) |>
        dplyr::mutate(sensor_n = i)
    }
    df <- join_trackball_files(data_list, sampling_rate)
  } else {
    df <- read_opticalflow(paths[i], col_time)
  }

  # Calculate coordinates (free/fixed)
  if (setup == "of_free") {
    df <- df |>
      compute_xy_coordinates_free()
  } else if (setup == "of_fixed") {
    df <- df |>
      compute_xy_coordinates_fixed(n_sensors, ball_diameter, ball_calibration, distance_scale)
  }

  # Scale distance and time and select output columns
  df <- df |>
    dplyr::mutate(keypoint = "centroid") |>
    .scale_values(c("x", "y", "dx", "dy"), distance_scale) |>
    dplyr::mutate(time = .data$time / sampling_rate) |>
    dplyr::mutate(uid = stringi::stri_rand_strings(1, 20, pattern = "[A-Z0-9]")) |>
    dplyr::select("uid", "time", "keypoint", "x", "y", "dx", "dy")

  return(df)
}

#' Read optical flow sensor file
#' @description Read optical flow sensor data.
#' @param path Path to the file.
#' @inheritParams read_trackball
#' @keywords internal
read_opticalflow <- function(path, col_time, verbose = FALSE) {
  # Read file
  if (ensure_file_has_expected_headers(path, c("x", "y", "time"))) {
    data <- vroom::vroom(
      path,
      delim = ",",
      show_col_types = FALSE
    ) |>
      suppressMessages()
  } else {
    data <- vroom::vroom(
      path,
      skip = 2,
      delim = ",",
      show_col_types = TRUE,
      .name_repair = "unique"
    ) |>
      suppressMessages()
  }

  # Change column names
  data <- data |>
    dplyr::rename("dx" := 1) |>
    dplyr::rename("dy" := 2) |>
    dplyr::rename("time" := all_of(col_time))

  # If time is a datetime stamp, convert it into seconds from start
  # NEEDS TO GO INTO THE TIME VALIDATOR
  if (.is.POSIXt(data$time) == TRUE) {
    data <- data |>
      mutate(time = as.numeric(.data$time))
  } else if (is.character(data$time)) {
    data <- data |>
      mutate(time = as.numeric(as.POSIXct(.data$time)))
  }
  return(data)
}

#' Join data files with non-matching time stamps
#' @description Join data files with non-matching time stamps
#' @param data_list List of 2 dataframes
#' @inheritParams read_trackball
#' @keywords internal
join_trackball_files <- function(data_list, sampling_rate) {
  ## Find shared time frame between both sensors
  highest_min_time <- max(c(min(data_list[[1]]$time), min(data_list[[2]]$time)))
  lowest_max_time <- min(c(max(data_list[[1]]$time), max(data_list[[2]]$time)))
  data_list[[1]] <- filter(data_list[[1]], .data$time > highest_min_time & .data$time < lowest_max_time)
  data_list[[2]] <- filter(data_list[[2]], .data$time > highest_min_time & .data$time < lowest_max_time)

  # We use the provided sampling rate to create shared a shared time frame
  data_list[[1]] <- data_list[[1]] |>
    dplyr::mutate(time = as.numeric(.data$time - highest_min_time)) |>
    dplyr::filter(.data$time > 0) |>
    dplyr::mutate(time_group = floor(.data$time * sampling_rate)) |>
    dplyr::group_by(.data$time_group) |>
    dplyr::summarise(
      x = sum(.data$dx),
      y = sum(.data$dy)
    )
  data_list[[2]] <- data_list[[2]] |>
    mutate(time = as.numeric(.data$time - highest_min_time)) |>
    filter(.data$time > 0) |>
    mutate(time_group = floor(.data$time * sampling_rate)) |>
    group_by(.data$time_group) |>
    summarise(
      x = sum(.data$dx),
      y = sum(.data$dy)
    )

  # We then merge the two data frames
  df <- full_join(
    data_list[[1]], data_list[[2]],
    by = "time_group",
    suffix = c("_1", "_2")
  ) |>
    dplyr::mutate(
      x_1 = if_else(is.na(.data$x_1), 0, .data$x_1),
      x_2 = if_else(is.na(.data$x_2), 0, .data$x_2),
      y_1 = if_else(is.na(.data$y_1), 0, .data$y_1),
      y_2 = if_else(is.na(.data$y_2), 0, .data$y_2)
    )

  # Some times do not have any sensor data, so we add those in with zeros
  min_t <- min(df$time_group)
  max_t <- max(df$time_group)
  full_t_seq <- seq(from = min_t, to = max_t, by = 1)
  missing_times <- tibble(
    time_group = setdiff(full_t_seq, df$time_group),
    x_1 = 0,
    x_2 = 0,
    y_1 = 0,
    y_2 = 0
  )

  df <- dplyr::bind_rows(df, missing_times) |>
    dplyr::arrange(.data$time_group)
  return(df)
}

#' @inheritParams read_trackball
#' @keywords internal
compute_xy_coordinates_free <- function(data) {
  # Convert time back to seconds
  data <- data |>
    dplyr::rename(
      time = "time_group",
      dx = "y_1",
      dy = "y_2"
    )

  data <- data |>
    dplyr::mutate(
      x = cumsum(.data$dx),
      y = cumsum(.data$dy)
    ) |>
    dplyr::relocate("time", .before = 1)
  return(data)
}

#' @inheritParams read_trackball
#' @keywords internal
compute_xy_coordinates_fixed <- function(data, n_sensors, ball_diameter, ball_calibration, distance_scale) {
  if (n_sensors == 2) {
    data <- data |>
      dplyr::rename(time = "time_group") |>
      dplyr::mutate(
        sensor_dx = collapse::fmean(c(.data$x_1, .data$x_2)), # Takes the mean of the x reading on both sensors
        sensor_dy = .data$y_1
      )
  } else if (n_sensors == 1) {
    data <- data |>
      dplyr::rename(
        time = "time_group",
        sensor_dx = .data$x_1,
        sensor_dy = .data$y_1
      )
  }

  # Compute the xy coordinates by calculating the angle turned and displacement in every bin
  if (!is.null(ball_calibration)) {
    data <- data |>
      dplyr::mutate(d_angle = (.data$sensor_dx / ball_calibration) * 2 * pi) # in radians
  } else if (!is.null(distance_scale)) {
    data <- data |>
      dplyr::mutate(d_angle = (.data$sensor_dx / (ball_diameter * pi * distance_scale)) * 2 * pi) # in radians
  }
  data <- data |>
    dplyr::mutate(
      dx = .data$sensor_dy * cos(.data$d_angle),
      dy = .data$sensor_dy * sin(.data$d_angle)
    ) |>
    dplyr::mutate(
      x = cumsum(.data$dx),
      y = cumsum(.data$dy)
    ) |>
    dplyr::relocate("time", .before = 1)
  return(data)
}
