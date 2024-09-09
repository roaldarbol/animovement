#' Read trackball data
#'
#' @description
#' Read trackball data.
#'
#' @param filepaths To file paths, one for each sensor
#' @param setup Which type of experimental setup was used. Expects either `of_free`, `of_fixed` or `fictrac` (soon).
#' @param sampling_rate Sampling rate tells the function how long time it should integrate over. A sampling rate of 60(Hz) will mean windows of 1/60 sec are used to integrate over.
#' @param col_dx Column name for x-axis values
#' @param col_dy Column name for y-axis values
#' @param col_time Which column contains the information about time. Can be specified either by the column number (numeric) or the name of the column if it has one (character). Should either be a datetime (POSIXt) or seconds (numeric).
#' @param distance_scale If using computer mice, you might be getting unit-less data out. However, computer mice have a factor called "dots-per-cm", which you can use to convert your estimates into centimeters.
#' @param distance_unit Which unit should be used. If `distance_scale` is also used, the unit will be for the scaled data. E.g. for trackball data with optical flow sensors, you can use the mouse dots-per-cm (dpcm) of 394 by setting `distance_unit = "cm"` and `distance_scale = 394`.
#'
#' @import dplyr
#' @importFrom vroom vroom
#' @importFrom collapse fmean
#' @importFrom cli cli_abort
#'
#' @return A list of data frames
#' @export
read_trackball <- function(
    filepaths,
    setup = c("of_free", "of_fixed", "fictrac"),
    sampling_rate,
    col_dx = "x",
    col_dy = "y",
    col_time = "time",
    distance_scale = NULL,
    distance_unit = NULL
){
  validate_files(filepaths, expected_suffix = "csv")
  validate_trackball(filepaths, setup)
  n_sensors <- length(filepaths)

  # Read data
  if (n_sensors == 2){
    data_list <- list()
    for (i in 1:n_sensors){
      data_list[[i]] <- read_opticalflow(filepaths[i], col_time) |>
        dplyr::mutate(sensor_n = i)
    }
    df <- join_trackball_files(data_list)
  } else {
    df <- read_opticalflow(filepaths[i], col_time)
  }

  df <- df |>
      compute_xy_coordinates(setup, n_sensors)

  # Scale distance and time and select output columns
  df <- df |>
    dplyr::mutate(keypoint = "centroid") |>
    .scale_values(c("x", "y", "dx", "dy"), distance_scaling) |>
    dplyr::mutate(time = time / sampling_rate) |>
    dplyr::select(time, keypoint, x, y, dx, dy)

  return(df)
}

#' Read optical flow sensor file
#' @description Read optical flow sensor data
#' @inheritParams read_trackball
read_opticalflow <- function(path, col_time){
  # Read file
  if (.file_has_headers(path)){
    data <- vroom::vroom(
      path,
      delim = ",",
      show_col_types = FALSE) |>
      suppressMessages()
  } else {
  data <- vroom::vroom(
    path,
    skip = 1,
    delim = ",",
    show_col_types = FALSE,
    .name_repair = "unique") |>
    suppressMessages()
  }

  # Change column names
  data <- data |>
    dplyr::rename("dx" := 1) |>
    dplyr::rename("dy" := 2) |>
    dplyr::rename("time" := all_of(col_time))

  # If time is a datetime stamp, convert it into seconds from start
  if (.is.POSIXt(data$time) == TRUE){
    data <- data |>
      mutate(time = as.numeric(time))
  }
  return(data)
}

#' Join data files with non-matching time stamps
#' @description Join data files with non-matching time stamps
#' @inheritParams read_trackball
join_trackball_files <- function(data_list, sampling_rate){
  ## Find shared time frame between both sensors
  highest_min_time <- max(c(min(data_list[[1]]$time), min(data_list[[2]]$time)))
  lowest_max_time <- min(c(max(data_list[[1]]$time), max(data_list[[2]]$time)))
  data_list[[1]] <- filter(data_list[[1]], "time" > highest_min_time & "time" < lowest_max_time)
  data_list[[2]] <- filter(data_list[[2]], "time" > highest_min_time & "time" < lowest_max_time)

  # We use the provided sampling rate to create shared a shared time frame
  data_list[[1]] <- data_list[[1]] |>
    dplyr::mutate(time = as.numeric(.data$time - highest_min_time)) |>
    dplyr::filter(time > 0) |>
    dplyr::mutate(time_group = floor(time * sampling_rate)) |>
    dplyr::group_by(time_group) |>
    dplyr::summarise(x = sum(dx),
              y = sum(dy))
  data_list[[2]] <- data_list[[2]] |>
    mutate(time = as.numeric(time - highest_min_time)) |>
    filter(time > 0) |>
    mutate(time_group = floor(time * sampling_rate)) |>
    group_by(time_group) |>
    summarise(x = sum(dx),
              y = sum(dy))

  # We then merge the two data frames
  df <- full_join(
    data_list[[1]], data_list[[2]],
    by = "time_group",
    suffix = c("_1", "_2")
  ) |>
    dplyr::mutate(x_1 = if_else(is.na(x_1), 0, x_1),
                  x_2 = if_else(is.na(x_2), 0, x_2),
                  y_1 = if_else(is.na(y_1), 0, y_1),
                  y_2 = if_else(is.na(y_2), 0, y_2))

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
    dplyr::arrange(time_group)
}

#' @inheritParams read_trackball
compute_xy_coordinates <- function(data, setup, n_sensors){
  # Convert time back to seconds
  if (setup == "of_free"){
    df <- df |>
      dplyr::rename(
        time = time_group,
        dx = y_1,
        dy = y_2)
  } else if (setup == "of_fixed" & n_sensors == 2){
    df <- df |>
      dplyr::rename(time = time_group) |>
      dplyr::mutate(dx = collapse::fmean(c(x_1, x_2)), # Takes the mean of the x reading on both sensors
                    dy = y_1)
  } else if (setup == "of_fixed" & n_sensors == 1){
    df <- df |>
      dplyr::rename(time = time_group)
  }

  df <- df |>
    dplyr::mutate(x = cumsum(dx),
                  y = cumsum(dy)) |>
    dplyr::relocate(time, .before = 1)
  return(df)
}


