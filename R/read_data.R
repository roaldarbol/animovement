#' Read Trackball Data
#'
#' @param filepaths To file paths, one for each sensor
#' @param configuration Open- or closed-loop configuration
#' @param time_col Which column contains the information about time. Can be specified either by the column number (numeric) or the name of the column if it has one (character). Should either be a datetime (POSIXt) or seconds (numeric).
#' @param sampling_rate Sampling rate tells the function how long time it should intergrate over. A sampling rate of 60(Hz) will mean windows of 1/60 sec are used to integrate over.
#' @param mouse_dpcm If using computer mice, you might be getting unit-less data out. However, computer mice have a factor called "dots-per-cm", which you can use to convert your estimates into centimeters.
#'
#' @import dplyr
#' @importFrom vroom vroom
#' @importFrom stats median
#' @importFrom cli cli_abort
#'
#' @return A list of data frames
#' @export
read_trackball <- function(
    filepaths,
    configuration = c("free", "fixed"),
    time_col,
    sampling_rate,
    mouse_dpcm = NULL
){
  # CHECKS
  ## Check whether there are two files paths
  if (length(filepaths) != 2){
    cli::cli_abort("Incorrect number of file paths. Please provide 2 filepaths.")
  }

  ## Check file extension
  file_exts <- c(.get_file_ext(filepaths[1]), .get_file_ext(filepaths[2]))
  if (file_exts[1] != file_exts[2]){
    cli::cli_abort("Files have different extension. Please provide 2 files of the same format.")
  }

  if (!file_exts[1] %in% c("csv", "rds")){
    cli::cli_abort("Incorrect file format. Use either csv or rds files.")
  }

  ## Check configuration
  if (!configuration %in% c("free", "fixed")){
    cli::cli_abort("Incorrect configuration. Configuration needs to be either \"free\" or \"fixed\".")
  }

  ## Check time assignment
  if (is.null(time_col) & is.null(sampling_rate)){
    ## Should be updated. Not quite sure how that works right now.
    cli::cli_abort("No way of assigning time. Please provide either `time_col` or `sampling_rate`")
  }

  # Read data
  if (configuration == "free"){
    data_list <- list()

    # Read each data frame
    for (i in 1:length(filepaths)){
      n <- as.numeric(i)

      if (is.character(time_col)){
        data_list[[i]] <- vroom::vroom(
          filepaths[i],
          show_col_types = FALSE)
      } else if (is.numeric(time_col)){
        data_list[[i]] <- vroom::vroom(
          filepaths[i],
          skip = 1,
          show_col_types = FALSE)
      }

      data_list[[i]] <- data_list[[i]] |>
        dplyr::rename("x" := 1) |>
        dplyr::rename("y" := 2) |>
        dplyr::mutate(sensor_n = i)

      if (is.character(time_col)){
        data_list[[i]] <- data_list[[i]] |>
          dplyr::rename("time" := all_of(time_col))
      } else if (is.numeric(time_col)){
        data_list[[i]] <- data_list[[i]] |>
          dplyr::rename("time" := all_of(time_col))
      }

      # If time is a datetime stamp, convert it into seconds from start
      time_posixt <- .is.POSIXt(data_list[[i]]$time)
      if (time_posixt == TRUE){
        data_list[[i]] <- data_list[[i]] |>
          mutate(time = as.numeric(time))
      }
    }

    # Merge the two data frames
    ## Find the offset between the two sensors for every single frame
    # which_max_rows <- which.max(c(nrow(data_list[[1]]), nrow(data_list[[2]])))
    # which_min_rows <- which.min(c(nrow(data_list[[1]]), nrow(data_list[[2]])))
    # min_rows <- min(c(nrow(data_list[[1]]), nrow(data_list[[2]])))
    # nearest_rows <- c()
    # for (i in 1:min_rows){
    #   nearest_rows <- c(nearest_rows, .find_nearest(data_list[[which_min_rows]]$time[i], data_list[[which_max_rows]]$time))
    # }

    ## Find shared timeframe between both sensors
    highest_min_time <- max(c(min(data_list[[1]]$time), min(data_list[[2]]$time)))
    lowest_max_time <- min(c(max(data_list[[1]]$time), max(data_list[[2]]$time)))
    data_list[[1]] <- filter(data_list[[1]], time > highest_min_time & time < lowest_max_time)
    data_list[[2]] <- filter(data_list[[2]], time > highest_min_time & time < lowest_max_time)

    # We use the provided sampling rate to create shared a shared time frame
    data_list[[1]] <- data_list[[1]] |>
      mutate(time = as.numeric(time - highest_min_time)) |>
      filter(time > 0) |>
      mutate(time_group = floor(time * sampling_rate)) |>
      group_by(time_group) |>
      summarise(x = sum(x),
                y = sum(y))
    data_list[[2]] <- data_list[[2]] |>
      mutate(time = as.numeric(time - highest_min_time)) |>
      filter(time > 0) |>
      mutate(time_group = floor(time * sampling_rate)) |>
      group_by(time_group) |>
      summarise(x = sum(x),
                y = sum(y))

    # We then merge the two data frames
    df <- full_join(
      data_list[[1]], data_list[[2]],
      by = "time_group",
      suffix = c("_1", "_2")
      ) |>
      dplyr::select("y_1", "y_2", "time_group") |>
      dplyr::mutate(y_1 = if_else(is.na(y_1), 0, y_1),
                    y_2 = if_else(is.na(y_2), 0, y_2))

    # Some times do not have any sensor data, so we add those in with zeros
    min_t <- min(df$time_group)
    max_t <- max(df$time_group)
    full_t_seq <- seq(from = min_t, to = max_t, by = 1)
    missing_times <- tibble(
      time_group = setdiff(full_t_seq, df$time_group),
      y_1 = 0,
      y_2 = 0
      )

    df <- dplyr::bind_rows(df, missing_times) |>
      dplyr::arrange(time_group)

    # Convert time back to seconds
    df <- df |>
      dplyr::rename(time = time_group,
                    dx = y_1,
                    dy = y_2) |>
      dplyr::mutate(x = cumsum(dx),
                    y = cumsum(dy)) |>
      dplyr::relocate(time, .before = 1) |>
      dplyr::mutate(time = time / sampling_rate) |>
      dplyr::select(x, y, time)

    # Adjust distances for mouse sensor "dots-per-cm"
    if (!is.null(mouse_dpcm)){
      df <- df |>
        dplyr::mutate(x = x / mouse_dpcm,
                      y = y / mouse_dpcm)
    }
  }

  return(df)
}

.find_nearest <- function(x, v){ which.min(abs(v - x)) }

.get_file_ext <- function(filename) {
  nameSplit <- strsplit(x = filename, split = "\\.")[[1]]
  return(nameSplit[length(nameSplit)])
}

.is.POSIXt <- function(x) inherits(x, "POSIXt")
