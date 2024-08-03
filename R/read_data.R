#' Read Trackball Data
#'
#' @param format Format of data files (default: csv)
#' @param configuration Open- or closed-loop configuration
#' @param filepaths To file paths, one for each sensor
#'
#' @import dplyr
#' @importFrom vroom vroom
#' @importFrom stats median
#' @importFrom cli cli_abort
#'
#' @return A list of data frames
#' @export
read_trackball_data <- function(
    filepaths,
    configuration = c("free", "fixed"),
    format = c("csv", "RDS")
){
  # Check whether there are two files paths
  if (length(filepaths) != 2){
    cli::cli_abort("Incorrect number of file paths. Please provide 2 filepaths.")
  }

  if (configuration == "free"){
    data_list <- list()

    # Read each data frame
    for (i in 1:length(filepaths)){
      n <- as.numeric(i)
      data_list[[i]] <- vroom::vroom(
        filepaths[i],
        skip = 1,
        show_col_types = FALSE
      ) |>
        dplyr::rename("x_{{ n }}" := 1) |>
        dplyr::rename("y_{{ n }}" := 2) |>
        dplyr::rename("time_{{ n }}" := 3) |>
        dplyr::rename("datetime_{{ n }}" := 4) |>
        dplyr::rename(.data$time_diff := 5) |>
        dplyr::select(-.data$time_diff) |>
        dplyr::mutate(sensor_n = i)
    }

    # Merge the two data frames
    ## Find the offset between the two sensors
    which_max_rows <- which.max(c(nrow(data_list[[1]]), nrow(data_list[[2]])))
    min_rows <- min(c(nrow(data_list[[1]]), nrow(data_list[[2]])))
    rows_off <- c()
    for (i in 1:min_rows){
      rows_off <- c(rows_off, .find_nearest(data_list[[1]]$time_1[i], data_list[[2]]$time_2) - i)
    }

    # Here we'll use the median offset, but we can get more out of the data by finding all the best alignments
    rows_off_median <- stats::median(rows_off)
    data_list[[which_max_rows]] <- dplyr::slice(data_list[[which_max_rows]], 1:min_rows)

    # We then merge the two data frames
    data <- dplyr::bind_cols(data_list[[1]], data_list[[2]]) |> suppressMessages()
    data <- data |>
      dplyr::select(.data$y_1, .data$y_2, .data$time_1, .data$time_2, .data$datetime_1, .data$datetime_2) |>
      dplyr::rename(x = .data$y_1) |>
      dplyr::rename(y = .data$y_2) |>
      dplyr::rename(time_x = .data$time_1) |>
      dplyr::rename(time_y = .data$time_2) |>
      dplyr::rename(datetime_x = .data$datetime_1) |>
      dplyr::rename(datetime_y = .data$datetime_2)

    # Process to align the data - can come up with a few algorithms
    # ...
  }

  return(data)
}

.find_nearest <- function(x, v){ which.min(abs(v - x)) }
