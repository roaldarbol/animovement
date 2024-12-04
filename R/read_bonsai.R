#' Read centroid tracking data from Bonsai
#'
#' @description
#'
#' @param path Path to a Bonsai data file
#' @import dplyr
#' @import tidyselect
#' @importFrom vroom vroom
#'
#' @return a movement dataframe
#' @export
read_bonsai <- function(path) {
  # There can be tracking from multiple ROIs at the same time
  # We need to check everything matches expectations
  # We should be able to use only a single timestamp (should be the same across all ROIs)
  validate_files(path, expected_suffix = "csv")
  data <- vroom::vroom(
    path,
    delim = ",",
    show_col_types = FALSE
    ) |>
    suppressMessages() |>
    dplyr::select(tidyselect::contains(c("Timestamp", "Centroid"))) |>
    dplyr::rename(time = tidyselect::contains("Timestamp"),
                  x = tidyselect::contains("X"),
                  y = tidyselect::contains("Y")) |>
    dplyr::mutate(keypoint = factor("centroid"),
                  individual = factor(NA),
                  confidence = as.numeric(NA)) |>
    dplyr::relocate("keypoint", .after = "time") |>
    dplyr::relocate("individual", .after = "time") |>
    convert_nan_to_na()

  attributes(data)$spec <- NULL
  attributes(data)$problems <- NULL

  # Metadata
  data <- data |>
    init_metadata() |>
    set_start_datetime(data$time[[1]]) |>
    dplyr::mutate(time = as.numeric(.data$time - min(.data$time, na.rm = TRUE)))
  return(data)
}
