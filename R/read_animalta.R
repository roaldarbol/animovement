#' Read AnimalTA data
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' @param path An AnimalTA data frame
#' @param detailed Animal export either raw (default) or detailed data files. We
#'   only have limited support for detailed data.
#'
#' @import dplyr
#' @import vroom
#' @importFrom janitor clean_names
#'
#' @return a movement dataframe
#' @export
read_animalta <- function(path, detailed = FALSE) {
  # Inspect headers
  if (detailed == TRUE){
    validate_files(
      path,
      expected_suffix = "csv",
      expected_headers = c("X", "Y", "Time")
      )
    data <- read_animalta_detailed(path)
  } else {
    validate_files(
      path,
      expected_suffix = "csv",
      expected_headers = c("Time", "X_Arena0_Ind0", "Y_Arena0_Ind0")
    )
    data <- read_animalta_raw(path)
  }
  data <- data |>
    dplyr::mutate(keypoint = factor("centroid")) |>
    dplyr::relocate("keypoint", .after = "individual")
  return(data)
}

#' @inheritParams read_animalta
#' @keywords internal
read_animalta_detailed <- function(path){
  data <- vroom::vroom(
    path,
    delim = ";",
    show_col_types = FALSE
  ) |>
  janitor::clean_names() |>
    dplyr::mutate(frame = as.numeric(.data$frame),
                  time = as.numeric(.data$time)) |>
    dplyr::rename(individual = "ind") |>
    dplyr::mutate(individual = factor(.data$individual)) |>
    dplyr::select(-c("frame", "arena"))
  attributes(data)$spec <- NULL
  attributes(data)$problems <- NULL
  return(data)
}

#' @inheritParams read_animalta
#' @import tidyr
#' @keywords internal
read_animalta_raw <- function(path){
  data <- vroom::vroom(
    path,
    delim = ";",
    show_col_types = FALSE
  ) |>
    janitor::clean_names()

  data <- data |>
    tidyr::pivot_longer(cols = 3:ncol(data),
                        names_to = c("coordinate", "individual", "arena"),
                        names_sep = "_",
                        values_to = "val") |>
    tidyr::pivot_wider(id_cols = c("time", "individual", "arena"),
                       names_from = "coordinate",
                       values_from = "val") |>
    tidyr::unite("individual", c("individual", "arena")) |>
    dplyr::mutate(individual = factor(.data$individual))
  return(data)
}
