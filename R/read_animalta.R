#' Read AnimalTA data
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' @param path An AnimalTA data frame
#' @param with_roi Were one or more ROIs used?
#'
#' @import dplyr
#' @import vroom
#' @importFrom janitor clean_names
#'
#' @return a movement dataframe
#' @export
read_animalta <- function(path, with_roi = FALSE) {
  # Inspect headers
  if (with_roi == FALSE){
    validate_files(
      path,
      expected_suffix = "csv",
      expected_headers = c("X", "Y", "Time")
      )
    data <- read_animalta_no_roi(path)
  } else {
    validate_files(
      path,
      expected_suffix = "csv",
      expected_headers = c("Time", "X_Arena0_Ind0", "Y_Arena0_Ind0")
    )
    data <- read_animalta_with_roi(path)
  }
  data <- data |>
    dplyr::mutate(keypoint = factor("centroid")) |>
    dplyr::relocate("keypoint", .after = "individual")
  return(data)
}

#' @inheritParams read_animalta
#' @keywords internal
read_animalta_no_roi <- function(path){
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
read_animalta_with_roi <- function(path){
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
                       names_from = .data$coordinate,
                       values_from = .data$val) |>
    tidyr::unite("individual", c("individual", "arena")) |>
    dplyr::mutate(individual = factor(.data$individual))
  return(data)
}
