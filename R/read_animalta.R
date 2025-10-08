#' @title Read AnimalTA data
#' @name read_animalta
#'
#' @description Read a data frame from AnimalTA
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
#'
#' @references
#' - Chiara, V., & Kim, S.-Y. (2023). AnimalTA: A highly flexible and easy-to-use
#' program for tracking and analysing animal movement in different environments.
#' *Methods in Ecology and Evolution*, 14, 1699–1707. \doi{0.1111/2041-210X.14115}.
#'
#' @export
read_animalta <- function(path, detailed = FALSE) {
  # Inspect headers
  if (detailed == TRUE) {
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
    dplyr::relocate("keypoint", .after = "individual") |>
    dplyr::mutate(
      confidence = as.numeric(NA),
      keypoint = factor(.data$keypoint),
      individual = factor(.data$individual)
    )

  # Init metadata
  data <- data |>
    init_metadata()

  return(data)
}

#' @inheritParams read_animalta
#' @keywords internal
read_animalta_detailed <- function(path) {
  data <- vroom::vroom(
    path,
    delim = ";",
    show_col_types = FALSE
  ) |>
    janitor::clean_names() |>
    dplyr::mutate(
      frame = as.numeric(.data$frame),
      time = as.numeric(.data$time)
    ) |>
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
read_animalta_raw <- function(path) {
  data <- vroom::vroom(
    path,
    delim = ";",
    show_col_types = FALSE
  ) |>
    janitor::clean_names()

  data <- data |>
    tidyr::pivot_longer(
      cols = 3:ncol(data),
      names_to = c("coordinate", "individual", "arena"),
      names_sep = "_",
      values_to = "val"
    ) |>
    tidyr::pivot_wider(
      id_cols = c("time", "individual", "arena"),
      names_from = "coordinate",
      values_from = "val"
    ) |>
    tidyr::unite("individual", c("individual", "arena")) |>
    dplyr::mutate(individual = factor(.data$individual))
  return(data)
}
