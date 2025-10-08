#' Read idtracker.ai data
#'
#' @param path Path to an idtracker.ai data frame
#' @param path_probabilities Path to a csv file with probabilities. Only needed if you are reading csv files as they are included in h5 files.
#' @param version idtracker.ai version. Currently only v6 output is implemented
#'
#' @import dplyr
#' @import tidyr
#' @import rhdf5
#' @importFrom vroom vroom
#' @importFrom janitor clean_names
#'
#' @return a movement dataframe
#' @export
read_idtracker <- function(path, path_probabilities = NULL, version = 6) {
  # Needs to check the file extension
  # If probabilites are given, extension needs to be csv
  validate_files(path, expected_suffix = c("csv", "h5"))
  if (!is.null(path_probabilities) & .get_file_ext(path) == "h5") {
    cli::cli_warn(
      "You supplied a h5 file and probabilities in csv; the h5 data already contains the probabilities, so we only the h5 data."
    )
  }
  if (.get_file_ext(path) == "csv") {
    data <- read_idtracker_csv(path, path_probabilities, version = version)
  } else if (.get_file_ext(path) == "h5") {
    data <- read_idtracker_h5(path, version = version)
  }

  # Init metadata
  data <- data |>
    init_metadata()

  return(data)
}

#' @inheritParams read_idtracker
#' @keywords internal
read_idtracker_csv <- function(path, path_probabilities, version = 6) {
  data <- vroom::vroom(
    path,
    delim = ",",
    show_col_types = FALSE
  ) |>
    suppressMessages() |>
    janitor::clean_names()

  data <- data |>
    tidyr::pivot_longer(
      cols = 2:ncol(data),
      names_to = c("coordinate", "individual"),
      names_sep = "(?<=[A-Za-z])(?=[0-9])",
      values_to = "val"
    ) |>
    tidyr::pivot_wider(
      id_cols = c("seconds", "individual"),
      names_from = "coordinate",
      values_from = "val"
    ) |>
    dplyr::rename(time = "seconds") |>
    dplyr::mutate(individual = factor(.data$individual))

  if (!is.null(path_probabilities)) {
    probs <- read_idtracker_probabilities(path_probabilities)
    data <- dplyr::left_join(data, probs, by = c("individual", "time"))
  }

  # Convert NaN to NA
  data <- data |>
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      ~ ifelse(is.nan(.), NA, .)
    )) |>
    dplyr::mutate(
      individual = factor(.data$individual),
      keypoint = factor("centroid")
    ) |>
    dplyr::relocate("keypoint", .after = "individual")

  return(data)
}

#' @inheritParams read_idtracker
#' @keywords internal
read_idtracker_probabilities <- function(path) {
  data <- vroom::vroom(
    path,
    delim = ",",
    show_col_types = FALSE
  ) |>
    suppressMessages() |>
    janitor::clean_names()

  data <- data |>
    tidyr::pivot_longer(
      cols = 2:ncol(data),
      names_to = c("placeholder", "individual"),
      names_sep = "(?<=[A-Za-z])(?=[0-9])",
      values_to = "confidence"
    ) |>
    dplyr::select(-"placeholder") |>
    dplyr::rename(time = "seconds")
}

#' @inheritParams read_idtracker
#' @keywords internal
read_idtracker_h5 <- function(path, version = version) {
  traj_dimensions <- rhdf5::h5ls(path) |>
    dplyr::as_tibble(.name_repair = "unique") |>
    dplyr::filter(.data$name == "trajectories") |>
    dplyr::pull(dim) |>
    strsplit(" x ")

  n_individuals <- traj_dimensions[[1]][2] |> as.numeric()

  data <- data.frame()
  for (i in 1:n_individuals) {
    trajectories <- rhdf5::h5read(path, "trajectories")[, i, ] |>
      t() |>
      dplyr::as_tibble(.name_repair = "unique") |>
      suppressMessages() |>
      dplyr::rename(x = "...1", y = "...2")

    probs <- rhdf5::h5read(path, "id_probabilities")[, i, ] |>
      dplyr::as_tibble(.name_repair = "unique") |>
      dplyr::rename(confidence = "value")

    data_temp <- dplyr::bind_cols(trajectories, probs) |>
      dplyr::mutate(
        individual = factor(i),
        keypoint = factor("centroid"),
        time = row_number()
      )

    data <- dplyr::bind_rows(data, data_temp)
  }

  data <- data |>
    # Convert NaN to NA
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      ~ ifelse(is.nan(.), NA, .)
    )) |>
    dplyr::relocate("keypoint", .before = "x") |>
    dplyr::relocate("individual", .before = "keypoint") |>
    dplyr::relocate("time", .before = "individual") |>
    dplyr::mutate(
      individual = factor(.data$individual),
      keypoint = factor(.data$keypoint)
    )
  return(data)
}
