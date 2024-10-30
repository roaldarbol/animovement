#' Read SLEAP data
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' @param path A SLEAP analysis data frame in HDF5 (.h5) format
#'
#' @import dplyr
#' @import tidyr
#' @import rhdf5
#' @importFrom cli cli_abort
#'
#' @return a movement dataframe
#' @export
read_sleap <- function(path) {
  validate_files(path, expected_suffix = c("h5", "csv"))

  file_ext <- .get_file_ext(path)
  if (file_ext == "h5"){
    data <- read_sleap_h5(path)
  } else if (file_ext == "csv"){
    cli::cli_abort("We hope to support SLEAP CSV import soon!")
  }

  return(data)
}

#' SLEAP HDF5 Reader
#' @keywords internal
read_sleap_h5 <- function(path){
  n_individuals <- rhdf5::h5ls(path) |>
    dplyr::as_tibble() |>
    dplyr::filter(.data$name == "track_names") |>
    dplyr::pull(dim) |>
    as.numeric()

  if (n_individuals == 0){
    n_individuals <- 1
  }

  node_names <- rhdf5::h5read(path, "node_names") |>
    as.vector()

  data <- data.frame()
  for (i in 1:n_individuals){
    point_scores = rhdf5::h5read(path, "point_scores")[,,i] |>
      dplyr::as_tibble(.name_repair = "unique") |>
      suppressMessages() |>
      dplyr::rename_with(~ node_names) |>
      dplyr::mutate(time = dplyr::row_number()) |>
      tidyr::pivot_longer(cols = !"time",
                          names_to = "keypoint",
                          values_to = "confidence")

    x_coords <- rhdf5::h5read(path, "tracks")[,,1,i] |>
      dplyr::as_tibble(.name_repair = "unique") |>
      suppressMessages() |>
      dplyr::rename_with(~ node_names) |>
      dplyr::mutate(time = dplyr::row_number()) |>
      tidyr::pivot_longer(cols = !"time",
                          names_to = "keypoint",
                          values_to = "x")

    y_coords = rhdf5::h5read(path, "tracks")[,,2,i] |>
      dplyr::as_tibble(.name_repair = "unique") |>
      suppressMessages() |>
      dplyr::rename_with(~ node_names) |>
      dplyr::mutate(time = dplyr::row_number()) |>
      tidyr::pivot_longer(cols = !"time",
                          names_to = "keypoint",
                          values_to = "y")

    df_temp <- dplyr::left_join(x_coords, y_coords, by = c("time", "keypoint")) |>
      dplyr::left_join(point_scores, by = c("time", "keypoint")) |>
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~dplyr::na_if(., NaN))) |>
      dplyr::mutate(individual = paste0("individual", i))

    data <- dplyr::bind_rows(data, df_temp)
  }

  data <- data |>
    dplyr::relocate("individual", .after = "time") |>
    dplyr::arrange(.data$time, .data$individual)

  if (length(unique(data$individual)) < 2){
    data <- data |>
      dplyr::select(-"individual")
  }
  return(data)
}
