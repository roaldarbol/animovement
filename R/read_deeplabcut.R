#' Read DeepLabCut data
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Read csv files from DeepLabCut (DLC). The function recognises whether it is a
#' single- or multi-animal dataset.
#'
#' @param path Path to a DeepLabCut data file
#'
#' @import dplyr
#' @import tidyr
#' @importFrom vroom vroom
#'
#' @return a movement dataframe
#' @export
read_deeplabcut <- function(path) {
  validate_files(path, expected_suffix = "csv")

  # Check whether it's a multi-animal daata set
  multianimal <- vroom::vroom(
      path,
      delim = ",",
      show_col_types = FALSE,
      skip = 3,
      n_max = 1,
      col_names = FALSE
    ) |>
    t() |>
    is.character()

  if (multianimal == FALSE){
    data <- read_deeplabcut_single(path)
  } else if (multianimal == TRUE){
    data <- read_deeplabcut_multi(path)
  }
  return(data)
}

#' Read single-animal DLC files
#' @keywords internal
read_deeplabcut_single <- function(path){

  # Get metadata
  header_2 <- vroom::vroom(
    path,
    delim = ",",
    show_col_types = FALSE,
    skip = 1,
    n_max = 1,
    col_names = FALSE
  )

  header_3 <- vroom::vroom(
    path,
    delim = ",",
    show_col_types = FALSE,
    skip = 2,
    n_max = 1,
    col_names = FALSE
  )

  new_headers <- rbind(header_2, header_3) |>
    t() |>
    as.data.frame() |>
    dplyr::mutate(new_names = paste(.data$V1, .data$V2, sep = "_")) |>
    dplyr::select("new_names") |>
    as.data.frame() |>
    dplyr::pull()

  data <- vroom::vroom(
    path,
    delim = ",",
    show_col_types = FALSE,
    skip = 3,
    col_names = new_headers
  )

  # Do check

  # Wrangle
  data <- data |>
    dplyr::rename(time = 1) |>
    tidyr::pivot_longer(cols = !"time",
                 names_to = c("keypoint", "pos"),
                 names_pattern = "(.*)_(\\w+)",
                 values_to = "val") |>
    tidyr::pivot_wider(id_cols = c("time", "keypoint"),
                names_from = "pos",
                values_from = "val") |>
    dplyr::rename(confidence = "likelihood")
  return(data)
}

#' Read multi-animal DLC files
#' @keywords internal
read_deeplabcut_multi <- function(path){

  # Get metadata
  header_2 <- vroom::vroom(
    path,
    delim = ",",
    show_col_types = FALSE,
    skip = 1,
    n_max = 1,
    col_names = FALSE
  )

  header_3 <- vroom::vroom(
    path,
    delim = ",",
    show_col_types = FALSE,
    skip = 2,
    n_max = 1,
    col_names = FALSE
  )

  header_4 <- vroom::vroom(
    path,
    delim = ",",
    show_col_types = FALSE,
    skip = 3,
    n_max = 1,
    col_names = FALSE
  )

  new_headers <- rbind(header_2, header_3, header_4) |>
    t() |>
    as.data.frame() |>
    dplyr::mutate(new_names = paste(.data$V1, .data$V2, .data$V3, sep = "_")) |>
    dplyr::select("new_names") |>
    as.data.frame() |>
    dplyr::pull()

  data <- vroom::vroom(
    path,
    delim = ",",
    show_col_types = FALSE,
    skip = 4,
    col_names = new_headers
  )

  # Do check

  # Wrangle
  data <- data |>
    dplyr::rename(time = 1) |>
    tidyr::pivot_longer(cols = !"time",
                        names_to = c("individual", "keypoint", "pos"),
                        names_sep = "_",
                        values_to = "val") |>
    tidyr::pivot_wider(id_cols = c("time", "individual", "keypoint"),
                       names_from = "pos",
                       values_from = "val") |>
    dplyr::rename(confidence = "likelihood")
  return(data)
}
