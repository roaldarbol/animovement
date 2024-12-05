#' Read TRex data
#'
#' @param path Path to a TRex data frame in CSV format.
#'
#' @import dplyr
#' @import tidyr
#' @importFrom janitor clean_names
#' @importFrom vroom vroom
#' @import tidyselect
#'
#' @return a movement dataframe
#' @export
read_trex <- function(path) {
  # Validators
  validate_files(path, expected_suffix = "csv")
  file_ext <- .get_file_ext(path)
  if (file_ext == "csv"){
    data <- read_trex_csv(path)
  }

  # Init metadata
  data <- data |>
    init_metadata()

  return(data)
}

#' Read TRex CSV file
#' @keywords internal
read_trex_csv <- function(path){

  # Read function
  data <- vroom::vroom(
    path,
    delim = ",",
    show_col_types = FALSE
  ) |>
    suppressMessages() |>
    janitor::clean_names() |>
    dplyr::select(tidyselect::contains(c("x_", "y_", "time"))) |>
    dplyr::select(!c("vx_cm_s", "vy_cm_s", "timestamp")) |>
    dplyr::rename(x_centroid = "x_number_wcentroid_cm",
                  x_head = "x_cm",
                  y_centroid = "y_number_wcentroid_cm",
                  y_head = "y_cm") |>
    tidyr::pivot_longer(cols = !"time",
                        names_sep = "_",
                        names_to = c("pos", "keypoint"),
                        values_to = "val") |>
    tidyr::pivot_wider(id_cols = c("time", "keypoint"),
                       names_from = "pos",
                       values_from = "val") |>
    dplyr::mutate(individual = factor(NA),
                  confidence = as.numeric(NA),
                  keypoint = factor(.data$keypoint)) |>
    dplyr::relocate("individual", .after = "time")

  return(data)
}
