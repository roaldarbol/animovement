#' Read TRex Movement Tracking Data
#'
#' @description
#' Reads and formats movement tracking data exported from TRex (Walter & Couzin, 2021).
#' TRex is a software for tracking animal movement in videos, which exports
#' coordinate data in CSV format. This function processes these files into
#' a standardized movement data format.
#'
#' @param path Character string specifying the path to a TRex CSV file.
#'   The file should contain columns for:
#'   - time
#'   - x and y coordinates for tracked points (e.g., x_head, y_head)
#'   - x and y coordinates for centroid (x_number_wcentroid_cm, y_number_wcentroid_cm)
#'
#' @return A data frame containing movement data with the following columns:
#'   - `time`: Time values from the tracking
#'   - `individual`: Factor (set to NA, as TRex tracks one individual)
#'   - `keypoint`: Factor identifying tracked points (e.g., "head", "centroid")
#'   - `x`: x-coordinates in centimeters
#'   - `y`: y-coordinates in centimeters
#'   - `confidence`: Numeric confidence values (set to NA as TRex doesn't provide these)
#'
#' @details
#' The function performs several processing steps:
#' 1. Validates the input file format (must be CSV)
#' 2. Reads the data using vroom for efficient processing
#' 3. Cleans column names to a consistent format
#' 4. Restructures the data from wide to long format
#' 5. Initializes metadata fields required for movement data
#'
#' @references
#' Walter, T., & Couzin, I. D. (2021). TRex, a fast multi-animal tracking
#' system with markerless identification, and 2D estimation of posture and
#' visual fields. eLife, 10, e64000.
#'
#' @examples
#' \dontrun{
#' # Read a TRex CSV file
#' data <- read_trex("path/to/trex_export.csv")
#' }
#'
#' @seealso
#' - `init_metadata()` for details on metadata initialization
#' - TRex software: https://trex.run
#'
#' @import dplyr tidyr tidyselect
#' @importFrom janitor clean_names
#' @importFrom vroom vroom
#'
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

#' Read and Process TRex CSV File
#'
#' @description
#' Internal function that handles the actual reading and processing of
#' TRex CSV files. Called by read_trex() after file validation.
#'
#' @param path Character string specifying path to TRex CSV file
#'
#' @return A processed data frame in movement data format
#'
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
