#' Validate DeepLabCut-style .csv files.
#'
#' @description
#' The validator ensures that the file contains the expected index column levels.
#'
#' @param path Path to the .csv file.
#'
#' @importFrom vroom vroom
#'
#' @keywords internal
#' @export
validate_deeplabcut_csv <- function(path){
  ensure_dlc_expected_header_levels(path)
}

#' Ensure that the .csv file contains the expected index column levels.
#' @description Ensure that the .csv file contains the expected index column levels.
#' @inheritParams validate_deeplabcut_csv
#' @keywords internal
ensure_dlc_expected_header_levels <- function(path){
  # These are to be found among the top 4 rows of the file.
  expected_levels = c("scorer", "bodyparts", "coords")

  # Read the first 4 lines of the file
  f <- vroom::vroom(path, nrows = 4)
  if (!expected_levels %in% names(f)){
      cli::cli_abort(".csv header rows do not match the known format for DeepLabCut pose estimation output files.")
  }
}
