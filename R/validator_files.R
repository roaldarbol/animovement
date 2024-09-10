#' Validate file
#'
#' @description The validator ensures that the file:
#'
#' - is not a directory,
#' - exists if it is meant to be read,
#' - does not exist if it is meant to be written,
#' - has one of the expected suffix(es).
#'
#' @param path Path(s) to the file.
#' @param expected_permission Expected access permission(s) for the file. If
#'   "r", the file is expected to be readable. If "w", the file is expected to
#'   be writable. If "rw", the file is expected to be both readable and
#'   writable. Default: "r".
#' @param expected_suffix Expected suffix(es) for the file. If NULL (default), this check is skipped.
#' @param expected_headers Expected column name(s) to be present among the header names. Default is c("x", "y", "time").
#'
#' @importFrom vroom vroom
#' @importFrom cli cli_abort
#' @export
validate_files <- function(
    path,
    expected_permission = "r",
    expected_suffix = NULL,
    expected_headers = c("x", "y", "time")
    ){
  # Perform checks on all supplied paths
  for (p in path){
    ensure_is_not_dir(p)
    ensure_file_exists_when_expected(p, expected_permission)
    ensure_file_has_access_permissions(p, expected_permission)
    if (!is.null(expected_suffix)){
      ensure_file_has_expected_suffix(p, expected_suffix)
    }
    ensure_file_has_headers(p)
    ensure_file_has_expected_headers(p, expected_headers)
  }
}

#' Ensure that the path does not point to a directory.
#' @description Ensure that the path does not point to a directory.
#' @inheritParams validate_files
ensure_is_not_dir <- function(path){
  if (dir.exists(path)){
    cli::cli_abort("Expected a file path but got a directory: {path}")
  }
}

#' Ensure that the file exists (or not) as needed.
#' @description Ensure that the file exists (or not) as needed. This depends on the expected usage (read and/or write).
#' @inheritParams validate_files
ensure_file_exists_when_expected <- function(path, expected_permission){
  if (expected_permission %in% c("r", "rw") & file.access(path, mode = 0) == -1){
      cli::cli_abort("File {path} does not exist.")
  } else if (expected_permission %in% c("w", "rw") & file.access(path, mode = 0) == 0){
      cli::cli_abort("File {path} already exists.")
  }
}

#' Ensure that the file has the expected access permission(s).
#' @description Ensure that the file has the expected access permission(s).
#' @inheritParams validate_files
ensure_file_has_access_permissions <- function(path, expected_permission){
  if (expected_permission %in% c("r", "rw") & file.access(path, mode = 4) == -1){
      cli::cli_abort("Unable to read file: {path}. Make sure that you have read permissions.")
  } else if (expected_permission %in% c("w", "rw") & file.access(path, mode = 2) == -1){
      cli::cli_abort("Unable to write to file: {path}. Make sure that you have write permissions.")
  }
}

#' Ensure that the file has one of the expected suffix(es).
#' @description Ensure that the file has one of the expected suffix(es).
#' @inheritParams validate_files
ensure_file_has_expected_suffix <- function(path, expected_suffix){
  path_suffix <- .get_file_ext(path)
  if (!path_suffix %in% expected_suffix){
    cli::cli_abort("Expected file with suffix(es) {expected_suffix}, but got suffix {path_suffix} instead.")
  }
}

#' Ensure file has headers
#' @inheritParams validate_files
#' @export
ensure_file_has_headers <- function(path){
  df <- vroom::vroom(
    path,
    n_max = 10,
    delim = ",",
    show_col_types = FALSE,
    .name_repair = "unique") |>
    suppressMessages()
  has_headers <- ncol(df) > 1
  return(has_headers)
}

#' Ensure file has expected headers
#' @inheritParams validate_files
#' @export
ensure_file_has_expected_headers <- function(path, expected_headers = c("x", "y", "time")){
  df <- vroom::vroom(
    path,
    n_max = 10,
    delim = ",",
    show_col_types = FALSE,
    .name_repair = "unique") |>
    suppressMessages()
  has_correct_headers <- all(expected_headers %in% names(df))
  return(has_correct_headers)
}
