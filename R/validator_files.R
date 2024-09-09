#' Validate file
#'
#' @description The validator ensures that the file:
#'
#' - is not a directory,
#' - exists if it is meant to be read,
#' - does not exist if it is meant to be written,
#' - has one of the expected suffix(es).
#'
#' @param path Path to the file.
#' @param expected_permission Expected access permission(s) for the file. If
#'   "r", the file is expected to be readable. If "w", the file is expected to
#'   be writable. If "rw", the file is expected to be both readable and
#'   writable. Default: "r".
#' @param expected_suffix Expected suffix(es) for the file. If NULL (default), this check is skipped.
#'
#' @export
validate_files <- function(paths, expected_permission = "r", expected_suffix = NULL){
  for (path in paths){
    ensure_is_not_dir(path)
    ensure_file_exists_when_expected(path, expected_permission)
    ensure_file_has_access_permissions(path, expected_permission)
    if (!is.null(expected_suffix)){
      ensure_file_has_expected_suffix(path, expected_suffix)
    }
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
