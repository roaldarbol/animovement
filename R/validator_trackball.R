#' Validate trackball files
#'
#' @description
#' The validator ensures that:
#'
#'  - A valid setup is given.
#'  - A valid number of files are provided.
#'  - Identical suffixes when tewo files are given.
#'
#' @param paths Path to the file(s).
#' @param setup Experimental setup used. Expects either "of_free", "of_fixed" or "fictrac".
#' @param col_time Column which contains time
#'
#' @export
validate_trackball <- function(paths, setup, col_time){
  ensure_trackball_setup(setup)
  ensure_number_of_files(paths = paths, setup)
  ensure_identical_suffix(paths)
  for (path in paths){
    ensure_header_match(path, col_time)
  }

}

#' Ensure that a valid setup is given
#' @description Ensure that a valid setup is given
#' @inheritParams validate_trackball
ensure_trackball_setup <- function(setup){
  if (!setup %in% c("of_free", "of_fixed", "fictrac")){
    cli::cli_abort("Expected setup being either \"of_free\", \"of_fixed\" or \"fictrac\", but got {setup} instead.")
  }
}

#' @inheritParams validate_trackball
ensure_number_of_files <- function(paths, setup){
  if (setup == "of_free" & length(paths) != 2){
    cli::cli_abort("For setup {setup} expected 2 files, but got {length(paths)} files instead.")
  }
  if (setup == "of_fixed" & !length(paths) %in% c(1,2)){
    cli::cli_abort("For setup {setup} expected 1 or 2 files, but got {length(paths)} files instead.")
  }
}

#' @inheritParams validate_trackball
ensure_identical_suffix <- function(paths){
  ## Check file extension
  if (length(paths) == 2){
    file_exts <- c(.get_file_ext(paths[1]), .get_file_ext(paths[2]))
    if (file_exts[1] != file_exts[2]){
      cli::cli_abort("Files have different suffixes. Please provide 2 files of the same format.")
    }
  }
}

#' @inheritParams validate_trackball
ensure_header_match <- function(path, col_time){
  if (!.file_has_headers(path) & is.character(col_time)){
    cli::cli_abort("`col_time` is a string ({col_time}), but the file doesn't have named headers. Either use a column number or provide a file with named headers.")
  }
}
