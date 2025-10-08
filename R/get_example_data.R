#' Download example tracking data
#'
#' Downloads example data for different animal tracking software and returns the path
#' to the downloaded file. The function caches the data to avoid repeated downloads.
#'
#' @param source Character string specifying the tracking software. Currently supported:
#'   - "deeplabcut": Data from DeepLabCut tracking
#'
#' @param cache_dir Character string specifying the directory where to cache the downloaded
#'   files. Defaults to a temporary directory using `tempdir()`.
#'
#' @return Character string with the path to the downloaded file.
#'
#' @details
#' The function downloads example data from a GitHub repository and caches it locally.
#' If the file already exists in the cache directory, it will use the cached version
#' instead of downloading it again.
#'
#' The data sources are hosted at: https://github.com/roaldarbol/movement-data
#'
#' @importFrom cli cli_abort cli_warn cli_inform
#' @importFrom utils download.file
#'
#' @examples
#' \dontrun{
#' # Get path to DeepLabCut example data
#' path <- get_example_data("deeplabcut")
#'
#' # Read the data using preferred method
#' data <- read_deeplabcut(path)
#' }
#' @export
get_example_data <- function(source, cache_dir = tempdir()) {
  # Check if source is provided
  if (missing(source)) {
    cli::cli_abort(
      "Must specify a {.arg source}. Currently supported: {.val deeplabcut}"
    )
  }

  # Define available sources and their corresponding URLs
  sources <- list(
    deeplabcut = list(
      url = "https://raw.githubusercontent.com/roaldarbol/movement-data/main/data/deeplabcut/mouse_single.csv",
      filename = "dlc_mouse_single.csv"
    )
  )

  # Check if source is supported
  if (!source %in% names(sources)) {
    cli::cli_abort(c(
      "Source {.val {source}} is not supported.",
      "i" = "Currently supported sources: {.val {names(sources)}}"
    ))
  }

  # Get URL and filename for the specified source
  file_url <- sources[[source]]$url
  filename <- sources[[source]]$filename

  data_path <- file.path(cache_dir, filename)

  if (!file.exists(data_path)) {
    cli::cli_inform("Downloading example {source} data...")

    # Create cache directory if it doesn't exist
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE)
    }

    # Try to download the file
    download_success <- try(
      {
        utils::download.file(file_url, destfile = data_path, quiet = TRUE)
      },
      silent = TRUE
    )

    # Check if download failed
    if (inherits(download_success, "try-error")) {
      cli::cli_abort(c(
        "Failed to download example data.",
        "i" = "Please check your internet connection.",
        "i" = "URL attempted: {.url {file_url}}"
      ))
    }
  }

  return(data_path)
}
