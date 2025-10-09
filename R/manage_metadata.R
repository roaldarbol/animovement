#' @keywords internal
default_metadata <- function() {
  list(
    uuid = as.character(generate_uuid()),
    source = as.character(NA),
    source_version = as.character(NA),
    filename = as.character(NA),
    sampling_rate = as.numeric(NA),
    start_datetime = lubridate::as_datetime(NA),
    reference_frame = factor("allocentric"),
    coordinate_system = factor("cartesian"),
    point_of_reference = factor("bottom_left")
  )
}

#' Initiate movement metadata
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' @param data movement data frame
#'
#' @return data frame with metadata
#' @export
init_metadata <- function(
    data,
    metadata = list()
) {
  # Check whether metadata is empty and whether it makes sense
  if (is.list(metadata) && !rlang::is_empty(metadata)){
    ensure_valid_metadata(metadata)
  }

  if (!check_metadata_exists(data) && !rlang::is_empty(metadata)) {
    attributes(data)$metadata <- metadata
  } else if (!check_metadata_exists(data) && rlang::is_empty(metadata)) {
    attributes(data)$metadata <- default_metadata()
  }

  return(data)
}

#' @keywords internal
ensure_valid_metadata <- function(metadata){
  if (!is.list(metadata)){
    cli::cli_abort("Metadata must be a list")
  }

  # Does the metadata field exist?
  for (i in names(metadata)){
    if (!i %in% names(default_metadata())){
      cli::cli_abort("Metadata field {i} is not valid.")
    }
  }

  # Is the metadata field in the correct class?
  for (i in 1:length(metadata)){
    if (class(metadata[[i]]) != class(names(default_metadata())[names(default_metadata()) == names(metadata[i])])){
      cli::cli_abort("Metadata field {names(metadata[i])} is of class {class(metadata[[i]])},
                     but it should be of class {class(names(default_metadata())[names(default_metadata()) == names(metadata[i])])}.")
    }
  }
}

#' @keywords internal
is_valid_metadata <- function(metadata){
  ensure_valid_metadata(metadata)
  return(TRUE)
}



generate_uuid <- function(length = 20) {
  stringi::stri_rand_strings(1, length, pattern = "[A-Z0-9]")
}

#' Set UUID
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Adds a unique identifier (UUID) to the data frames metadata
#'
#' @param data movement data frame
#' @param length length of identifier. (default: 20)
#'
#' @return data frame with the "uuid" metadata field filled out
#' @export
set_uuid <- function(data, length = 20) {
  ensure_metadata_exists(data)
  attributes(data)$metadata$uuid <- generate_uuid(length)
  return(data)
}


#' Set starting datetime
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' @param data movement data frame
#' @param start_datetime starting datetime. provided either as POSIXt, or as a string that can be parsed by the *anytime* package.
#'
#' @import anytime
#'
#' @return movement data frame with starting datetime in metadata
#' @export
set_start_datetime <- function(data, start_datetime) {
  ensure_metadata_exists(data)
  if (!check_class(start_datetime, "POSIXt")) {
    start_datetime <- anytime::anytime(start_datetime) |>
      suppressWarnings()
    if (is.na(start_datetime)) {
      cli::cli_warn(
        "Unable to parse the supplied datetime. Please provide datetime in POSIXt format, or see the \"anytime\" package documentation for other allowed input types."
      )
    }
  }
  attributes(data)$metadata$start_datetime <- start_datetime
  return(data)
}

#' Assign a new individual identifier to all rows in a dataset
#'
#' This function replaces any existing individual identifiers with a new specified
#' identifier across all rows in the dataset. The data is first ungrouped to ensure
#' consistent application of the new identifier.
#'
#' @param data A data frame or tibble containing the data to be modified
#' @param individual The new identifier value to be assigned to all rows
#'
#' @return A modified data frame with the new individual identifier applied as a factor
#'
#' @export
#'
#' @importFrom dplyr ungroup mutate
#'
#' @examples
#' data <- data.frame(time = 1:5, value = rnorm(5))
#' result <- set_individual(data, "subject_A")
set_individual <- function(data, individual) {
  new_id <- individual
  data <- data |>
    dplyr::ungroup() |>
    dplyr::mutate(individual = factor(new_id))
  return(data)
}

#' Adjust time values to reflect a new sampling rate
#'
#' This function modifies time values in a dataset to match a new sampling rate and
#' updates the corresponding metadata. It handles both integer and non-integer
#' time values, ensuring time series start from zero when appropriate.
#'
#' @param data A data frame or tibble containing the time series data
#' @param sampling_rate The new target sampling rate to convert to
#' @param old_framerate The original sampling rate of the data (defaults to 1)
#'
#' @return A modified data frame with adjusted time values and updated metadata
#'
#' @export
#'
#' @importFrom dplyr mutate
#'
#' @details The function calculates a scaling factor based on the ratio of old to
#' new sampling rates. For integer time values, it ensures they start from zero. All
#' time values are then scaled proportionally to maintain relative temporal
#' relationships.
#'
#' @examples
#' data <- data.frame(time = 0:10, value = rnorm(11))
#' result <- set_sampling_rate(data, sampling_rate = 60, old_sampling_rate = 30)
set_sampling_rate <- function(data, sampling_rate, old_sampling_rate = 1) {
  has_sampling_rate <- !is.null(attributes(data)$metadata$sampling_rate)
  if (isTRUE(has_sampling_rate)) {
    old_sampling_rate <- attributes(data)$metadata$sampling_rate
  }

  scaling_factor <- old_sampling_rate / sampling_rate

  # Ensure frame numbers start at zero
  if (is.integer(data$time)) {
    data <- data |>
      dplyr::mutate(time = .data$time - min(.data$time, na.rm = TRUE))
  }
  data <- data |>
    dplyr::mutate(time = .data$time * scaling_factor)

  attributes(data)$metadata$sampling_rate <- sampling_rate

  return(data)
}

#' @keywords internal
check_class <- function(x, class) {
  class %in% class(x)
}

#' @keywords internal
ensure_class <- function(x, class) {
  if (!class %in% class(x)) {
    cli::cli_abort("Expected an object of class {class}, but got {class(x)}.")
  }
}

#' Get/extract metadata
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' @param data movement data frame
#'
#' @return the metadata associated with the movement data frame
#' @export
get_metadata <- function(data) {
  ensure_metadata_exists(data)
  return(attributes(data)$metadata)
}
