#' Initiate movement metadata
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' @param data movement data frame
#'
#' @return data frame with metadata
#' @export
init_metadata <- function(data){
  if (!check_metadata_exists(data)){
    attributes(data)$metadata <- list(
      uuid = generate_uuid(),
      source = NA,
      source_version = NA,
      filename = NA,
      sampling_rate = NA,
      start_datetime = NA,
      reference_frame = "allocentric",
      coordinate_system = "cartesian",
      point_of_reference = "bottom_left"
    )
  }

  return(data)
}

generate_uuid <- function(length = 20){
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
set_uuid <- function(data, length = 20){
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
set_start_datetime <- function(data, start_datetime){
  ensure_metadata_exists(data)
  if (!check_class(start_datetime, "POSIXt")){
    start_datetime <- anytime::anytime(start_datetime) |>
      suppressWarnings()
    if (is.na(start_datetime)){
      cli::cli_warn("Unable to parse the supplied datetime. Please provide datetime in POSIXt format, or see the \"anytime\" package documentation for other allowed input types.")
    }
  }
  attributes(data)$metadata$start_datetime <- start_datetime
  return(data)
}

#' @keywords internal
check_class <- function(x, class){
  class %in% class(x)
}

#' @keywords internal
ensure_class <- function(x, class){
  if (!class %in% class(x)){
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
get_metadata <- function(data){
  ensure_metadata_exists(data)
  return(attributes(data)$metadata)
}
