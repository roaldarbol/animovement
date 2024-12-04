
ensure_output_header_names <- function(
    data,
    expected_headers = c("time", "individual", "keypoint", "x", "y", "confidence")){
  headers <- names(data)[names(data) %in% expected_headers]
  if (!all(expected_headers %in% headers)){
    cli::cli_abort("Got header names '{headers}', but expected '{expected_headers}'.")
  }
}

ensure_output_header_class <- function(
    data,
    expected_headers = c("time", "individual", "keypoint", "x", "y", "confidence"),
    expected_header_class = c("numeric", "factor", "factor", "numeric", "numeric", "numeric")){
  data <- data |>
    dplyr::select(all_of(expected_headers))
  header_classes <- sapply(data, class)
  if (!all(expected_header_class == header_classes)){
    cli::cli_abort("Expected output headers to be {expected_header_class}, got {header_classes}.")
  }
}

ensure_output_no_nan <- function(data){

}

check_metadata_exists <- function(data){
  "metadata" %in% names(attributes(data))
}

ensure_metadata_exists <- function(data){
  if(!check_metadata_exists(data)){
    cli::cli_abort("Metadata hasn't been initiated. Initate it with `init_metadata`.")
  }
}

ensure_output_correct_metadata <- function(data, mandatory_metadata_fields){
  metadata <- get_metadata(data)
  metadata_names <- names(metadata)[names(metadata) %in% mandatory_metadata_fields]
  if (!all(mandatory_metadata_fields %in% metadata_names)){
    cli::cli_abort("The dataframe does not have the mandatory 'metadata' fields")
  }
}
