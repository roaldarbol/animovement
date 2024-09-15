.find_nearest <- function(x, v) {
  which.min(abs(v - x))
}

.get_file_ext <- function(filename) {
  nameSplit <- strsplit(x = filename, split = "\\.")[[1]]
  return(nameSplit[length(nameSplit)])
}

.is.POSIXt <- function(x) inherits(x, "POSIXt")

.scale_values <- function(data, variables, scaling_factor) {
  # Adjust distances for mouse sensor "dots-per-cm"
  if (!is.null(scaling_factor)) {
    data <- data |>
      dplyr::mutate(across(all_of(variables), ~ .x / scaling_factor))
  }
}
