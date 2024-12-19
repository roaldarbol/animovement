#' Classify Values Into Sequences with Minimum Run Length Constraints
#'
#' @description
#' Classifies numeric values into "high" and "low" categories based on a threshold,
#' while enforcing minimum run lengths for both categories. Values exceeding the
#' threshold are classified as "high", others as "low". Short runs that don't meet
#' the minimum length requirement are reclassified into the opposite category.
#'
#' @param values Numeric vector to be classified
#' @param threshold Numeric value used as classification boundary between "high" and "low"
#' @param min_low_frames Minimum number of consecutive frames required for a "low" sequence
#' @param min_high_frames Minimum number of consecutive frames required for a "high" sequence
#' @param return_type Should the function return "factor" ("high"/"low") or "numeric" (1/0) (default: "numeric")
#'
#' @return Character vector of same length as input, with values classified as either
#'         "high" or "low". NA values in input remain NA in output.
#'
#' @details
#' The classification process occurs in two steps:
#' 1. Initial classification based on threshold
#' 2. Reclassification of sequences that don't meet minimum length requirements
#'
#' The function first processes "low" sequences, then "high" sequences. This order
#' can affect the final classification when there are competing minimum length
#' requirements.
#'
#' @examples
#' # Basic usage
#' values <- c(1, 1.5, 2.8, 3.2, 3.0, 2.9, 1.2, 1.1)
#' result <- classify_by_threshold_with_min_runs(values,
#'                                              threshold = 2.5,
#'                                              min_low_frames = 2,
#'                                              min_high_frames = 3)
#'
#' # Handling NAs
#' values_with_na <- c(1, NA, 3, 3.2, NA, 1.2)
#' result <- classify_by_threshold_with_min_runs(values_with_na,
#'                                              threshold = 2.5,
#'                                              min_low_frames = 2,
#'                                              min_high_frames = 2)
#'
#' @export
classify_by_threshold <- function(values,
                                  threshold,
                                  min_low_frames,
                                  min_high_frames,
                                  return_type = c("numeric", "factor")) {

  return_type <- match.arg(return_type)

  # Initial binary sequence with NA handling
  binary_seq <- ifelse(is.na(values), NA_character_,
                       ifelse(values > threshold, "high", "low"))
  n <- length(binary_seq)

  # Helper function to process runs
  process_runs <- function(seq, target_value, min_frames) {
    result <- seq
    run_start <- 1
    current_run <- 1

    for (i in 2:length(seq)) {
      # Skip NA values in comparison
      if (is.na(seq[i]) || is.na(seq[i-1])) {
        # Start a new run after NA
        run_start <- i
        current_run <- 1
        next
      }

      if (seq[i] == seq[i-1]) {
        current_run <- current_run + 1
      } else {
        # Check if we need to flip the previous run
        if (!is.na(seq[i-1]) && seq[i-1] == target_value && current_run < min_frames) {
          result[run_start:(i-1)] <- ifelse(target_value == "low", "high", "low")
        }
        run_start <- i
        current_run <- 1
      }
    }

    # Check the last run
    if (!is.na(seq[length(seq)]) && seq[length(seq)] == target_value && current_run < min_frames) {
      result[run_start:length(seq)] <- ifelse(target_value == "low", "high", "low")
    }

    return(result)
  }

  # Process low sequences first
  result <- process_runs(binary_seq, "low", min_low_frames)

  # Then process high sequences
  result <- process_runs(result, "high", min_high_frames)

  # Convert character states to numeric
  if (return_type == "numeric"){
    result <- numeric(length(result))
    result[is.na(result)] <- NA_real_
    result[result == "high"] <- 1
    result[result == "low"] <- 0
  }

  return(result)
}
