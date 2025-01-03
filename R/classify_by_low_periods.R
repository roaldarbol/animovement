#' Classifies Periods of Low Activity in Time Series Using Peaks and Troughs
#'
#' @description
#' Identifies periods of low activity in a time series by analyzing peaks and troughs,
#' returning a logical vector marking these periods. Low activity periods are defined
#' as regions between consecutive troughs that contain no peaks.
#'
#' @param peaks logical vector; TRUE indicates peak positions
#' @param troughs logical vector; same length as peaks, TRUE indicates trough positions
#'
#' @return logical vector; TRUE indicates periods of low activity
#'
#' @details
#' The function performs the following steps:
#' 1. Validates input lengths
#' 2. Initializes all periods as potentially low activity (TRUE)
#' 3. For each pair of consecutive troughs:
#'    - If no peaks exist between them, maintains TRUE for that period
#'    - If any peaks exist, marks that period as FALSE (not low activity)
#'
#' @examples
#' peaks <- c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
#' troughs <- c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE)
#' classify_low_periods(peaks, troughs)
#'
#' @export
classify_low_periods <- function(peaks, troughs) {
  # Input validation
  if (length(peaks) != length(troughs)) {
    cli::cli_abort("Lengths of peaks and troughs must match")
  }

  # Initialize output vector
  result <- rep(TRUE, length(peaks))

  # Find indices of troughs
  trough_indices <- which(troughs)

  # For each consecutive pair of troughs
  for (i in seq_len(length(trough_indices) - 1)) {
    start_idx <- trough_indices[i]
    end_idx <- trough_indices[i + 1]

    # Check if there are any peaks between these troughs
    between_slice <- peaks[(start_idx + 1):(end_idx - 1)]

    if (length(between_slice) > 0 && !any(between_slice)) {
      # If no peaks between troughs, set those positions to FALSE
      result[(start_idx + 1):(end_idx - 1)] <- FALSE
    }
  }

  return(result)
}
