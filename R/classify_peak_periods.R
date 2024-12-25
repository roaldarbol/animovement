#' Classifies Periods of High Activity in Time Series Using Peaks and Troughs
#'
#' @description
#' Identifies periods of high activity in a time series by analyzing peaks and troughs,
#' returning a logical vector marking these periods. The function handles special cases
#' like adjacent peaks and the initial sequence before the first trough.
#'
#' @param x numeric vector; the time series values
#' @param peaks logical vector; same length as x, TRUE indicates peak positions
#' @param troughs logical vector; same length as x, TRUE indicates trough positions
#'
#' @return logical vector; TRUE indicates periods of high activity
#'
#' @details
#' The function performs the following steps:
#' 1. Resolves adjacent peaks by keeping only the highest
#' 2. Handles the initial sequence before the first trough
#' 3. Identifies regions between troughs containing exactly one peak
#'
#' @examples
#' x <- c(1, 3, 2, 1, 4, 2, 1)
#' peaks <- c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)
#' troughs <- c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE)
#' classify_peak_periods(x, peaks, troughs)
#'
#' @export
classify_peak_periods <- function(x, peaks, troughs) {
  n <- length(x)
  result <- logical(n)

  # First handle adjacent peaks - keep only highest
  peak_indices <- which(peaks)
  for(i in 1:(length(peak_indices)-1)) {
    if(!any(troughs[peak_indices[i]:peak_indices[i+1]])) {
      if(x[peak_indices[i]] <= x[peak_indices[i+1]]) {
        peaks[peak_indices[i]] <- FALSE
      } else {
        peaks[peak_indices[i+1]] <- FALSE
      }
    }
  }

  # Handle start sequence
  trough_indices <- which(troughs)
  if(length(peak_indices) > 0 && length(trough_indices) > 0) {
    if(peak_indices[1] < trough_indices[1]) {
      result[1:trough_indices[1]] <- TRUE
    }
  }

  # Find regions between troughs that have exactly one peak
  for(i in 1:(length(trough_indices)-1)) {
    current_trough <- trough_indices[i]
    next_trough <- trough_indices[i+1]
    peaks_between <- which(peaks[current_trough:next_trough])

    if(length(peaks_between) == 1) {
      result[(current_trough+1):(next_trough-1)] <- TRUE
    }
  }

  return(result)
}
