#' Classifies Periods of High Activity in Time Series Using Peaks and Troughs
#'
#' @description
#' Identifies periods of high activity in a time series by analyzing peaks and troughs,
#' returning a logical vector marking these periods. The function handles special cases
#' like adjacent peaks and the initial/final sequences.
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
#' 3. Handles the final sequence after the last event
#' 4. Identifies regions between troughs containing exactly one peak
#'
#' @examples
#' x <- c(1, 3, 2, 1, 4, 2, 1)
#' peaks <- c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)
#' troughs <- c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE)
#' classify_high_periods(x, peaks, troughs)
#'
#' @export
classify_high_periods <- function(x, peaks, troughs) {
  # Input validation
  if (length(peaks) != length(troughs) || length(x) != length(peaks)) {
    cli::cli_abort("Lengths of x, peaks, and troughs must match")
  }

  n <- length(x)
  result <- logical(n)

  # First handle adjacent peaks - keep only highest
  peak_indices <- which(peaks)
  for(i in 1:(length(peak_indices)-1)) {
    # Look at all peaks until we find a trough
    for(j in (i+1):length(peak_indices)) {
      if(any(troughs[peak_indices[i]:peak_indices[j]])) break
      # Keep highest peak, remove others
      if(x[peak_indices[i]] <= x[peak_indices[j]]) {
        peaks[peak_indices[i]] <- FALSE
        break
      } else {
        peaks[peak_indices[j]] <- FALSE
      }
    }
  }

  # Handle start sequence
  first_event <- min(c(peak_indices[1], trough_indices[1]))
  result[1:first_event] <- ifelse(first_event == peak_indices[1], TRUE, FALSE)

  # End sequence
  last_event <- max(c(peak_indices[length(peak_indices)],
                      trough_indices[length(trough_indices)]))
  result[last_event:n] <- ifelse(last_event == peak_indices[length(peak_indices)],
                                 TRUE, FALSE)

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
