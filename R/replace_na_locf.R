#' Replace Missing Values Using Last Observation Carried Forward
#'
#' @description
#' Replaces missing values by carrying forward the last observed value, with control
#' over both minimum and maximum gap sizes to fill.
#'
#' @param x A vector containing numeric data with missing values (NAs)
#' @param min_gap Integer specifying minimum gap size to fill. Gaps shorter
#'   than this will be left as NA. Default is 1 (fill all gaps).
#' @param max_gap Integer or Inf specifying maximum gap size to fill. Gaps longer
#'   than this will be left as NA. Default is Inf (no upper limit).
#'
#' @return A numeric vector with NA values replaced by the last observed value where
#' gap length criteria are met.
#'
#' @details
#' The function applies both minimum and maximum gap criteria:
#' - Gaps shorter than min_gap are left as NA
#' - Gaps longer than max_gap are left as NA
#' - Only gaps that meet both criteria are filled
#' If both parameters are specified, min_gap must be less than or equal to max_gap.
#'
#' @examples
#' \dontrun{
#' x <- c(1, NA, NA, 4, 5, NA, NA, NA, 9)
#' replace_na_locf(x)  # fills all gaps
#' replace_na_locf(x, min_gap = 2)  # only gaps >= 2
#' replace_na_locf(x, max_gap = 2)  # only gaps <= 2
#' replace_na_locf(x, min_gap = 2, max_gap = 3)  # gaps between 2 and 3
#' }
#'
#' @export
replace_na_locf <- function(x, min_gap = 1, max_gap = Inf) {
  # Input validation
  if (!is.numeric(x)) {
    cli::cli_abort("Input must be numeric")
  }

  if (min_gap < 1) {
    cli::cli_abort("min_gap must be >= 1")
  }

  if (max_gap < min_gap) {
    cli::cli_abort("max_gap must be >= min_gap")
  }

  if (!anyNA(x)) {
    return(x)
  }

  # Get run lengths of NA sequences
  runs <- rle(is.na(x))

  # If min_gap > 1 or max_gap is finite, filter gaps
  if (min_gap > 1 || is.finite(max_gap)) {
    # Create logical vector for invalid gap sizes
    invalid_gaps <- runs$values &
      runs$lengths < min_gap |
      runs$lengths > max_gap

    # Update runs to only fill valid gaps
    runs$values[runs$values] <- invalid_gaps[runs$values]
    gaps <- inverse.rle(runs)

    # Create temporary vector for LOCF
    temp <- x

    # Perform LOCF
    filled <- collapse::na_locf(temp)

    # Re-fill invalid gaps with NA
    filled[gaps] <- NA  # Only fill valid gaps
  } else {
    # If no gap filtering, perform LOCF on all NAs
    filled <- collapse::na_locf(x)
  }


  return(filled)
}
