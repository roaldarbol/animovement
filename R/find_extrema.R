#' Internal function for finding extrema
#' @noRd
find_extrema <- function(x, height_threshold, min_prominence, plateau_handling, type) {
  # Input validation
  if (!is.numeric(x) && !all(is.na(x))) stop("Input must be numeric or NA")

  # Initialize result vector
  n <- length(x)
  is_extremum <- rep(FALSE, n)

  # Handle special cases
  if (n < 3) return(rep(NA, n))

  # First and last points can't be extrema
  is_extremum[1] <- NA
  is_extremum[n] <- NA

  # Set comparison function based on type
  compare <- if (type == "peak") {
    function(a, b) if (is.na(a) || is.na(b)) NA else a > b
  } else {
    function(a, b) if (is.na(a) || is.na(b)) NA else a < b
  }

  # Function to safely find extremum value
  safe_extremum <- if (type == "peak") {
    function(x) if (all(is.na(x))) NA else min(x, na.rm = TRUE)
  } else {
    function(x) if (all(is.na(x))) NA else max(x, na.rm = TRUE)
  }

  # Function to handle plateaus
  handle_plateau <- function(start_idx, end_idx, value) {
    # Check if we can determine if this is an extremum
    if (start_idx == 1 || end_idx == n ||
        is.na(x[start_idx - 1]) || is.na(x[end_idx + 1])) {
      return(list(indices = NULL, is_na = TRUE))
    }

    if (!compare(value, x[start_idx - 1]) || !compare(value, x[end_idx + 1])) {
      return(list(indices = NULL, is_na = FALSE))  # Not an extremum
    }

    # Check height threshold
    if (type == "peak") {
      if (value <= height_threshold) return(list(indices = NULL, is_na = FALSE))
    } else {
      if (value >= height_threshold) return(list(indices = NULL, is_na = FALSE))
    }

    # Return indices based on plateau handling method
    plateau_length <- end_idx - start_idx + 1
    indices <- switch(plateau_handling,
                      "strict" = NULL,
                      "middle" = if (plateau_length %% 2 == 0) {
                        c(start_idx + plateau_length/2 - 1, start_idx + plateau_length/2)
                      } else {
                        start_idx + floor(plateau_length/2)
                      },
                      "first" = start_idx,
                      "last" = end_idx,
                      "all" = start_idx:end_idx
    )

    return(list(indices = indices, is_na = FALSE))
  }

  # Process the sequence
  i <- 2
  while (i < n) {
    # Handle NA
    if (is.na(x[i])) {
      is_extremum[i] <- NA
      i <- i + 1
      next
    }

    # Check for plateau
    plateau_end <- i
    while (plateau_end < n && !is.na(x[plateau_end + 1]) && x[plateau_end + 1] == x[i]) {
      plateau_end <- plateau_end + 1
    }

    if (plateau_end > i) {
      # Handle plateau
      result <- handle_plateau(i, plateau_end, x[i])
      if (result$is_na) {
        is_extremum[i:plateau_end] <- NA
      } else if (!is.null(result$indices)) {
        is_extremum[result$indices] <- TRUE
      }
      i <- plateau_end + 1
    } else {
      # Handle single point
      if (i > 1 && i < n) {
        if (is.na(x[i-1]) || is.na(x[i+1])) {
          is_extremum[i] <- NA
        } else {
          left_compare <- compare(x[i], x[i-1])
          right_compare <- compare(x[i], x[i+1])

          if (is.na(left_compare) || is.na(right_compare)) {
            is_extremum[i] <- NA
          } else if (left_compare && right_compare) {
            if ((type == "peak" && x[i] > height_threshold) ||
                (type == "trough" && x[i] < height_threshold)) {
              is_extremum[i] <- TRUE
            }
          }
        }
      }
      i <- i + 1
    }
  }

  # Apply prominence filter if specified
  if (min_prominence > 0) {
    for (i in which(is_extremum)) {
      if (is.na(is_extremum[i])) next

      # Calculate prominence
      left_idx <- i - 1
      right_idx <- i + 1

      # Look left
      while(left_idx > 1 && !is.na(x[left_idx]) &&
            (type == "peak" && x[left_idx] <= x[i] ||
             type == "trough" && x[left_idx] >= x[i])) {
        left_idx <- left_idx - 1
      }

      # Look right
      while(right_idx < n && !is.na(x[right_idx]) &&
            (type == "peak" && x[right_idx] <= x[i] ||
             type == "trough" && x[right_idx] >= x[i])) {
        right_idx <- right_idx + 1
      }

      # If we hit NA on either side before finding a higher/lower point,
      # we can't determine prominence
      if (is.na(x[left_idx]) || is.na(x[right_idx])) {
        is_extremum[i] <- NA
        next
      }

      extremum_value <- safe_extremum(c(x[left_idx:i], x[i:right_idx]))
      if (is.na(extremum_value)) {
        is_extremum[i] <- NA
        next
      }

      prominence <- if (type == "peak") {
        x[i] - extremum_value
      } else {
        extremum_value - x[i]
      }

      if (prominence < min_prominence) {
        is_extremum[i] <- FALSE
      }
    }
  }

  return(is_extremum)
}
