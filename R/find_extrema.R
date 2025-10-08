#' Find Peaks in Time Series Data
#'
#' @description
#' Identifies peaks (local maxima) in a numeric time series, with options to filter peaks based on
#' height and prominence. The function handles missing values (NA) appropriately and is compatible
#' with dplyr's mutate. Includes flexible handling of plateaus and adjustable window size for peak detection.
#'
#' @param x Numeric vector containing the time series data
#' @param min_height Minimum height threshold for peaks (default: -Inf)
#' @param min_prominence Minimum prominence threshold for peaks (default: 0)
#' @param plateau_handling String specifying how to handle plateaus. One of:
#'   * "strict" (default): No points in plateau are peaks
#'   * "middle": Middle point(s) of plateau are peaks
#'   * "first": First point of plateau is peak
#'   * "last": Last point of plateau is peak
#'   * "all": All points in plateau are peaks
#' @param window_size Integer specifying the size of the window to use for peak detection (default: 3).
#'   Must be odd and >= 3. Larger values detect peaks over wider ranges.
#'
#' @return A logical vector of the same length as the input where:
#' * `TRUE` indicates a confirmed peak
#' * `FALSE` indicates a non-peak
#' * `NA` indicates peak status could not be determined due to missing data
#'
#' @details
#' The function uses a sliding window algorithm for peak detection (window size specified by window_size parameter),
#' combined with a region-based prominence calculation method similar to that described in Palshikar (2009).
#'
#' # Peak Detection
#' A point is considered a peak if it is the highest point within its window (default window_size of 3
#' compares each point with its immediate neighbors). The first and last (window_size-1)/2 points in the
#' series cannot be peaks and are marked as NA. Larger window sizes will identify peaks that dominate over
#' a wider range, typically resulting in fewer peaks being detected.
#'
#' # Prominence
#' Prominence measures how much a peak stands out relative to its surrounding values.
#' It is calculated as the height of the peak minus the height of the highest minimum
#' between this peak and any higher peaks (or the end of the series if no higher peaks exist).
#'
#' # Plateau Handling
#' Plateaus (sequences of identical values) are handled according to the plateau_handling parameter:
#'
#' * strict: No points in a plateau are considered peaks (traditional behavior)
#' * middle: For plateaus of odd length, the middle point is marked as a peak.
#'          For plateaus of even length, the two middle points are marked as peaks.
#' * first: The first point of each plateau is marked as a peak
#' * last: The last point of each plateau is marked as a peak
#' * all: Every point in the plateau is marked as a peak
#'
#' Note that in all cases, the plateau must still qualify as a peak relative to its
#' surrounding window (i.e., higher than all other points in the window).
#'
#' # Missing Values (NA) Handling
#' The function uses the following rules for handling NAs:
#' * If a point is NA, it cannot be a peak (returns NA)
#' * If any point in the window is NA, peak status cannot be determined (returns NA)
#' * For prominence calculations, stretches of NAs are handled appropriately
#' * A minimum of window_size points is required; shorter series return all NAs
#'
#' @references
#' Palshikar, G. (2009). Simple Algorithms for Peak Detection in Time-Series.
#' Proc. 1st Int. Conf. Advanced Data Analysis, Business Analytics and Intelligence.
#'
#' @examples
#' # Basic usage with default window size (3)
#' x <- c(1, 3, 2, 6, 4, 5, 2)
#' find_peaks(x)
#'
#' # With larger window size
#' find_peaks(x, window_size = 5)  # More stringent peak detection
#'
#' # With minimum height
#' find_peaks(x, min_height = 4, window_size = 3)
#'
#' # With plateau handling
#' x <- c(1, 3, 3, 3, 2, 4, 4, 1)
#' find_peaks(x, plateau_handling = "middle", window_size = 3)  # Middle of plateaus
#' find_peaks(x, plateau_handling = "all", window_size = 5)     # All plateau points
#'
#' # With missing values
#' x <- c(1, 3, NA, 6, 4, NA, 2)
#' find_peaks(x)
#'
#' # Usage with dplyr
#' library(dplyr)
#' data_frame(
#'   time = 1:10,
#'   value = c(1, 3, 7, 4, 2, 6, 5, 8, 4, 2)
#' ) %>%
#'   mutate(peaks = find_peaks(value, window_size = 3))
#'
#' @seealso
#' * \code{\link{find_troughs}} for finding local minima
#' * \code{\link[pracma]{findpeaks}} in the pracma package for alternative peak detection methods
#'
#' @note
#' * The function is optimized for use with dplyr's mutate
#' * For noisy data, consider using a larger window_size or smoothing the series before peak detection
#' * Adjust min_height and min_prominence to filter out unwanted peaks
#' * Choose plateau_handling based on your specific needs
#' * Larger window_size values result in more stringent peak detection
#'
#' @export
find_peaks <- function(
  x,
  min_height = -Inf,
  min_prominence = 0,
  plateau_handling = c("strict", "middle", "first", "last", "all"),
  window_size = 3
) {
  plateau_handling <- match.arg(plateau_handling)
  find_extrema(
    x,
    min_height,
    min_prominence,
    plateau_handling,
    "peak",
    window_size
  )
}

#' Find Troughs in Time Series Data
#'
#' @description
#' Identifies troughs (local minima) in a numeric time series, with options to filter troughs based on
#' height and prominence. The function handles missing values (NA) appropriately and is compatible
#' with dplyr's mutate. Includes flexible handling of plateaus and adjustable window size for trough detection.
#'
#' @param x Numeric vector containing the time series data
#' @param max_height Maximum height threshold for troughs (default: Inf)
#' @param min_prominence Minimum prominence threshold for troughs (default: 0)
#' @param plateau_handling String specifying how to handle plateaus. One of:
#'   * "strict" (default): No points in plateau are troughs
#'   * "middle": Middle point(s) of plateau are troughs
#'   * "first": First point of plateau is trough
#'   * "last": Last point of plateau is trough
#'   * "all": All points in plateau are troughs
#' @param window_size Integer specifying the size of the window to use for trough detection (default: 3).
#'   Must be odd and >= 3. Larger values detect troughs over wider ranges.
#'
#' @return A logical vector of the same length as the input where:
#' * `TRUE` indicates a confirmed trough
#' * `FALSE` indicates a non-trough
#' * `NA` indicates trough status could not be determined due to missing data
#'
#' @details
#' The function uses a sliding window algorithm for trough detection (window size specified by window_size parameter),
#' combined with a region-based prominence calculation method similar to that described in Palshikar (2009).
#'
#' # Trough Detection
#' A point is considered a trough if it is the lowest point within its window (default window_size of 3
#' compares each point with its immediate neighbors). The first and last (window_size-1)/2 points in the
#' series cannot be troughs and are marked as NA. Larger window sizes will identify troughs that dominate over
#' a wider range, typically resulting in fewer troughs being detected.
#'
#' # Prominence
#' Prominence measures how much a trough stands out relative to its surrounding values.
#' It is calculated as the height of the lowest maximum between this trough and any lower
#' troughs (or the end of the series if no lower troughs exist) minus the height of the trough.
#'
#' # Plateau Handling
#' Plateaus (sequences of identical values) are handled according to the plateau_handling parameter:
#'
#' * strict: No points in a plateau are considered troughs (traditional behavior)
#' * middle: For plateaus of odd length, the middle point is marked as a trough.
#'          For plateaus of even length, the two middle points are marked as troughs.
#' * first: The first point of each plateau is marked as a trough
#' * last: The last point of each plateau is marked as a trough
#' * all: Every point in the plateau is marked as a trough
#'
#' Note that in all cases, the plateau must still qualify as a trough relative to its
#' surrounding window (i.e., lower than all other points in the window).
#'
#' # Missing Values (NA) Handling
#' The function uses the following rules for handling NAs:
#' * If a point is NA, it cannot be a trough (returns NA)
#' * If any point in the window is NA, trough status cannot be determined (returns NA)
#' * For prominence calculations, stretches of NAs are handled appropriately
#' * A minimum of window_size points is required; shorter series return all NAs
#'
#' @references
#' Palshikar, G. (2009). Simple Algorithms for Peak Detection in Time-Series.
#' Proc. 1st Int. Conf. Advanced Data Analysis, Business Analytics and Intelligence.
#'
#' @examples
#' # Basic usage with default window size (3)
#' x <- c(5, 3, 4, 1, 4, 2, 5)
#' find_troughs(x)
#'
#' # With larger window size
#' find_troughs(x, window_size = 5)  # More stringent trough detection
#'
#' # With maximum height
#' find_troughs(x, max_height = 3, window_size = 3)
#'
#' # With plateau handling
#' x <- c(5, 2, 2, 2, 3, 1, 1, 4)
#' find_troughs(x, plateau_handling = "middle", window_size = 3)  # Middle of plateaus
#' find_troughs(x, plateau_handling = "all", window_size = 5)     # All plateau points
#'
#' # With missing values
#' x <- c(5, 3, NA, 1, 4, NA, 5)
#' find_troughs(x)
#'
#' # Usage with dplyr
#' library(dplyr)
#' data_frame(
#'   time = 1:10,
#'   value = c(5, 3, 1, 4, 2, 1, 3, 0, 4, 5)
#' ) %>%
#'   mutate(troughs = find_troughs(value, window_size = 3))
#'
#' @seealso
#' * \code{\link{find_peaks}} for finding local maxima
#' * \code{\link[pracma]{findpeaks}} in the pracma package for alternative extrema detection methods
#'
#' @note
#' * The function is optimized for use with dplyr's mutate
#' * For noisy data, consider using a larger window_size or smoothing the series before trough detection
#' * Adjust max_height and min_prominence to filter out unwanted troughs
#' * Choose plateau_handling based on your specific needs
#' * Larger window_size values result in more stringent trough detection
#'
#' @export
find_troughs <- function(
  x,
  max_height = Inf,
  min_prominence = 0,
  plateau_handling = c("strict", "middle", "first", "last", "all"),
  window_size = 3
) {
  plateau_handling <- match.arg(plateau_handling)
  find_extrema(
    x,
    max_height,
    min_prominence,
    plateau_handling,
    "trough",
    window_size
  )
}

#' Internal function for finding extrema
#' @noRd
find_extrema <- function(
  x,
  height_threshold,
  min_prominence,
  plateau_handling,
  type,
  window_size
) {
  # Input validation
  if (!is.numeric(x) && !all(is.na(x))) {
    stop("Input must be numeric or NA")
  }
  if (!is.numeric(window_size) || window_size < 3 || window_size %% 2 == 0) {
    stop("window_size must be an odd integer >= 3")
  }

  # Initialize result vector
  n <- length(x)
  is_extremum <- rep(FALSE, n)

  # Handle special cases
  if (n < window_size) {
    return(rep(NA, n))
  }

  # Edge points can't be extrema
  half_window <- floor(window_size / 2)
  is_extremum[1:half_window] <- NA
  is_extremum[(n - half_window + 1):n] <- NA

  # Function to check if value meets height threshold
  meets_height_threshold <- if (type == "peak") {
    function(value) value > height_threshold
  } else {
    function(value) value < height_threshold
  }

  # Function to check if point is extremum within window
  is_window_extremum <- function(idx) {
    if (is.na(x[idx])) {
      return(NA)
    }

    # First check height threshold
    if (!meets_height_threshold(x[idx])) {
      return(FALSE)
    }

    window_start <- max(1, idx - half_window)
    window_end <- min(n, idx + half_window)
    window_values <- x[window_start:window_end]

    if (any(is.na(window_values))) {
      return(NA)
    }

    if (type == "peak") {
      all(window_values <= x[idx]) && any(window_values < x[idx])
    } else {
      all(window_values >= x[idx]) && any(window_values > x[idx])
    }
  }

  # Function to handle plateaus
  handle_plateau <- function(start_idx, end_idx, value) {
    # First check height threshold
    if (!meets_height_threshold(value)) {
      return(list(indices = NULL, is_na = FALSE))
    }

    # Check if we can determine if this is an extremum
    window_start <- max(1, start_idx - half_window)
    window_end <- min(n, end_idx + half_window)

    if (any(is.na(x[window_start:window_end]))) {
      return(list(indices = NULL, is_na = TRUE))
    }

    is_extremum <- if (type == "peak") {
      all(x[window_start:window_end] <= value) &&
        any(x[window_start:window_end] < value)
    } else {
      all(x[window_start:window_end] >= value) &&
        any(x[window_start:window_end] > value)
    }

    if (!is_extremum) {
      return(list(indices = NULL, is_na = FALSE))
    }

    # Return indices based on plateau handling method
    plateau_length <- end_idx - start_idx + 1
    indices <- switch(
      plateau_handling,
      "strict" = NULL,
      "middle" = if (plateau_length %% 2 == 0) {
        c(start_idx + plateau_length / 2 - 1, start_idx + plateau_length / 2)
      } else {
        start_idx + floor(plateau_length / 2)
      },
      "first" = start_idx,
      "last" = end_idx,
      "all" = start_idx:end_idx
    )

    return(list(indices = indices, is_na = FALSE))
  }

  # Process the sequence
  i <- (half_window + 1)
  while (i <= (n - half_window)) {
    # Handle NA
    if (is.na(x[i])) {
      is_extremum[i] <- NA
      i <- i + 1
      next
    }

    # Check for plateau
    plateau_end <- i
    while (
      plateau_end < n &&
        !is.na(x[plateau_end + 1]) &&
        x[plateau_end + 1] == x[i]
    ) {
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
      is_extremum[i] <- is_window_extremum(i)
      i <- i + 1
    }
  }

  # Apply prominence filter if specified
  if (min_prominence > 0) {
    for (i in which(is_extremum)) {
      if (is.na(is_extremum[i])) {
        next
      }

      # Calculate prominence using window_size for initial search
      left_idx <- max(1, i - half_window)
      right_idx <- min(n, i + half_window)

      while (
        left_idx > 1 &&
          !is.na(x[left_idx]) &&
          (type == "peak" &&
            x[left_idx] <= x[i] ||
            type == "trough" && x[left_idx] >= x[i])
      ) {
        left_idx <- left_idx - 1
      }

      while (
        right_idx < n &&
          !is.na(x[right_idx]) &&
          (type == "peak" &&
            x[right_idx] <= x[i] ||
            type == "trough" && x[right_idx] >= x[i])
      ) {
        right_idx <- right_idx + 1
      }

      if (is.na(x[left_idx]) || is.na(x[right_idx])) {
        is_extremum[i] <- NA
        next
      }

      extremum_value <- if (type == "peak") {
        min(x[c(left_idx:i, i:right_idx)], na.rm = TRUE)
      } else {
        max(x[c(left_idx:i, i:right_idx)], na.rm = TRUE)
      }

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
