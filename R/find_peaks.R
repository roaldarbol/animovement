#' Find Peaks in Time Series Data
#'
#' @description
#' Identifies peaks (local maxima) in a numeric time series, with options to filter peaks based on
#' height and prominence. The function handles missing values (NA) appropriately and is compatible
#' with dplyr's mutate. Includes flexible handling of plateaus (consecutive equal values).
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
#'
#' @return A logical vector of the same length as the input where:
#' * `TRUE` indicates a confirmed peak
#' * `FALSE` indicates a non-peak
#' * `NA` indicates peak status could not be determined due to missing data
#'
#' @details
#' The function uses a three-point sliding window algorithm for basic peak detection,
#' combined with a region-based prominence calculation method similar to that described
#' in Palshikar (2009).
#'
#' # Peak Detection
#' A point is considered a peak if it is strictly higher than both its immediate neighbors.
#' The first and last points in the series cannot be peaks and are always marked as NA.
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
#' surrounding points (i.e., higher than neighbors).
#'
#' # Missing Values (NA) Handling
#' The function uses the following rules for handling NAs:
#' * If a point is NA, it cannot be a peak (returns NA)
#' * If either neighbor is NA, peak status cannot be determined (returns NA)
#' * For prominence calculations, stretches of NAs are handled appropriately
#' * A minimum of 3 points is required; shorter series return all NAs
#'
#' @references
#' Palshikar, G. (2009). Simple Algorithms for Peak Detection in Time-Series.
#' Proc. 1st Int. Conf. Advanced Data Analysis, Business Analytics and Intelligence.
#'
#' @examples
#' # Basic usage
#' x <- c(1, 3, 2, 6, 4, 5, 2)
#' find_peaks(x)
#'
#' # With minimum height
#' find_peaks(x, min_height = 4)
#'
#' # With plateau handling
#' x <- c(1, 3, 3, 3, 2, 4, 4, 1)
#' find_peaks(x, plateau_handling = "middle")  # Middle of plateaus
#' find_peaks(x, plateau_handling = "all")     # All plateau points
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
#'   mutate(peaks = find_peaks(value))
#'
#' @seealso
#' * \code{\link{find_troughs}} for finding local minima
#' * \code{\link[pracma]{findpeaks}} in the pracma package for alternative peak detection methods
#'
#' @note
#' * The function is optimized for use with dplyr's mutate
#' * For noisy data, consider smoothing the series before peak detection
#' * Adjust min_height and min_prominence to filter out unwanted peaks
#' * Choose plateau_handling based on your specific needs
#'
#' @export
find_peaks <- function(x, min_height = -Inf, min_prominence = 0,
                       plateau_handling = c("strict", "middle", "first", "last", "all")) {
  plateau_handling <- match.arg(plateau_handling)
  find_extrema(x, min_height, min_prominence, plateau_handling, type = "peak")
}
