#' Find Troughs in Time Series Data
#'
#' @description
#' Identifies troughs (local minima) in a numeric time series, with options to filter troughs based on
#' height and prominence. The function handles missing values (NA) appropriately and is compatible
#' with dplyr's mutate. Includes flexible handling of plateaus (consecutive equal values).
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
#'
#' @return A logical vector of the same length as the input where:
#' * `TRUE` indicates a confirmed trough
#' * `FALSE` indicates a non-trough
#' * `NA` indicates trough status could not be determined due to missing data
#'
#' @details
#' The function uses a three-point sliding window algorithm for basic trough detection,
#' combined with a region-based prominence calculation method similar to that described
#' in Palshikar (2009).
#'
#' # Trough Detection
#' A point is considered a trough if it is strictly lower than both its immediate neighbors.
#' The first and last points in the series cannot be troughs and are always marked as NA.
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
#' surrounding points (i.e., lower than neighbors).
#'
#' # Missing Values (NA) Handling
#' The function uses the following rules for handling NAs:
#' * If a point is NA, it cannot be a trough (returns NA)
#' * If either neighbor is NA, trough status cannot be determined (returns NA)
#' * For prominence calculations, stretches of NAs are handled appropriately
#' * A minimum of 3 points is required; shorter series return all NAs
#'
#' @references
#' Palshikar, G. (2009). Simple Algorithms for Peak Detection in Time-Series.
#' Proc. 1st Int. Conf. Advanced Data Analysis, Business Analytics and Intelligence.
#'
#' @examples
#' # Basic usage
#' x <- c(5, 3, 4, 1, 4, 2, 5)
#' find_troughs(x)
#'
#' # With maximum height
#' find_troughs(x, max_height = 3)
#'
#' # With plateau handling
#' x <- c(5, 2, 2, 2, 3, 1, 1, 4)
#' find_troughs(x, plateau_handling = "middle")  # Middle of plateaus
#' find_troughs(x, plateau_handling = "all")     # All plateau points
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
#'   mutate(troughs = find_troughs(value))
#'
#' @seealso
#' * \code{\link{find_peaks}} for finding local maxima
#' * \code{\link[pracma]{findpeaks}} in the pracma package for alternative extrema detection methods
#'
#' @note
#' * The function is optimized for use with dplyr's mutate
#' * For noisy data, consider smoothing the series before trough detection
#' * Adjust max_height and min_prominence to filter out unwanted troughs
#' * Choose plateau_handling based on your specific needs
#'
#' @export
find_troughs <- function(x, max_height = Inf, min_prominence = 0,
                         plateau_handling = c("strict", "middle", "first", "last", "all")) {
  plateau_handling <- match.arg(plateau_handling)
  find_extrema(x, max_height, min_prominence, plateau_handling, type = "trough")
}
