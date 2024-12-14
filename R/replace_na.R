#' Replace Missing Values in Movement Data
#'
#' @description
#' Replaces missing values in x and y coordinates using interpolation,
#' handling each individual and keypoint trajectory independently.
#' Uses interpolation methods adapted from the imputeTS package
#' (Moritz & Hause, GPL-3).
#'
#' @param data A data frame containing movement tracking data with the following
#'   required columns:
#'   - `individual`: Identifier for each tracked subject
#'   - `keypoint`: Identifier for each tracked point
#'   - `x`: x-coordinates with potential NAs
#'   - `y`: y-coordinates with potential NAs
#' @param method Character string specifying interpolation method:
#'   - `"linear"`: Linear interpolation (default)
#'   - `"spline"`: Spline interpolation for smoother curves
#'   - `"stine"`: Stineman interpolation preserving data shape
#' @param maxgap Integer specifying maximum gap size to interpolate.
#'   Gaps longer than this will remain NA (default: 10)
#'
#' @return A data frame with the same structure as the input, but with
#'   missing values replaced by interpolated values where possible.
#'
#' @details
#' The function applies interpolation separately to each keypoint's x and y
#' coordinates for each individual. This ensures that interpolation only
#' uses relevant trajectory data and doesn't cross between different
#' keypoints or individuals.
#'
#' @examples
#' \dontrun{
#' # Create sample data with missing values
#' data <- data.frame(
#'   individual = rep(1:2, each = 4),
#'   keypoint = c(rep("head", 4), rep("tail", 4)),
#'   x = c(1, NA, 3, 4, 2, NA, NA, 5),
#'   y = c(5, 6, NA, 8, NA, 2, 3, 4)
#' )
#'
#' # Basic linear interpolation
#' replace_missing(data)
#'
#' # Spline interpolation with smaller max gap
#' replace_missing(data, method = "spline", maxgap = 2)
#' }
#'
#' @seealso
#' - `na_interpolation()` for details on the interpolation methods
#'
#' @importFrom dplyr group_by mutate
#'
#' @export
replace_missing <- function(data, method = "linear", maxgap = 10){
  d <- data |>
    dplyr::group_by(.data$individual, .data$keypoint) |>
    dplyr::mutate(x = na_interpolation(.data$x, option = method, maxgap = maxgap),
                  y = na_interpolation(.data$y, option = method, maxgap = maxgap))

  return(d)
}



#' Interpolate Missing Values in Movement Data
#'
#' @description
#' Replaces missing values using linear, spline, or stineman interpolation.
#' Adapted from the `na_interpolation` function in the imputeTS package
#' (Moritz & Hause, GPL-3).
#'
#' @param x A vector or data frame containing numeric data with missing values (NAs)
#' @param option Character string specifying interpolation method:
#'   - `"linear"`: Linear interpolation using stats::approx (default)
#'   - `"spline"`: Spline interpolation using stats::spline
#'   - `"stine"`: Stineman interpolation using stinepack::stinterp
#' @param maxgap Integer specifying maximum gap size to interpolate. Gaps longer
#'   than this will be left as NA. Default is Inf (no limit).
#' @param ... Additional parameters passed to the underlying interpolation functions
#'
#' @return An object of the same type as the input, with NA values replaced by
#' interpolated values where possible.
#'
#' @details
#' The function requires at least 2 non-NA values to perform interpolation. For
#' each interpolation method:
#' - Linear: Connects points with straight lines
#' - Spline: Uses cubic splines for smoother curves
#' - Stineman: Preserves monotonicity and shape of the data
#'
#' Additional parameters can be passed to the underlying functions:
#' - For linear: see ?stats::approx
#' - For spline: see ?stats::spline
#' - For stineman: see ?stinepack::stinterp
#'
#' @examples
#' \dontrun{
#' # Basic linear interpolation
#' data <- c(1, 2, NA, 4, 5)
#' na_interpolation(data)
#'
#' # Spline interpolation
#' na_interpolation(data, option = "spline")
#'
#' # Only interpolate gaps of size 2 or less
#' na_interpolation(data, maxgap = 2)
#'
#' # Using additional parameters
#' na_interpolation(data, option = "spline", method = "natural")
#' }
#'
#' @seealso
#' - stats::approx for linear interpolation details
#' - stats::spline for spline interpolation details
#' - stinepack::stinterp for Stineman interpolation details
#'
#' @references
#' Johannesson et al. (2015). "Package stinepack"
#'
#' @note
#' This function is adapted from the imputeTS package (version 3.3) by
#' Steffen Moritz and Ron Hause, available under GPL-3 license.
#'
#' @importFrom stats approx spline
#' @importFrom methods hasArg
#' @importFrom stinepack stinterp
#'
#' @export
na_interpolation <- function(x, option = "linear", maxgap = Inf, ...) {

  # Variable 'data' is used for all transformations to the time series
  # 'x' needs to stay unchanged to be able to return the same ts class in the end
  data <- x

  ## Set 'skip' variable, which let's the functions know whether to do the interpolation or simply return the un-interpolated data
  skip <- FALSE

  #----------------------------------------------------------
  # Mulivariate Input
  # The next 20 lines are just for checking and handling multivariate input.
  #----------------------------------------------------------

  # Check if the input is multivariate
  if (!is.null(dim(data)[2]) && dim(data)[2] > 1) {
    # Go through columns and impute them by calling this function with univariate input
    for (i in 1:dim(data)[2]) {
      if (!anyNA(data[, i])) {
        next
      }
      # if imputing a column does not work - mostly because it is not numeric - the column is left unchanged
      tryCatch(
        data[, i] <- na_interpolation(data[, i], option, maxgap),
        error = function(cond) {
          warning(paste(
            "na_interpolation: No imputation performed for column", i, "of the input dataset.
                Reason:", cond[1]
          ), call. = FALSE)
        }
      )
    }
    return(data)
  }

  #----------------------------------------------------------
  # Univariate Input
  # All relveant imputation / pre- postprocessing  code is within this part
  #----------------------------------------------------------

  else {
    missindx <- is.na(data)

    ##
    ## 1. Input Check and Transformation
    ##

    # 1.1 Check if NAs are present
    if (!anyNA(data)) {
      return(x)
    }

    # 1.2 special handling data types
    if (any(class(data) == "tbl")) {
      data <- as.vector(as.data.frame(data)[, 1])
    }

    # 1.3 Check for algorithm specific minimum amount of non-NA values
    if (sum(!missindx) < 2) {
      skip <- TRUE
      warning("At least 2 non-NA data points required in the time series to apply na_interpolation.")
    }

    # 1.4 Checks and corrections for wrong data dimension

    # Check if input dimensionality is not as expected
    if (!is.null(dim(data)[2]) && !dim(data)[2] == 1) {
      stop("Wrong input type for parameter x.")
    }

    # Altering multivariate objects with 1 column (which are essentially
    # univariate) to be dim = NULL
    if (!is.null(dim(data)[2])) {
      data <- data[, 1]
    }

    # 1.5 Check if input is numeric
    if (!is.numeric(data)) {
      stop("Input x is not numeric.")
    }

    ##
    ## End Input Check
    ##


    ##
    ## 2. Imputation Code
    ##
    if (isFALSE(skip)){
      n <- length(data)

      allindx <- 1:n
      indx <- allindx[!missindx]

      data_vec <- as.vector(data)

      # Linear Interpolation
      if (option == "linear") {
        # Check if 'rule' is used in function call, to allow parameter pass through for rule
        # Needed since parameter pass through via (...) to approx does not work, when value for 'rule' is also set in the code.
        if (methods::hasArg(rule)) {
          interp <- stats::approx(indx, data_vec[indx], 1:n, ...)$y
        }
        else {
          interp <- stats::approx(indx, data_vec[indx], 1:n, rule = 2, ...)$y
        }
      }
      # Spline Interpolation
      else if (option == "spline") {
        interp <- stats::spline(indx, data_vec[indx], n = n, ...)$y
      }
      # Stineman Interpolation
      else if (option == "stine") {
        interp <- stinepack::stinterp(indx, data_vec[indx], 1:n, ...)$y
        # avoid NAs at the beginning and end of series // same behavior like
        # for approx with rule = 2.
        if (any(is.na(interp))) {
          interp <- collapse::na_locf(interp) #na_remaining = "rev")
        }
      }
      # Wrong parameter option
      else {
        stop("Wrong parameter 'option' given. Value must be either 'linear', 'spline' or 'stine'.")
      }

      # Merge interpolated values back into original time series
      data[missindx] <- interp[missindx]

      ##
      ## End Imputation Code
      ##


      ##
      ## 3. Post Processing
      ##

      # 3.1 Check for Maxgap option

      # If maxgap = Inf then do nothing and when maxgap is lower than 0
      if (is.finite(maxgap) && maxgap >= 0) {

        # Get logical vector of the time series via is.na() and then get the
        # run-length encoding of it. The run-length encoding describes how long
        # the runs of FALSE and TRUE are
        rlencoding <- rle(is.na(x))

        # Runs smaller than maxgap (which shall still be imputed) are set FALSE
        rlencoding$values[rlencoding$lengths <= maxgap] <- FALSE

        # The original vector is being reconstructed by reverse.rls, only now the
        # longer runs are replaced now in the logical vector derived from is.na()
        # in the beginning all former NAs that are > maxgap are also FALSE
        en <- inverse.rle(rlencoding)

        # Set all positions in the imputed series with gaps > maxgap to NA
        # (info from en vector)
        data[en == TRUE] <- NA
      }

      ##
      ## End Post Processing
      ##


      ##
      ## 4. Final Output Formatting
      ##

      # Give back the object originally supplied to the function
      # (necessary for multivariate input with only 1 column)
      if (!is.null(dim(x)[2])) {
        x[, 1] <- data
        return(x)
      }

      ##
      ## End Final Output Formatting
      ##
    }

    return(data)
  }
}
