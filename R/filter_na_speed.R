#' Filter values by speed threshold
#'
#' @description
#' This function filters out values in a dataset where the calculated speed exceeds
#' a specified threshold. Values for `x`, `y`, and `confidence` are replaced with
#' `NA` if their corresponding speed exceeds the threshold. Speed is calculated
#' using the `calculate_kinematics` function.
#'
#' @param data A data frame containing the following required columns:
#'   - `x`: x-coordinates
#'   - `y`: y-coordinates
#'   - `time`: time values
#' Optional column:
#'   - `confidence`: confidence values for each observation
#' @param threshold A numeric value specifying the speed threshold, or "auto".
#'   - If numeric: Observations with speeds greater than this value will have their
#'     `x`, `y`, and `confidence` values replaced with `NA`
#'   - If "auto": Sets threshold at mean speed + 3 standard deviations
#'
#' @return A data frame with the same columns as the input `data`, but with
#' values replaced by `NA` where the speed exceeds the threshold.
#'
#' @details
#' The speed is calculated using the `calculate_kinematics` function, which
#' computes translational velocity (`v_translation`) and other kinematic parameters.
#' When using `threshold = "auto"`, the function calculates the threshold as the
#' mean speed plus three standard deviations, which assumes normally distributed speeds.
#'
#' @importFrom dplyr mutate if_else select
#' @importFrom cli cli_abort
#'
#' @examples
#' \dontrun{
#' data <- dplyr::tibble(
#'   time = 1:5,
#'   x = c(1, 2, 4, 7, 11),
#'   y = c(1, 1, 2, 3, 5),
#'   confidence = c(0.8, 0.9, 0.7, 0.85, 0.6)
#' )
#'
#' # Filter data by a speed threshold of 3
#' filter_by_speed(data, threshold = 3)
#'
#' # Use automatic threshold
#' filter_by_speed(data, threshold = "auto")
#' }
#'
#' @export
filter_na_speed <- function(data, threshold = "auto") {
  # Check required columns
  required_cols <- c("x", "y", "time")
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Missing required columns in data:",
      "x" = paste(missing_cols, collapse = ", ")
    ))
  }

  # Check that x, y, time are numeric
  non_numeric_cols <- sapply(data[required_cols], function(col) {
    !is.numeric(col)
  })
  if (any(non_numeric_cols)) {
    bad_cols <- names(non_numeric_cols)[non_numeric_cols]
    cli::cli_abort(c(
      "The following columns must be numeric:",
      "x" = paste(bad_cols, collapse = ", ")
    ))
  }

  # Check threshold input
  if (!identical(threshold, "auto") && !is.numeric(threshold)) {
    cli::cli_abort("threshold must be either 'auto' or a numeric value")
  }

  n_cols <- ncol(data)
  d <- data |>
    calculate_kinematics()

  if (identical(threshold, "auto")) {
    threshold <- mean(d$v_translation, na.rm = TRUE) +
      3 * sd(d$v_translation, na.rm = TRUE)
  }

  d <- d |>
    dplyr::mutate(
      x = dplyr::if_else(abs(.data$v_translation) > threshold, NA, .data$x),
      y = dplyr::if_else(abs(.data$v_translation) > threshold, NA, .data$y)
    )

  # Only modify confidence if it exists
  if ("confidence" %in% names(d)) {
    d <- d |>
      dplyr::mutate(
        confidence = dplyr::if_else(
          abs(.data$v_translation) > threshold,
          NA,
          .data$confidence
        )
      )
  }

  d <- d |> dplyr::select(all_of(1:n_cols))

  return(d)
}
