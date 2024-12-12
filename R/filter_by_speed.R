#' Filter values by speed threshold
#'
#' This function filters out values in a dataset where the calculated speed exceeds
#' a specified threshold. Values for `x`, `y`, and `confidence` are replaced with
#' `NA` if their corresponding speed exceeds the threshold. Speed is calculated
#' using the `calculate_kinematics` function.
#'
#' @param data A data frame containing at least the columns `x` and `y`.
#' @param threshold A numeric value specifying the speed threshold. Observations
#' with speeds greater than this value will have their `x`, `y`, and `confidence`
#' values replaced with `NA`. If set to `"auto"`, the function will throw an error
#' as automatic threshold determination is not yet implemented.
#'
#' @return A data frame with the same columns as the input `data`, but with
#' values replaced by `NA` where the speed exceeds the threshold.
#'
#' @details
#' - The speed is calculated using the `calculate_kinematics` function, which
#' computes translational velocity (`v_translation`) and other kinematic parameters.
#' - The `"auto"` threshold option is currently not implemented and will result
#' in an error.
#' - The output data frame retains the original columns and structure of the input
#' data.
#'
#' @importFrom dplyr mutate if_else select
#' @importFrom cli cli_abort
#'
#' @examples
#' library(dplyr)
#' data <- tibble::tibble(
#'   x = c(1, 2, 4, 7, 11),
#'   y = c(1, 1, 2, 3, 5),
#'   confidence = c(0.8, 0.9, 0.7, 0.85, 0.6)
#' )

#' # Filter data by a speed threshold of 3
#' filter_by_speed(data, threshold = 3)
#'
#' @export
filter_by_speed <- function(data, threshold = "auto"){
  n_cols <- ncol(data)
  d <- data |>
    calculate_kinematics()
  if (threshold == "auto"){
    cli::cli_abort("Not implemented yet.")
  } else {
  d <- d |>
    dplyr::mutate(x = dplyr::if_else(abs(.data$v_translation) < threshold, NA, .data$x),
                  y = dplyr::if_else(abs(.data$v_translation) < threshold, NA, .data$y),
                  confidence = dplyr::if_else(abs(.data$v_translation) < threshold, NA, .data$confidence)) |>
    dplyr::select(1:n_cols)
  }

  return(d)
}
