#' Filter low-confidence values in a dataset
#'
#' This function replaces values in columns `x`, `y`, and `confidence` with `NA`
#' if the confidence values are below a specified threshold.
#'
#' @param data A data frame containing the columns `x`, `y`, and `confidence`.
#' @param threshold A numeric value specifying the minimum confidence level to
#' retain data. Default is 0.6.
#'
#' @return A data frame with the same structure as the input, but where `x`, `y`,
#' and `confidence` values are replaced with `NA` if the confidence is below the threshold.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr if_else
#'
#' @examples
#' library(dplyr)
#' data <- dplyr::tibble(
#'   x = 1:5,
#'   y = 6:10,
#'   confidence = c(0.5, 0.7, 0.4, 0.8, 0.9)
#' )
#' filter_na_confidence(data, threshold = 0.6)
#'
#' @export
filter_na_confidence <- function(data, threshold=0.6){
  d <- data |>
    dplyr::mutate(x = dplyr::if_else(.data$confidence < threshold, NA, .data$x),
                  y = dplyr::if_else(.data$confidence < threshold, NA, .data$y),
                  confidence = dplyr::if_else(.data$confidence < threshold, NA, .data$confidence))
  return(d)
}
