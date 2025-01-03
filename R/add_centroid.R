#' Add Centroid to Movement Data
#'
#' @description
#' Calculates and adds a centroid point to movement tracking data. The centroid
#' represents the mean position of selected keypoints at each time point.
#'
#' @param data A data frame containing movement tracking data with the following
#'   required columns:
#'   - `individual`: Identifier for each tracked subject
#'   - `keypoint`: Factor specifying tracked points
#'   - `time`: Time values
#'   - `x`: x-coordinates
#'   - `y`: y-coordinates
#'   - `confidence`: Confidence values for tracked points
#' @param include_keypoints Optional character vector specifying which keypoints
#'   to use for centroid calculation. If NULL (default), all keypoints are used
#'   unless `exclude_keypoints` is specified.
#' @param exclude_keypoints Optional character vector specifying which keypoints
#'   to exclude from centroid calculation. If NULL (default), no keypoints are
#'   excluded unless `include_keypoints` is specified.
#' @param centroid_name Character string specifying the name for the centroid
#'   keypoint (default: "centroid")
#'
#' @return A data frame with the same structure as the input, but with an
#'   additional keypoint representing the centroid. The centroid's confidence
#'   values are set to NA.
#'
#' @details
#' The function calculates the centroid as the mean x and y position of the
#' selected keypoints at each time point for each individual. Keypoints can be
#' selected either by specifying which ones to include (`include_keypoints`) or
#' which ones to exclude (`exclude_keypoints`). The resulting centroid is added
#' as a new keypoint to the data frame.
#'
#' @examples
#' \dontrun{
#' # Add centroid using all keypoints
#' add_centroid(movement_data)
#'
#' # Calculate centroid using only specific keypoints
#' add_centroid(movement_data,
#'             include_keypoints = c("head", "thorax", "abdomen"))
#'
#' # Calculate centroid excluding certain keypoints
#' add_centroid(movement_data,
#'             exclude_keypoints = c("antenna_left", "antenna_right"),
#'             centroid_name = "body_centroid")
#' }
#'
#' @seealso
#' `convert_nan_to_na()` for NaN handling in the centroid calculation
#'
#' @importFrom dplyr filter group_by summarise mutate arrange bind_rows
#'
#' @export
add_centroid <- function(data,
                         include_keypoints=NULL,
                         exclude_keypoints=NULL,
                         centroid_name="centroid"){
  # Check that centroid isn't there
  # Check that it's a movement data frame
  # To be optimised with collapse later on
  if (!is.null(include_keypoints)){
    df_centroid <- data |>
      dplyr::filter(.data$keypoint %in% include_keypoints)
  } else if (!is.null(exclude_keypoints)){
    df_centroid <- data |>
      dplyr::filter(!.data$keypoint %in% exclude_keypoints)
  } else {
    df_centroid <- data
  }

  df_centroid <- df_centroid |>
    dplyr::group_by(.data$individual, .data$time) |>
    dplyr::summarise(x = mean(.data$x, na.rm=TRUE),
                     y = mean(.data$y, na.rm=TRUE),
                     confidence = NA,
                     .groups = "keep") |>
    dplyr::mutate(keypoint = factor(as.character(centroid_name))) |>
    convert_nan_to_na()

  data <- bind_rows(data, df_centroid) |>
    dplyr::arrange(.data$time, .data$individual, .data$keypoint)

  return(data)
}
