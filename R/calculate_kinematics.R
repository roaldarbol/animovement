#' Calculate kinematics
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Calculate kinematics.
#'
#' @param data Data frame
#'
#' @return A data frame with kinematics calculated
#' @export
#' @import dplyr
#' @importFrom rlang :=
#' @importFrom rlang .data
#'
calculate_kinematics <- function(
    data) {
  # We first temporarily back-calculate from our xy coordinates to the distances (dx, dy) covered between each observation (which is what we got from the sensors initially)
  data <- data |>
    dplyr::group_by(.data$individual, .data$keypoint) |>
    dplyr::mutate(
      dx = .data$x - lag(.data$x),
      dy = .data$y - lag(.data$y),
      # dt = .data$time - lag(.data$time)
    )

  # Find the sampling rate
  # sampling_rate <- round(1 / stats::median(data$dt, na.rm = TRUE))

  # Calculate kinematics
  data <- data |>
    dplyr::group_by(.data$individual, .data$keypoint) |>
    dplyr::mutate(
      distance = calculate_distance(.data$dx, .data$dy),
      v_translation = calculate_derivative(.data$distance, 0, .data$time, lag(.data$time)),
      a_translation = calculate_derivative(.data$v_translation, lag(.data$v_translation), .data$time, lag(.data$time)),
      direction = calculate_direction(.data$dx, .data$dy),
      rotation = calculate_angular_difference(.data$direction, lag(.data$direction)),
      v_rotation = calculate_derivative(0, .data$rotation, .data$time, lag(.data$time)),
      a_rotation = calculate_derivative(.data$v_rotation, lag(.data$v_rotation), .data$time, lag(.data$time)),
      # We change the directions to stay within 2pi only here, otherwise rotation becomes harder to alculate
      direction = adjust_direction(.data$direction) # Keep direction between 0 and 2*pi
    )

  # Remove leftover columns
  data <- data |>
    select(-c("dx", "dy"))
  # select(-c("dx", "dy", "dt"))

  return(data)
}

#' Calculate distance (Pythagoras)
#' Calculate distance from an x and y distance, using Pythagoras theorem.
#' @param dx dx
#' @param dy dy
#' @keywords internal
calculate_distance <- function(dx, dy) {
  sqrt(dx^2 + dy^2)
}

#' Calculate direction
#' Calculate direction (angle) from x and y distance using the (two-argument) arc-tangent. Converts to `circular`.
#' @inheritParams calculate_distance
#' @importFrom circular circular
#' @keywords internal
calculate_direction <- function(dx, dy) {
  if_else(dx == 0 & dy == 0, NA, circular::circular(atan2(dy, dx), modulo = "asis"))
}

#' Calculate angular difference
#' @param from_angle From angle
#' @param to_angle To angle
#' @keywords internal
calculate_angular_difference <- function(from_angle, to_angle) {
  ensure_circular(from_angle)
  ensure_circular(to_angle)
  diff_angle <- from_angle - to_angle
  case_when(diff_angle > pi ~ diff_angle - 2 * pi,
    diff_angle < -pi ~ diff_angle + 2 * pi,
    .default = diff_angle
  )
}

#' Calculate the derivative (dx/dt)
#' Calculate the derivative (dx/dt) with four arguments
#' @param from_x Current x value
#' @param to_x Lagging x value
#' @param from_t Current timestamp
#' @param to_t Lagging timestamp
#' @keywords internal
calculate_derivative <- function(from_x, to_x, from_t, to_t) {
  (from_x - to_x) / (from_t - to_t)
}

#' Adjust direction
#' Constrains the direction to be between 0 and 2pi
#' @param direction Direction
#' @importFrom circular circular
#' @keywords internal
adjust_direction <- function(direction) {
  circular::circular(direction, modulo = "2pi")
}

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

calculate_centroid_x <- function(x){
 mean(x, na.rm=TRUE)
}

calculate_centroid_y <- function(y){
  mean(y, na.rm=TRUE)
}
