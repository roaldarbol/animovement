#' Transform coordinates to egocentric reference frame
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Transforms Cartesian coordinates into an egocentric reference frame through
#' a two-step process: translation followed by rotation. First translates all
#' coordinates relative to a reference keypoint, then rotates the coordinate
#' system based on specified alignment points.
#'
#' @param data movement data frame with columns: time, individual, keypoint, x, y
#' @param to_keypoint character; keypoint to use as the new origin
#' @param alignment_points character vector of length 2 specifying the keypoint names
#'        to use for alignment
#' @param align_perpendicular logical; if TRUE, alignment_points will be rotated to be
#'        perpendicular to the 0-degree axis. If FALSE (default), alignment_points
#'        will be rotated to align with the 0-degree axis
#'
#' @details
#' This function combines translation and rotation to create an egocentric reference
#' frame. It:
#' 1. Translates all coordinates relative to the specified keypoint (to_keypoint)
#' 2. Rotates the coordinate system based on the alignment points
#'
#' The translation makes the reference keypoint the new origin (0,0), while the
#' rotation standardizes the orientation. This is particularly useful for:
#' * Creating egocentric reference frames
#' * Standardizing pose data across frames or individuals
#' * Analyzing relative motion patterns
#'
#' @examples
#' \dontrun{
#' # Transform coordinates to make nose the origin and align body axis
#' transformed_data <- transform_to_egocentric(
#'   data,
#'   to_keypoint = "nose",
#'   alignment_points = c("nose", "tail"),
#'   align_perpendicular = FALSE
#' )
#'
#' # Transform to make nose origin and ears perpendicular to forward axis
#' transformed_data <- transform_to_egocentric(
#'   data,
#'   to_keypoint = "nose",
#'   alignment_points = c("ear_left", "ear_right"),
#'   align_perpendicular = TRUE
#' )
#' }
#'
#' @return movement data frame in egocentric reference frame
#' @export
transform_to_egocentric <- function(
  data,
  to_keypoint, # Reference point for translation
  alignment_points, # Two keypoint names to use for alignment
  align_perpendicular = FALSE # If TRUE, alignment_points will be made perpendicular to 0°
) {
  # First translate
  translated_data <- translate_coords(data, to_keypoint = to_keypoint)

  # Then rotate
  transformed_data <- rotate_coords(
    translated_data,
    alignment_points,
    align_perpendicular
  )

  return(transformed_data)
}
