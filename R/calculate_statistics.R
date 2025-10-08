#' Calculate summary statistics
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Calculate summary statistics for tracks
#'
#' @param data A kinematics data frame
#' @param measures Measures of central tendency and dispersion. Options are `median_mad` (default) and `mean_sd`. See description for more information.
#' @param straightness Which method to calculate path straightness. Choose between "A" (default), "B", "C"... or a combination (e.g. "c("A","B")"). See description for details about the different calculations.
#' @return An data frame data frame with kinematics calculated
#' @export
#' @import dplyr
#' @import circular
#' @importFrom collapse fmean fmedian fsd
#' @importFrom rlang :=
#' @importFrom rlang .data
#'
calculate_statistics <- function(
  data,
  measures = "median_mad",
  straightness = c("A", "B", "C", "D")
) {
  validate_statistics()

  # Ungroup (makes summarise work better)
  data <- data |>
    dplyr::ungroup()

  # Calculate translational and rotational separately (maybe?) and gather at the end
  totals <- data |>
    dplyr::summarise(
      across(
        c("d_translation", "d_rotation"),
        ~ collapse::fsum(abs(.x)),
        .names = "total_{.col}"
      ),
      across(
        c("x", "y"),
        ~ dplyr::last(.x, na_rm = TRUE),
        .names = "last_{.col}"
      ),
      .by = c("individual", "keypoint")
    )

  totals <- totals |>
    calculate_straightness(straightness)
  # dplyr::mutate("straightness_{{ A }} = calculate_straightness(.data$last_x, .data$last_y, .data$total_distance, method = straightness))

  if (measures == "median_mad") {
    data <- data |>
      dplyr::summarise(
        across(
          c("direction"),
          ~ collapse::fmean(circular::circular(.x, modulo = "2pi")),
          .names = "median_{.col}"
        ),
        across(
          c("direction"),
          ~ calculate_circular_mad(circular::circular(.x, modulo = "2pi")),
          .names = "mad_{.col}"
        ),
        across(
          c("v_translation", "a_translation", "v_rotation", "a_rotation"),
          ~ collapse::fmedian(abs(.x)),
          .names = "median_{.col}"
        ),
        across(
          c("v_translation", "a_translation", "v_rotation", "a_rotation"),
          ~ stats::mad(abs(.x), na.rm = TRUE),
          .names = "mad_{.col}"
        ),
        .by = c("individual", "keypoint")
      ) |>
      left_join(totals) |>
      select(-c("last_x", "last_y")) |>
      suppressMessages()
  } else if (measures == "mean_sd") {
    data <- data |>
      dplyr::summarise(
        across(
          c("direction"),
          ~ collapse::fmean(circular::circular(.x)),
          .names = "mean_{.col}"
        ),
        across(
          c("direction"),
          ~ circular::sd(circular::circular(.x)),
          .names = "sd_{.col}"
        ),
        across(
          c("v_translation", "a_translation", "v_rotation", "a_rotation"),
          ~ collapse::fmean(abs(.x)),
          .names = "mean_{.col}"
        ),
        across(
          c("v_translation", "a_translation", "v_rotation", "a_rotation"),
          ~ collapse::fsd(abs(.x)),
          .names = "sd_{.col}"
        ),
        .by = c("individual", "keypoint")
      ) |>
      left_join(totals) |>
      select(-c("last_x", "last_y")) |>
      suppressMessages()
  }

  return(data)
}

#' Calculate circular Median Absolute Deviation (MAD)
#' @param angles Vector of angles
#' @importFrom circular circular
#' @importFrom collapse fmedian
#' @keywords internal
calculate_circular_mad <- function(angles) {
  ensure_circular(angles)
  # Convert angles to circular object
  angles_circular <- circular::circular(angles, units = "radians")

  # Calculate circular median
  circular_median <- collapse::fmedian(angles_circular)

  # Compute absolute deviations from the circular median
  abs_dev <- abs(angles_circular - circular_median)

  # Calculate and return the median absolute deviation
  collapse::fmedian(abs_dev)
}
