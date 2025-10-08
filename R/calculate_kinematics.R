#' Calculate kinematics from position data
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Calculates kinematic measurements including translational and rotational motion from
#' position data. The function computes velocities, accelerations, and angular measurements
#' from x-y coordinate time series data.
#'
#' @param data A data frame containing at minimum:
#'   * time (numeric): Time points of measurements
#'   * x (numeric): X-coordinates
#'   * y (numeric): Y-coordinates
#' @param by Character vector specifying additional grouping variables (optional).
#'   If the input data frame is already grouped, those groups will be preserved
#'   and any additional groups specified in `by` will be added.
#'
#' @returns A data frame containing the original data plus calculated kinematics:
#'   * distance: Distance traveled between consecutive points
#'   * v_translation: Translational velocity
#'   * a_translation: Translational acceleration
#'   * direction: Movement direction in radians
#'   * rotation: Angular change between consecutive points
#'   * v_rotation: Angular velocity
#'   * a_rotation: Angular acceleration
#'
#' @section Warning:
#' Time points should be regularly sampled for accurate derivatives.
#'
#' @examples
#' # Basic usage with just x-y coordinates
#' df <- data.frame(
#'   time = 1:10,
#'   x = runif(10),
#'   y = runif(10)
#' )
#' calculate_kinematics(df)
#'
#' # Using with grouping variables
#' df_grouped <- data.frame(
#'   time = rep(1:5, 2),
#'   x = runif(10),
#'   y = runif(10),
#'   individual = rep(c("A", "B"), each = 5)
#' )
#' calculate_kinematics(df_grouped, by = "individual")
#'
#' @export
#' @import dplyr
#' @importFrom rlang := .data
#'
calculate_kinematics <- function(data, by = NULL) {
  # Validate input
  required_cols <- c("time", "x", "y")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      c(
        "Missing required columns in input data:",
        "x" = "{.val {missing_cols}}"
      )
    )
  }

  # Preserve existing groups and add new ones if specified
  if (!is.null(by)) {
    missing_by_cols <- setdiff(by, names(data))
    if (length(missing_by_cols) > 0) {
      cli::cli_abort(
        c(
          "Grouping columns specified in 'by' not found in data:",
          "x" = "{.val {missing_by_cols}}"
        )
      )
    }
    data <- data |>
      group_by(across(all_of(by)), .add = TRUE)
  }

  # Calculate intermediate values
  data <- data |>
    dplyr::mutate(
      dx = .data$x - lag(.data$x),
      dy = .data$y - lag(.data$y)
    )

  # Calculate kinematics
  data <- data |>
    dplyr::mutate(
      d_translation = calculate_distance(.data$dx, .data$dy),
      v_translation = calculate_derivative(
        .data$d_translation,
        0,
        .data$time,
        lag(.data$time)
      ),
      a_translation = calculate_derivative(
        .data$v_translation,
        lag(.data$v_translation),
        .data$time,
        lag(.data$time)
      ),
      direction = calculate_direction(.data$dx, .data$dy),
      d_rotation = calculate_angular_difference(
        .data$direction,
        lag(.data$direction)
      ),
      v_rotation = calculate_derivative(
        0,
        .data$d_rotation,
        .data$time,
        lag(.data$time)
      ),
      a_rotation = calculate_derivative(
        .data$v_rotation,
        lag(.data$v_rotation),
        .data$time,
        lag(.data$time)
      ),
      direction = constrain_angles_radians(.data$direction)
    )

  # Remove intermediate columns
  data |>
    select(-c("dx", "dy"))
}
