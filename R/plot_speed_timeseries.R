#' Plot Time Series of Keypoint Speed
#'
#' @description
#' Creates a multi-panel visualization of keypoint speed data over time.
#' Each keypoint gets its own panel showing its speed,
#' useful for analyzing movement intensity and identifying potential tracking issues.
#'
#' @param data A data frame containing tracked keypoint data with the following columns:
#'   - `time`: Numeric time values
#'   - `keypoint`: Factor specifying the keypoint names
#'   - `x`: x-coordinates
#'   - `y`: y-coordinates
#' @param y_max Optional numeric value specifying the maximum value for the y-axis.
#'   If NULL (default), the y-axis limit is automatically determined from the data.
#'
#' @return A ggplot object combining individual time series plots for each keypoint
#'   using patchwork. The plots are stacked vertically with shared axes and legends.
#'
#' @examples
#' \dontrun{
#' # Plot with automatic y-axis scaling
#' plot_speed_timeseries(movement_data)
#'
#' # Plot with fixed maximum speed of 100
#' plot_speed_timeseries(movement_data, y_max = 100)
#' }
#'
#' @seealso
#' - `plot_position_timeseries()` for plotting position data
#' - `calculate_speed()` for the speed calculation
#'
#' @export
plot_speed_timeseries <- function(data, y_max = NULL) {
  keypoints <- levels(data$keypoint)
  plot_ts <- list()

  # Calculate speeds for all keypoints
  data <- data |>
    dplyr::group_by(.data$keypoint) |>
    dplyr::mutate(speed = calculate_speed(.data$x, .data$y, .data$time)) |>
    dplyr::ungroup()

  for (j in seq_along(keypoints)) {
    df <- data |>
      dplyr::filter(.data$keypoint == keypoints[j])

    if (!all(is.na(df$x))){
      plot_ts[[j]] <- df |>
        subplot_speed_timeseries(keypoint = keypoints[j], y_max = y_max)
    } else {
      cli::cli_inform("All values for {keypoints[j]} were NA. Returning a blank plot.")
      plot_ts[[j]] <- ggplot2::ggplot() +
        ggplot2::ggtitle("", subtitle = keypoints[j])
    }
  }

  output_plot <- patchwork::wrap_plots(plot_ts, ncol = 1) +
    patchwork::plot_annotation(
      title = "Time Series of Keypoint Speed",
      subtitle = "Speed calculated as magnitude of velocity vector"
    ) +
    patchwork::plot_layout(
      axes = "collect",
      axis_titles = "collect",
      guides = "collect"
    )

  return(output_plot)
}

#' Create Individual Speed Time Series Plot for a Keypoint
#'
#' @description
#' Internal helper function that creates a single time series plot for one keypoint's
#' speed.
#'
#' @param data Data frame containing speed data for a single keypoint
#' @param keypoint Character string of the keypoint name (used for subtitle)
#' @param y_max Optional numeric value for maximum y-axis limit
#'
#' @return A ggplot object showing the time series for speed
#'
#' @keywords internal
subplot_speed_timeseries <- function(data, keypoint, y_max = NULL) {
  p <- data |>
    ggplot2::ggplot(aes(x = .data$time)) +
    ggplot2::geom_line(aes(y = .data$speed), colour = "#1f77b4") +
    ggplot2::labs(y = "Speed") +
    ggplot2::ggtitle("", subtitle = keypoint) +
    ggplot2::theme_linedraw()

  if (!is.null(y_max)) {
    p <- p + ggplot2::coord_cartesian(ylim = c(0, y_max))
  }

  return(p)
}
