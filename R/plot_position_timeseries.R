#' Plot Time Series of Keypoint Position
#'
#' @description
#' Creates a multi-panel visualization of keypoint position data over time.
#' Each keypoint gets its own panel showing its x and/or y coordinates,
#' with different colors distinguishing between x (orange) and y (blue) coordinates.
#' Useful for visually inspecting movement patterns and identifying potential tracking issues.
#'
#' @param data A data frame containing tracked keypoint data with the following columns:
#'   - `time`: Numeric time values
#'   - `keypoint`: Factor specifying the keypoint names
#'   - `x`: x-coordinates
#'   - `y`: y-coordinates
#' @param reference_keypoint Optional character string. If provided, all coordinates will
#'   be translated relative to this keypoint's position. Must match one of the keypoint
#'   levels in the data.
#' @param dimension Character string specifying which coordinates to plot.
#'   Options are:
#'   - `"xy"`: Plot both x and y coordinates (default)
#'   - `"x"`: Plot only x coordinates
#'   - `"y"`: Plot only y coordinates
#'
#' @return A ggplot object combining individual time series plots for each keypoint
#'   using patchwork. The plots are stacked vertically with shared axes and legends.
#'
#' @examples
#' \dontrun{
#' # Plot all coordinates
#' check_timeseries(movement_data)
#'
#' # Plot coordinates relative to "head" keypoint
#' check_timeseries(movement_data, reference_keypoint = "head")
#'
#' # Plot only x coordinates
#' check_timeseries(movement_data, dimension = "x")
#' }
#'
#' @seealso
#' `translate_coords()` for the coordinate translation functionality used when
#' `reference_keypoint` is specified.
#'
#' @export
plot_position_timeseries <- function(data, reference_keypoint=NULL, dimension = "xy"){
  n_keypoints <- nlevels(data$keypoint)
  keypoints <- levels(data$keypoint)
  plot_ts <- list()
  orange <- "#FFA500"
  blue <- "#1f77b4"

  if (!is.null(reference_keypoint) && reference_keypoint %in% keypoints){
    data <- data |>
      translate_coords(to_keypoint = reference_keypoint)
  }

  for (j in 1:length(keypoints)){
    df <- data |>
      dplyr::ungroup() |>
      dplyr::filter(.data$keypoint == keypoints[j])

    plot_ts[[j]] <- df |>
      subplot_position_timeseries(keypoint = keypoints[j], dimension = dimension)
  }

  output_plot <- patchwork::wrap_plots(plot_ts, ncol = 1) +
    patchwork::plot_annotation(title = "Time Series of Keypoint Position",
                               theme = theme(plot.subtitle = ggtext::element_markdown(lineheight = 1.1))) +
    patchwork::plot_layout(axes = "collect",
                           axis_titles = "collect",
                           guides = "collect")
  if (dimension == "xy"){
    output_plot <- output_plot +
      patchwork::plot_annotation(subtitle = paste0(
      "Timeseries for <b style='color:", orange, ";' >X</b> and <b style='color:", blue, "' >Y</b> coordinates
                    over time"))
  } else if (dimension == "x"){
    output_plot <- output_plot +
      patchwork::plot_annotation(subtitle = paste0(
        "Timeseries for <b style='color:", orange, ";' >X</b> coordinates
                    over time"))
  } else if (dimension == "y"){
    output_plot <- output_plot +
      patchwork::plot_annotation(subtitle = paste0(
        "Timeseries for <b style='color:", blue, "' >Y</b> coordinates
                    over time"))
  }

  return(output_plot)
}

#' Create Individual Time Series Plot for a Keypoint
#'
#' @description
#' Internal helper function that creates a single time series plot for one keypoint's
#' coordinates.
#'
#' @param data Data frame containing coordinate data for a single keypoint
#' @param keypoint Character string of the keypoint name (used for subtitle)
#' @param dimension Character string: "xy", "x", or "y"
#'
#' @return A ggplot object showing the time series for the specified coordinates
#'
#' @keywords internal
subplot_position_timeseries <- function(data, keypoint, dimension = "xy") {
  if (dimension == "xy") {
    # Get ranges for scaling
    x_range <- range(data$x, na.rm = TRUE)
    y_range <- range(data$y, na.rm = TRUE)

    # Create plot with dual axes
    p <- data |>
      ggplot2::ggplot(aes(x = .data$time)) +
      # X coordinate on primary y-axis
      ggplot2::geom_line(aes(y = .data$x), colour = "#FFA500") +
      # Y coordinate on secondary y-axis (scaled)
      ggplot2::geom_line(aes(y = scales::rescale(.data$y, to = x_range, from = y_range)),
                         colour = "#1f77b4") +
      # Primary axis label (X coordinate)
      ggplot2::scale_y_continuous(
        name = "X coordinate",
        sec.axis = ggplot2::sec_axis(
          # Transform function to convert back to original y-coordinate scale
          trans = ~ scales::rescale(., to = y_range, from = x_range),
          name = "Y coordinate"
        )
      )
  } else {
    # Single axis plots remain the same
    p <- data |>
      ggplot2::ggplot(aes(x = .data$time))

    if (dimension == "x") {
      p <- p + ggplot2::geom_line(aes(y = .data$x), colour = "#FFA500")
    } else if (dimension == "y") {
      p <- p + ggplot2::geom_line(aes(y = .data$y), colour = "#1f77b4")
    }
  }

  p <- p +
    ggplot2::ggtitle("", subtitle = keypoint) +
    ggplot2::theme_linedraw()

  return(p)
}
