#' Analyze the distribution of distances from keypoints to the centroid
#'
#' This function generates visualizations of the distances from each keypoint to
#' a calculated centroid in the data. By default, it produces histograms of the
#' distance distributions, but it can also create confidence plots if specified.
#'
#' @param data A data frame containing at least the columns `keypoint`, `x`, and `y`.
#' @param reference_keypoint The keypoint used as a reference to calculate the distance.
#' @param type Character string specifying the type of plot to create. Options are:
#'   - `"histogram"`: Histograms of the distance distributions (default)
#'   - `"confidence"`: Plots showing confidence intervals for the distances
#'
#' @return A `patchwork` object combining plots for each keypoint, visualizing
#' the distances to the centroid.
#'
#' @details
#' The centroid is computed using the `add_centroid` function and distances are
#' calculated with the `calculate_distance_to_centroid` function.
#' The function automatically excludes the centroid itself from the visualizations.
#' Histograms provide an overview of distance distributions, while confidence plots
#' summarize variability with intervals.
#'
#' @importFrom dplyr filter mutate
#' @importFrom patchwork wrap_plots plot_annotation plot_layout
#' @importFrom ggtext element_markdown
#' @importFrom cli cli_abort
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' data <- dplyr::tibble(
#'   keypoint = rep(c("head", "arm", "leg", "torso"), each = 10),
#'   x = rnorm(40, mean = 0, sd = 1),
#'   y = rnorm(40, mean = 0, sd = 1)
#' )
#'
#' # Plot histogram of distances
#' check_pose(data, reference_keypoint = "head", type = "histogram")
#'
#' # Plot confidence intervals
#' check_pose(data, reference_keypoint = "head", type = "confidence")
#' }
#'
#' @export
check_pose <- function(data, reference_keypoint, type = "histogram") {
  # Parameters
  keypoints <- unique(data$keypoint)
  n_keypoints <- length(keypoints)
  color_occurrence = "indianred"
  color_total = "steelblue"
  color_border = "black"
  na_plots <- list()
  d_ref <- data |>
    dplyr::filter(.data$keypoint == reference_keypoint) |>
    dplyr::mutate(keypoint = "reference_keypoint")
  data <- dplyr::bind_rows(data, d_ref)

  if (n_keypoints <= 1) {
    cli::cli_abort(
      "Can only check poses when the data contains more than 1 keypoint."
    )
  }

  # Calculate distance to centroid
  data <- data |>
    calculate_distance_to_centroid(centroid_name = "reference_keypoint") |>
    dplyr::filter(.data$keypoint != "reference_keypoint") |>
    dplyr::mutate(keypoint = factor(.data$keypoint))

  for (i in 1:length(keypoints)) {
    if (type == "histogram") {
      na_plots[[i]] <- data |>
        dplyr::filter(.data$keypoint == keypoints[i]) |>
        subplot_dist_to_centroid_hist(keypoint = keypoints[i])
    } else if (type == "confidence") {
      na_plots[[i]] <- data |>
        dplyr::filter(.data$keypoint == keypoints[i]) |>
        subplot_dist_to_centroid_confidence(keypoint = keypoints[i])
    }
  }

  output_plot <- patchwork::wrap_plots(na_plots) +
    patchwork::plot_annotation(
      title = "Distance from keypoint to centroid",
      subtitle = "Gap sizes (NAs in a row) ordered by most common",
      theme = theme(
        plot.subtitle = ggtext::element_markdown(lineheight = 1.1),
        legend.position = "bottom"
      )
    ) +
    patchwork::plot_layout(axes = "collect", axis_titles = "collect")
  if (type == "histogram") {
    output_plot <- output_plot + patchwork::plot_layout(guides = "collect")
  }
  return(output_plot)
}
