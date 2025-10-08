#' Visualize the distribution of confidence values for each keypoint
#'
#' This function generates histograms showing the distribution of confidence
#' values for each keypoint in the dataset.
#'
#' @param data A data frame containing at least the columns `keypoint` and `confidence`.
#'
#' @return A `patchwork` object combining histograms for each keypoint, visualizing
#' the confidence value distributions.
#'
#' @details
#' - Each keypoint in the dataset is assigned its own histogram, showing the
#' frequency of different confidence values.
#' - Confidence values are grouped and visualized using the `subplot_confidence`
#' function.
#' - The combined plots use `patchwork` for alignment and styling.
#'
#' @importFrom dplyr filter
#' @importFrom patchwork wrap_plots plot_annotation plot_layout
#' @importFrom ggtext element_markdown
#'
#' @examples
#' library(dplyr)
#' library(patchwork)
#' data <- dplyr::tibble(
#'   keypoint = rep(c("head", "arm", "leg", "torso"), each = 10),
#'   confidence = runif(40, min = 0, max = 1)
#' )
#' # Generate histograms of confidence distributions
#' check_confidence(data)
#'
#' @export
check_confidence <- function(data) {
  # Parameters
  keypoints <- unique(data$keypoint)
  n_keypoints <- length(keypoints)
  color_occurrence = "indianred"
  color_total = "steelblue"
  color_border = "black"
  na_plots <- list()

  for (i in 1:length(keypoints)) {
    na_plots[[i]] <- data |>
      dplyr::filter(.data$keypoint == keypoints[i]) |>
      subplot_confidence(keypoint = keypoints[i])
  }

  output_plot <- patchwork::wrap_plots(na_plots) +
    patchwork::plot_annotation(
      title = "Confidence",
      subtitle = "Number of observations by confidence",
      theme = theme(
        plot.subtitle = ggtext::element_markdown(lineheight = 1.1),
        legend.position = "bottom"
      )
    ) +
    patchwork::plot_layout(
      axes = "collect",
      axis_titles = "collect",
      guides = "collect"
    )

  return(output_plot)
}
