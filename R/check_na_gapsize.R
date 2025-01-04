#' Visualize the occurrence of gap sizes in the data
#'
#' This function generates a plot showing the distribution of gap sizes (consecutive
#' `NA` values) in the data, either aggregated or broken down by keypoints.
#'
#' @param data A data frame containing at least the columns `x`
#' and `keypoint`.
#' @param limit An integer specifying the maximum gap size to include in the plot.
#' Default is 10.
#' @param include_total Logical. If `TRUE`, includes the total count of gaps of
#' each size in the plot. Default is `TRUE`.
#' @param by_keypoint Logical. If `TRUE`, generates a separate plot for each keypoint.
#' If `FALSE`, creates a single aggregated plot for all keypoints. Default is `TRUE`.
#'
#' @return A `patchwork` object combining one or more ggplots that visualize the
#' occurrence of gap sizes (consecutive `NA`s) in the data.
#'
#' @details
#' - The plot highlights the most common gap sizes in the data, ordered by frequency.
#' - Different colors represent the occurrence (`indianred`), total counts (`steelblue`),
#' and border outlines (`black`).
#' - The function uses `patchwork` to combine multiple plots when `by_keypoint = TRUE`.
#'
#' @importFrom dplyr ungroup filter select
#' @importFrom ggplot2 ggplot theme
#' @importFrom patchwork wrap_plots plot_annotation plot_layout
#' @importFrom ggtext element_markdown
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' library(patchwork)
#' data <- dplyr::tibble(
#'   x = c(NA, NA, 3, NA, 5, 6, NA, NA, NA, 10),
#'   keypoint = factor(rep(c("head", "arm"), each = 5))
#' )
#' check_na_gapsize(data, limit = 5, include_total = TRUE, by_keypoint = TRUE)
#'
#' @export
check_na_gapsize <- function(data,
                             limit = 10,
                             include_total = TRUE,
                             by_keypoint = TRUE){
  n_keypoints <- nlevels(data$keypoint)
  color_occurrence = "indianred"
  color_total = "steelblue"
  color_border = "black"
  keypoints <- levels(data$keypoint)
  na_plots <- list()

  if (by_keypoint == TRUE){
    for (j in 1:length(keypoints)){
      df <- data |>
        dplyr::ungroup() |>
        dplyr::filter(.data$keypoint == keypoints[j]) |>
        dplyr::select("x")

      na_plots[[j]] <- ggplot_na_gapsize(df,
                                         limit = limit,
                                         include_total = include_total,
                                         keypoint = keypoints[j],
                                         title = NULL,
                                         subtitle = NULL)
    }
  } else {
    df <- data |>
      dplyr::select("x")

    na_plots[[1]] <- ggplot_na_gapsize(df, title = NULL, subtitle = NULL)
  }

  output_plot <- patchwork::wrap_plots(na_plots) +
    patchwork::plot_annotation(title = "Occurrence of gap sizes",
                               subtitle = "Gap sizes (NAs in a row) ordered by most common",
                               theme = theme(plot.subtitle = ggtext::element_markdown(lineheight = 1.1),
                                             legend.position="bottom")
    ) +
    patchwork::plot_layout(axes = "collect",
                           axis_titles = "collect",
                           guides = "collect")

  return(output_plot)
}
