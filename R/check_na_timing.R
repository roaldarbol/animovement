#' Visualize the timing of missing values in the data
#'
#' This function generates a plot to visualize where missing values (`NA`s) occur
#' in the data over time. It can display separate plots for each keypoint or a
#' single aggregated plot for all keypoints.
#'
#' @param data A data frame containing at least the columns `x`, `individual`,
#' and `keypoint`.
#' @param by_keypoint Logical. If `TRUE`, generates a separate plot for each keypoint.
#' If `FALSE`, creates a single aggregated plot for all keypoints. Default is `TRUE`.
#'
#' @return A `patchwork` object combining one or more ggplots that show the
#' timing of missing values (`NA`) in the data.
#'
#' @details
#' - Missing values are highlighted using a red (`indianred2`) color, and non-missing
#' values are shown in blue (`steelblue`).
#' - The function uses the `patchwork` package to combine multiple plots when `by_keypoint = TRUE`.
#'
#' @importFrom dplyr ungroup filter select
#' @importFrom ggplot2 ggplot theme
#' @importFrom patchwork wrap_plots plot_annotation plot_layout
#' @importFrom ggtext element_markdown
#' @importFrom cli cli_inform
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' library(patchwork)
#' data <- dplyr::tibble(
#'   x = c(1, 2, NA, 4, NA, 6),
#'   individual = rep("A", 6),
#'   keypoint = factor(rep(c("head", "arm"), each = 3))
#' )
#' check_na_timing(data, by_keypoint = TRUE)
#'
#' @export
check_na_timing <- function(data,
                            by_keypoint = TRUE){

  individuals <- unique(data$individual)
  n_individuals <- length(individuals)
  n_keypoints <- nlevels(data$keypoint)
  color_missing = "indianred2"
  color_existing = "steelblue"
  keypoints <- levels(data$keypoint)
  na_plots <- list()
  if (by_keypoint == TRUE){
    for (j in 1:length(keypoints)){
      df <- data |>
        dplyr::ungroup() |>
        dplyr::filter(.data$keypoint == keypoints[j]) |>
        dplyr::select("x")

      if (!all(is.na(df$x))){
        na_plots[[j]] <- ggplot_na_timing(df, keypoint = keypoints[j], title = NULL, subtitle = NULL)
      } else {
        cli::cli_inform("All values for {keypoints[j]} were NA. Returning a blank plot.")
        na_plots[[j]] <- ggplot()
      }
    }
  } else {
    df <- data |>
      dplyr::select("x")

    if (!all(is.na(df$x))){
      na_plots[[1]] <- ggplot_na_timing(df, title = NULL, subtitle = NULL)
    } else {
      cli::cli_inform("All values for {keypoints[j]} were NA. Returning a blank plot.")
      na_plots[[j]] <- ggplot()
    }
  }

  output_plot <- patchwork::wrap_plots(na_plots) +
    patchwork::plot_annotation(title = "Missing Values by Time",
                               subtitle = paste0("Amount of <b style='color:", color_missing, ";' >NA</b>
                    and  <b style='color:", color_existing, "' >non-NA</b>
                    over time"),
                               theme = theme(plot.subtitle = ggtext::element_markdown(lineheight = 1.1))
    ) +
    patchwork::plot_layout(axes = "collect",
                           axis_titles = "collect")

  return(output_plot)
}
