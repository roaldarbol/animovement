#' We check the quality in multiple ways.
#' First, we check the NAs. We can produce plots to show *how prevalent* they are, *when* they occur, *how long* they are
#' We can also make a "compare" function, which gives is a before/after filtering picture.

#' Title
#'
#' @param data
#' @param by_keypoint
#'
#' @return
#' @export
#'
#' @examples
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

      na_plots[[j]] <- ggplot_na_timing(df, keypoint = keypoints[j], title = NULL, subtitle = NULL)
    }
  } else {
    df <- data |>
      dplyr::select("x")

    na_plots[[1]] <- ggplot_na_timing(df, title = NULL, subtitle = NULL)
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

#' Title
#'
#' @param data
#' @param limit
#' @param include_total
#' @param by_keypoint
#'
#' @return
#' @export
#'
#' @examples
check_na_gapsize <- function(data,
                            limit = 10,
                            include_total = TRUE,
                            by_keypoint = TRUE){
  individuals <- unique(data$individual)
  n_individuals <- length(individuals)
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

check_poses <- function(data, type = "histogram"){
  # Parameters
  individuals <- unique(data$individual)
  n_individuals <- length(individuals)
  keypoints <- unique(data$keypoint)
  n_keypoints <- length(keypoints)
  color_occurrence = "indianred"
  color_total = "steelblue"
  color_border = "black"
  na_plots <- list()
  centroid_name <- "test_centroid"

  if (n_keypoints <= 1){
    cli::cli_abort("Can only check poses when the data contains more than 1 keypoint.")
  }

  # Calculate distance to centroid
  data <- data |>
    add_centroid(centroid_name = centroid_name) |>
    calculate_distance_to_centroid(centroid_name = centroid_name) |>
    dplyr::filter(.data$keypoint != centroid_name) |>
    dplyr::mutate(keypoint = factor(.data$keypoint))

  for (i in 1:length(keypoints)){
    if (type == "histogram"){
      na_plots[[i]] <- data |>
        dplyr::filter(.data$keypoint == keypoints[i]) |>
        subplot_dist_to_centroid_hist(keypoint = keypoints[i])
    } else if (type == "confidence"){
      na_plots[[i]] <- data |>
        dplyr::filter(.data$keypoint == keypoints[i]) |>
        subplot_dist_to_centroid_confidence(keypoint = keypoints[i])
    }
  }

  output_plot <- patchwork::wrap_plots(na_plots) +
    patchwork::plot_annotation(title = "Distance from keypoint to centroid",
                               subtitle = "Gap sizes (NAs in a row) ordered by most common",
                               theme = theme(plot.subtitle = ggtext::element_markdown(lineheight = 1.1),
                                             legend.position="bottom")
    ) +
    patchwork::plot_layout(axes = "collect",
                           axis_titles = "collect")
  if (type == "histogram"){
    output_plot <- output_plot + patchwork::plot_layout(guides = "collect")
  }
  return(output_plot)
}

check_confidence <- function(data){
  # Parameters
  individuals <- unique(data$individual)
  n_individuals <- length(individuals)
  keypoints <- unique(data$keypoint)
  n_keypoints <- length(keypoints)
  color_occurrence = "indianred"
  color_total = "steelblue"
  color_border = "black"
  na_plots <- list()

  for (i in 1:length(keypoints)){
    na_plots[[i]] <- data |>
      dplyr::filter(.data$keypoint == keypoints[i]) |>
      subplot_confidence(keypoint = keypoints[i])
  }

  output_plot <- patchwork::wrap_plots(na_plots) +
    patchwork::plot_annotation(title = "Confidence",
                               subtitle = "Number of observations by confidence",
                               theme = theme(plot.subtitle = ggtext::element_markdown(lineheight = 1.1),
                                             legend.position="bottom")
    ) +
    patchwork::plot_layout(axes = "collect",
                           axis_titles = "collect",
                           guides = "collect")

  return(output_plot)
}
