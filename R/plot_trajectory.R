check_trajectory <- function(
  data,
  reference_keypoint = NULL,
  coord_fixed = FALSE
) {
  n_keypoints <- nlevels(data$keypoint)
  keypoints <- levels(data$keypoint)
  plot_trajectories <- list()

  if (!is.null(reference_keypoint) && reference_keypoint %in% keypoints) {
    data <- data |>
      translate_coords(to_keypoint = reference_keypoint)
  }

  for (j in 1:length(keypoints)) {
    df <- data |>
      dplyr::ungroup() |>
      dplyr::filter(.data$keypoint == keypoints[j])

    plot_trajectories[[j]] <- df |>
      subplot_trajectory(keypoint = keypoints[j], coord_fixed = coord_fixed)
  }

  output_plot <- patchwork::wrap_plots(plot_trajectories) +
    patchwork::plot_annotation(
      title = "Trajectory of keypoints",
      subtitle = "Trajecotry of individual keypoints over time",
      theme = theme(
        plot.subtitle = ggtext::element_markdown(lineheight = 1.1),
        legend.position = "right"
      )
    ) +
    patchwork::plot_layout(
      axes = "collect",
      axis_titles = "collect",
      guides = "collect"
    )

  return(output_plot)
}
