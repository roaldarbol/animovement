subplot_trajectory <- function(data, keypoint, coord_fixed){
  p <- data |>
    tidyr::drop_na(.data$x,.data$y) |>
    ggplot2::ggplot(aes(.data$x, .data$y, colour = .data$time)) +
    ggplot2::geom_path(alpha = 0.6) +
    ggplot2::scale_colour_viridis_c() +
    ggplot2::guides(colour = ggplot2::guide_colorbar("Time")) +
    ggplot2::ggtitle("", subtitle = keypoint) +
    ggplot2::theme_linedraw()

  if (isTRUE(coord_fixed)){
    p <- p + ggplot2::coord_fixed()
  }

  return(p)
}
