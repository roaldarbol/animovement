subplot_dist_to_centroid_hist <- function(data, keypoint){
  p <- data |>
    ggplot2::ggplot(aes(dist_to_centroid)) +
    ggplot2::geom_histogram() +
    ggplot2::ggtitle("", subtitle = keypoint)

  return(p)
}

subplot_dist_to_centroid_confidence <- function(data, keypoint){
  if (nrow(data) < 1000){
    p <- data |>
      ggplot2::ggplot(aes(dist_to_centroid, confidence)) +
      ggplot2::geom_point(alpha = 0.5) +
      ggplot2::ggtitle("", subtitle = keypoint)
  } else {
    p <- data |>
      ggplot2::ggplot(aes(dist_to_centroid, confidence)) +
      stat_density_2d(aes(fill = ..level..), geom = "polygon") +
      scale_fill_continuous(type = "viridis") +
      ggplot2::ggtitle("", subtitle = keypoint)
  }
  return(p)
}

subplot_confidence <- function(data, keypoint){
  minx <- if_else(min(data$confidence, na.rm = TRUE) < 0, min(data$confidence, na.rm = TRUE), 0)
  maxx <- max(data$confidence, na.rm = TRUE)
  p <- data |>
    ggplot2::ggplot(ggplot2::aes(confidence)) +
    coord_cartesian(xlim = c(-minx, maxx)) +
    ggplot2::scale_y_log10() +
    # ggplot2::scale_x_continuous(limits = c(minx, maxx)) +
    geom_histogram() +
    ggplot2::ggtitle("", subtitle = keypoint)
  return(p)
}
