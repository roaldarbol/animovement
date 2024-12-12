filter_confidence <- function(data, threshold=0.6){
  d <- data |>
    dplyr::mutate(x = if_else(.data$confidence < threshold, NA, .data$x),
                  y = if_else(.data$confidence < threshold, NA, .data$y),
                  confidence = if_else(.data$confidence < threshold, NA, .data$confidence))
  return(d)
}
