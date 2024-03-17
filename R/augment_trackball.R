#' Title
#'
#' @param data
#' @param x
#' @param y
#' @param time Which variable contains time. If no column includes time, use sampling_rate.
#' @param sampling_rate
#' @param rollmean_k
#' @param mouse_dcpm
#'
#' @return
#' @export
#' @import dplyr
#' @importFrom tidyr replace_na
#' @importFrom zoo rollmean
#'
#' @examples
augment_trackball <- function(
    data,
    x = y_Right,
    y = y_Left,
    time = NULL,
    sampling_rate = 125,
    rollmean_k = 30,
    mouse_dcpm = 394
    ) {
  data <- data |>
    mutate(row = row_number(),
           time = row / sampling_rate,
           x = y_Right,
           x = zoo::rollmean(x, k = rollmean_k, fill = NA),
           y = y_Left,
           y = zoo::rollmean(y, k = rollmean_k, fill = NA),
           cum_x = 0,
           cum_y = 0) |>
    select(!c(1:4)) |>
    mutate(distance = sqrt(x^2 + y^2) / mouse_dpcm,
           v_translation = distance * sampling_rate,
           direction = atan2(y,x),
           rotation = if_else(abs(x) > 1 & abs(y) > 1,
                              abs(lag(direction) - direction),
                              0),
           rotation = if_else(rotation > pi, 2*pi - rotation, rotation),
           v_rotation = rotation * sampling_rate) |>
    mutate(
      x = tidyr::replace_na(x, 0),
      y = tidyr::replace_na(y, 0))

  data$cum_x <- cumsum(data$x)
  data$cum_y <- cumsum(data$y)
  return(data)
}
