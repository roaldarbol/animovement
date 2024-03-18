#' Augment Trackball Data
#'
#' @param data Data frame
#' @param x Y-data from first sensor.
#' @param y Y-data from second sensor.
#' @param time Which variable contains time. If no column includes time, use sampling_rate.
#' @param sampling_rate Sampling rate of the sensors.
#' @param rollmean_k Parameter for smoothing filter.
#' @param mouse_dpcm "Dots-per-cm" - or how little movement the mouse can detect.
#'
#' @return An augmented data frame
#' @export
#' @import dplyr
#' @importFrom tidyr replace_na
#' @importFrom zoo rollmean
#' @importFrom rlang :=
#' @importFrom rlang .data
#'
augment_trackball <- function(
    data,
    x,
    y,
    time = NULL,
    sampling_rate = 125,
    rollmean_k = 30,
    mouse_dpcm = 394
    ) {
  data <- data |>
    dplyr::mutate(
      row = dplyr::row_number(),
      time = row / sampling_rate,
      {{ x }} := zoo::rollmean({{ x }}, k = rollmean_k, fill = NA),
      {{ y }} := zoo::rollmean({{ y }}, k = rollmean_k, fill = NA),
      "cum_{{ x }}" := 0,
      "cum_{{ y }}" := 0
      ) |>
    dplyr::mutate(distance = sqrt({{ x }}^2 + {{ y }}^2) / mouse_dpcm,
           v_translation = .data$distance * sampling_rate,
           direction = atan2({{ y }}, {{ x }}),
           rotation = dplyr::if_else(abs({{ x }}) > 1 & abs({{ y }}) > 1,
                              abs(dplyr::lag(.data$direction) - .data$direction),
                              0),
           rotation = dplyr::if_else(.data$rotation > pi, 2*pi - .data$rotation, .data$rotation),
           v_rotation = .data$rotation * sampling_rate) |>
    dplyr::mutate(
      {{ x }} := tidyr::replace_na({{ x }}, 0),
      {{ y }} := tidyr::replace_na({{ y }}, 0))

  data <- data |>
    dplyr::mutate(
      "cum_{{ x }}" := cumsum({{ x }}),
      "cum_{{ y }}" := cumsum({{ y }})
      )
  return(data)
}
