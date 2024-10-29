#' Group every N observations together
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Sometimes your sampling rate is too high; group_every allows you to
#' down-sample by creating "bins" which can subsequently be summarised on. When using `n`, data
#' needs to be regularly sampled; if there are gaps in time, the bin duration will differ.
#' Works well with `calculate_summary()` for movement data.
#'
#' @param data Input data frame
#' @param seconds Number of seconds to bin together
#' @param n Number of observations to include in each bin/group
#'
#' @import dplyr
#' @importFrom cli cli_abort
#'
#' @return Grouped data frame, with new "bin" variable.
#' @export
#'
#' @examples
#' ## Group by every 5 seconds
#' df_time <- data.frame(
#'   time = seq(from = 0.02, to = 100, by = 1/30), # time at 30Hz, slightly offset
#'   y = rnorm(3000)) # random numbers
#'
#' df_time |>
#'   group_every(seconds = 5) |> # group for every 5 seconds
#'   dplyr::summarise(time = min(time), # summarise for time and y
#'                    mean_y = mean(y)) |>
#'   dplyr::mutate(time = floor(time)) # floor to get the round second number
#'
#' # Group every n observations
#' df <- data.frame(
#'   x = seq(1:1000),
#'   y = rnorm(1000))
#'
#' df |>
#'   group_every(n = 30) |> # group every 30 observations together
#'   dplyr::summarise(mean_x = mean(x),
#'                    mean_y = mean(y))
group_every <- function(data, seconds = NULL, n = NULL){
  if (!is.null(n) & !is.null(seconds)){
    cli::cli_abort("Both `n` and `seconds` are provided; please only specify one or the other.")
  }
  if (!is.null(seconds) & !"time" %in% names(data)){
    cli::cli_abort("To group by seconds, there needs to be a \"time\" column in the data.")
  }

  if (!is.null(n)){
    data_grouped <- data |>
      dplyr::mutate(bin = ceiling(dplyr::row_number() / n)) |>
      dplyr::group_by(.data$bin)
  }

  if (!is.null(seconds)){
    data_grouped <- data |>
      dplyr::mutate(bin = .data$time %/% seconds) |>
      dplyr::group_by(.data$bin)
  }
  return(data_grouped)
}

