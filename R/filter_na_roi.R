#' Filter coordinates outside a region of interest (ROI)
#'
#' @description
#' Filters out coordinates that fall outside a specified region of interest by
#' setting them to NA. The ROI can be either rectangular (defined by min/max
#' coordinates) or circular (defined by center and radius).
#'
#' @param data A data frame containing 'x' and 'y' coordinates
#' @param x_min Minimum x-coordinate for rectangular ROI
#' @param x_max Maximum x-coordinate for rectangular ROI
#' @param y_min Minimum y-coordinate for rectangular ROI
#' @param y_max Maximum y-coordinate for rectangular ROI
#' @param x_center x-coordinate of circle center for circular ROI
#' @param y_center y-coordinate of circle center for circular ROI
#' @param radius Radius of circular ROI
#'
#' @return A data frame with coordinates outside ROI set to NA
#' @export
#'
#' @importFrom cli cli_abort
#'
#' @examples
#' # Create sample data
#' sample_data <- expand.grid(
#'   x = seq(0, 100, by = 10),
#'   y = seq(0, 100, by = 10)
#' ) |> as.data.frame()
#'
#' # Rectangular ROI example
#' sample_data |>
#'   filter_na_roi(x_min = 20, x_max = 80, y_min = 20, y_max = 80)
#'
#' # Circular ROI example
#' sample_data |>
#'   filter_na_roi(x_center = 50, y_center = 50, radius = 25)
filter_na_roi <- function(data,
                          x_min = NULL,
                          x_max = NULL,
                          y_min = NULL,
                          y_max = NULL,
                          x_center = NULL,
                          y_center = NULL,
                          radius = NULL) {

  if (all(is.null(x_center), is.null(y_center), is.null(radius))) {
    if (all(is.null(x_min), is.null(x_max), is.null(y_min), is.null(y_max))) {
      cli::cli_abort(c(
        "No ROI parameters provided.",
        "i" = "For rectangular ROI: Provide at least one of {.var x_min}, {.var x_max}, {.var y_min}, {.var y_max}",
        "i" = "For circular ROI: Provide {.var x_center}, {.var y_center}, and {.var radius}"
      ))
    }
    data <- filter_na_roi_square(data, x_min, x_max, y_min, y_max)
  } else {
    if (any(is.null(x_center), is.null(y_center), is.null(radius))) {
      cli::cli_abort(c(
        "Incomplete circular ROI parameters.",
        "x" = "All of {.var x_center}, {.var y_center}, and {.var radius} must be provided."
      ))
    }
    data <- filter_na_roi_circle(data, x_center, y_center, radius)
  }
  return(data)
}

#' Filter coordinates outside a rectangular ROI
#'
#' @description
#' Helper function for filter_na_roi() that handles rectangular ROIs.
#' Sets coordinates to NA if they fall outside the specified bounds.
#'
#' @param data A data frame containing 'x' and 'y' coordinates
#' @param x_min,x_max,y_min,y_max Bounds of the rectangular ROI
#'
#' @return A data frame with coordinates outside rectangular ROI set to NA
#' @keywords internal
#'
#' @importFrom dplyr mutate if_else
#' @importFrom rlang .data
filter_na_roi_square <- function(data, x_min, x_max, y_min, y_max) {
  if (!is.null(x_min)) {
    data <- data |>
      dplyr::mutate(
        x = dplyr::if_else(.data$x < x_min, NA_real_, .data$x),
        y = dplyr::if_else(.data$x < x_min, NA_real_, .data$y)
      )
  }

  if (!is.null(x_max)) {
    data <- data |>
      dplyr::mutate(
        x = dplyr::if_else(.data$x > x_max, NA_real_, .data$x),
        y = dplyr::if_else(.data$x > x_max, NA_real_, .data$y)
      )
  }

  if (!is.null(y_min)) {
    data <- data |>
      dplyr::mutate(
        x = dplyr::if_else(.data$y < y_min, NA_real_, .data$x),
        y = dplyr::if_else(.data$y < y_min, NA_real_, .data$y)
      )
  }

  if (!is.null(y_max)) {
    data <- data |>
      dplyr::mutate(
        x = dplyr::if_else(.data$y > y_max, NA_real_, .data$x),
        y = dplyr::if_else(.data$y > y_max, NA_real_, .data$y)
      )
  }
  return(data)
}

#' Filter coordinates outside a circular ROI
#'
#' @description
#' Helper function for filter_na_roi() that handles circular ROIs.
#' Sets coordinates to NA if they fall outside the specified circle.
#'
#' @param data A data frame containing 'x' and 'y' coordinates
#' @param x_center,y_center Center coordinates of the circular ROI
#' @param radius Radius of the circular ROI
#'
#' @return A data frame with coordinates outside circular ROI set to NA
#' @keywords internal
#'
#' @importFrom dplyr mutate if_else select
#' @importFrom rlang .data
filter_na_roi_circle <- function(data, x_center, y_center, radius) {
  data |>
    dplyr::mutate(
      is_outside = ((.data$x - x_center)^2 + (.data$y - y_center)^2) > radius^2,
      x = dplyr::if_else(.data$is_outside, NA_real_, .data$x),
      y = dplyr::if_else(.data$is_outside, NA_real_, .data$y)
    ) |>
    dplyr::select(-"is_outside")
}
