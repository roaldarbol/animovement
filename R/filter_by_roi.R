filter_by_roi <- function(data,
                          x_min=NULL,
                          x_max=NULL,
                          y_min=NULL,
                          y_max=NULL,
                          x_center=NULL,
                          y_center=NULL,
                          radius=NULL){
  if (all(is.null(x_center), is.null(y_center), is.null(radius))){
    if (all(is.null(x_min), is.null(x_max), is.null(y_min), is.null(y_max))){
      cli::cli_abort("To use a square ROI, at least one of the following must be provided: x_min, x_max, y_min & y_max.")
    }
    data <- filter_by_roi_square(data, x_min, x_max, y_min, y_max)
  } else {
    if (any(is.null(x_center), is.null(y_center), is.null(radius))){
      cli::cli_abort("To use a square ROI, all following must be provided: x_min, x_max, y_min & y_max.")
    }
    data <- filter_by_roi_circle(data, x_center, y_center, radius)
  }
  return(data)
}

filter_by_roi_square <- function(data, x_min, x_max, y_min, y_max){
  if (!is.null(x_min)){
    data <- data |>
      dplyr::mutate(x = case_when(.data$x < x_min ~ NA,
                                  .default = .data$x),
                    y = case_when(.data$x < x_min ~ NA,
                                  .default = .data$y))
  }

  if (!is.null(x_max)){
    data <- data |>
      dplyr::mutate(x = case_when(.data$x > x_max ~ NA,
                                  .default = .data$x),
                    y = case_when(.data$x > x_max ~ NA,
                                  .default = .data$y))
  }

  if (!is.null(y_min)){
    data <- data |>
      dplyr::mutate(x = case_when(.data$y < y_min ~ NA,
                                  .default = .data$x),
                    y = case_when(.data$y < y_min ~ NA,
                                  .default = .data$y))
  }

  if (!is.null(y_max)){
    data <- data |>
      dplyr::mutate(x = case_when(.data$y > y_max ~ NA,
                                  .default = .data$x),
                    y = case_when(.data$y > y_max ~ NA,
                                  .default = .data$y))
  }
  return(data)
}

filter_by_roi_circle <- function(data, x_center, y_center, radius){
  data <- data |>
    dplyr::mutate(x = case_when(((.data$x - x_center)^2 + (.data$y - y_center)^2) > radius^2 ~ NA,
                                .default = .data$x),
                  y = case_when(((.data$x - x_center)^2 + (.data$y - y_center)^2) > radius^2 ~ NA,
                                .default = .data$y))
  return(data)
}
