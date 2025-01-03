#' Map from Cartesian to polar coordinates
#'
#' @param data movement data frame with Cartesian coordinates
#'
#' @return movement data frame with polar coordinates
#' @export
map_to_polar <- function(data){
  ensure_coord_cartesian(data)
  data <- data |>
    mutate(
      rho = cartesian_to_rho(.data$x, .data$y),
      theta = cartesian_to_theta(.data$x, .data$y)) |>
    dplyr::select(-c("x", "y")) |>
    dplyr::relocate("rho", .after = "keypoint") |>
    dplyr::relocate("theta", .after = "rho")
  return(data)
}

cartesian_to_rho <- function(x,y){
  sqrt(x^2 + y^2)
}

cartesian_to_theta <- function(x,y,centered=FALSE){
  angle <- dplyr::case_when(x > 0 & y > 0 ~ atan(x/y),
                            x < 0 & y > 0 ~ pi - atan(-x/y),
                            x < 0 & y < 0 ~ atan(-x/-y) + pi,
                            x > 0 & y < 0 ~ 2*pi - atan(x/-y))

  # Keep between pi and -pi
  angle <- constrain_angles_radians(angle)

  if (centered == TRUE){
    angle <- dplyr::case_when(angle > pi ~ angle - 2*pi,
                              angle < -pi ~ angle + 2*pi,
                              .default = angle)
  }

  return(angle)
}
