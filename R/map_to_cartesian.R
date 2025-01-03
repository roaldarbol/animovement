#' Map from polar to Cartesian coordinates
#'
#' @param data movement data frame with polar coordinates
#'
#' @return movement data frame with Cartesian coordinates
#' @export
map_to_cartesian <- function(data){
  ensure_coord_polar(data)
  data <- data |>
    dplyr::mutate(
      x = polar_to_x(.data$rho, .data$theta),
      y = polar_to_y(.data$rho, .data$theta)) |>
    dplyr::select(-c("rho", "theta")) |>
    dplyr::relocate("x", .after = "keypoint") |>
    dplyr::relocate("y", .after = "x")
  return(data)
}

polar_to_x <- function(rho, theta){
  rho * cos(theta)
}

polar_to_y <- function(rho, theta){
  rho * sin(theta)
}
