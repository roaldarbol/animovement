set_origin <- function(data, x=NULL, y=NULL, z=NULL, rho=NULL, theta=NULL){
  if (!is.null(x) & !is.null(y)){
    data <- set_origin_cartesian(data, x, y, z)
  } else if (!is.null(rho) & !is.null(theta)){
    data <- set_origin_polar(data, rho, theta)
  }

  return(data)
}

set_origin_cartesian <- function(data, x, y, z=NULL){
  ensure_coord_cartesian(data)
  data <- data |>
    dplyr::mutate(x = .data$x - x,
                  y = .data$y - y)
  # For 3D
  if (!is.null(z)){
    data <- data |>
      dplyr::mutate(z = .data$z - z)
  }

  return(data)
}

# Currently not working
set_origin_polar <- function(data, rho, theta){
  ensure_coord_polar(data)

  data <- data |>
    dplyr::mutate(rho = sqrt((.data$rho - rho)^2 + (.data$theta - theta)^2),
                  theta = atan2((.data$theta - theta), (.data$rho - rho)) + theta)
# Let (r, θ) be the original polar coordinates of a point, and (ρ, α) be the new origin.
# The transformed polar coordinates (R, Θ) can be calculated as:
# R = √((r - ρ)^2 + (θ - α)^2)
# Θ = atan2((θ - α), (r - ρ)) + α
  return(data)
}

convert_cartesian_to_polar <- function(data){
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

convert_polar_to_cartesian <- function(data){
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

cartesian_to_rho <- function(x,y){
  sqrt(x^2 + y^2)
}

cartesian_to_theta <- function(x,y,centered=FALSE){
  angle <- dplyr::case_when(x > 0 & y > 0 ~ atan(x/y),
                     x < 0 & y > 0 ~ pi - atan(-x/y),
                     x < 0 & y < 0 ~ atan(-x/-y) + pi,
                     x > 0 & y < 0 ~ 2*pi - atan(x/-y))

  # Keep between pi and -pi
  if (centered == TRUE){
    angle <- dplyr::case_when(angle > pi ~ angle - 2*pi,
                              angle < -pi ~ angle + 2*pi,
                              .default = angle)
  }

  return(angle)
}

polar_to_x <- function(rho, theta){
  rho * cos(theta)
}

polar_to_y <- function(rho, theta){
  rho * sin(theta)
}

rad_to_deg <- function(x){
  (x * 180)/pi
}

deg_to_rad <- function(x){
  (x * pi)/180
}
