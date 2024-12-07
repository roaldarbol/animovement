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

constrain_angles_radians <- function(x){
  (x %% (2*pi))
}
