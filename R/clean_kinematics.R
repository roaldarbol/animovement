#' Clean kinematics
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' @param data tidy movement data frame with kinematics
#'
#' @return a clean kinematics data frame
#' @export
clean_kinematics <- function(data){
  # Place validators here

  # Proceed to make the cleaning
  data <- data |>
    dplyr::filter(.data$direction != atan2(1,0) & .data$direction != atan2(0,1)) |>
    dplyr::filter(.data$v_translation > 0)
  return(data)
}
