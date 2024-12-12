filter_by_speed <- function(data, threshold = "auto"){
  d <- data |>
    calculate_kinematics()
  if (threshold == "auto"){
    cli::cli_abort("Not implemented yet.")
  } else {
  d <- d |>
    dplyr::filter(abs(.data$v_translation) < threshold)
  }
}
