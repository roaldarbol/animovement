#' Calculate straightness measures
#'
#' @param data Data frame
#' @param straightness hich method to calculate path straightness. Choose between "A" (default), "B", "C"... or a combination (e.g. "c("A","B")"). See description for details about the different calculations.
#'
#' @keywords internal
#' @export
calculate_straightness <- function(data, straightness = c("A", "B", "C", "D")) {
  # Make validator to ensure that x,y,total_translation and total_rotation is present

  data <- data |>
    dplyr::mutate(
      straightness_A = calculate_straightness_A(
        .data$last_x,
        .data$last_y,
        .data$total_distance
      ),
      straightness_B = calculate_straightness_B(
        .data$last_x,
        .data$last_y,
        .data$total_distance
      ),
      straightness_C = calculate_straightness_C(
        .data$total_distance,
        .data$total_rotation
      ),
      straightness_D = calculate_straightness_D(
        .data$total_distance,
        .data$total_rotation
      )
    )

  # Only select the methods chosen by the user
  possible_straightness <- c("A", "B", "C", "D")
  possible_straightness_columns <- paste("straightness", possible_straightness, sep = "_")
  straightness_columns <- paste("straightness", straightness, sep = "_")
  columns_not_selected <- possible_straightness_columns[which(!possible_straightness_columns %in% straightness_columns)]
  data <- data |>
    dplyr::select(!all_of(columns_not_selected))

  return(data)
}

#' @keywords internal
calculate_straightness_A <- function(x, y, distance) {
  calculate_distance(x, y) / distance
}

#' @keywords internal
calculate_straightness_B <- function(x, y, distance) {
  distance / calculate_distance(x, y)
}

#' @keywords internal
calculate_straightness_C <- function(distance, rotation) {
  distance / as.numeric(rotation)
}

#' @keywords internal
calculate_straightness_D <- function(distance, rotation) {
  as.numeric(rotation) / distance
}
