filter_by_pose <- function(data){

}

calculate_distance_to_centroid <- function(data, centroid_name = "centroid"){
  data_egocentric_distance <- data.frame()
  individuals <- unique(data$individual)
  n_individuals <- length(individuals)
  if (n_individuals == 1 && is.na(individuals)){
    id_was_na <- TRUE
    data <- data |>
      dplyr::mutate(individual = "temporary_id")
    individuals <- unique(data$individual)
    n_individuals <- length(individuals)
  }

  for (i in 1:length(individuals)){
    temp_data <- data |>
      dplyr::filter(.data$individual == individuals[i])
    if (!centroid_name %in% unique(temp_data$keypoint)){
      temp_data <- temp_data |>
        add_centroid(centroid_name = centroid_name)
    }
    temp_data <- temp_data |>
      translate_coords(to_keypoint = centroid_name) |>
      dplyr::mutate(dist_to_centroid = calculate_distance(.data$x, .data$y))
    data_egocentric_distance <- bind_rows(data_egocentric_distance, temp_data)
  }
  return(data_egocentric_distance)
}
