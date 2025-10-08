

# Function to expand RLE to full boundary representation
expand_rle_to_boundary <- function(data) {
  rle_data |>
    # Create complete grid of all time/row combinations
    tidyr::complete(
      time = min(time):max(time),
      row = min(row):max(row)
    ) |>
    dplyr::group_by(time, row) |>
    dplyr::summarise(
      x_min = min(start_idx),
      x_max = max(start_idx + length - 1),
      .groups = 'drop'
    )
}


# Convert smoothed boundary back to RLE
boundary_to_rle <- function(data) {
  data |>
    # Filter out rows where we don't have data
    dplyr::filter(!is.na(x_min_smooth), !is.na(x_max_smooth)) |>
    # Create runs
    dplyr::group_by(time, row) |>
    dplyr::summarise(
      start_idx = floor(x_min_smooth),
      length = ceiling(x_max_smooth) - floor(x_min_smooth) + 1,
      value = 1,
      .groups = 'drop'
    )
}
