#' Join Data Frames by Closest Time Match, with Optional Grouping
#'
#' @description
#' Joins two data frames by matching the closest timestamp in the right-hand data frame
#' to each timestamp in the left-hand data frame, optionally within groups. Can match to
#' either the closest preceding or following timestamp.
#'
#' @param x Left-hand data frame
#' @param y Right-hand data frame
#' @param x_time_col Name of the time column in x
#' @param y_time_col Name of the time column in y
#' @param direction Either '>=' (default) to match to closest preceding or equal value,
#'   or '<=' to match to closest following or equal value
#' @param by Character vector of column names to group by. These columns must exist
#'   in both data frames and will be joined exactly.
#'
#' @return A data frame containing all columns from x and all columns from y except y_time_col.
#'   Rows in x with no matching timestamp in y will have NA values for y columns.
#'
#' @examples
#' x <- data.frame(
#'   group = c(1,1,1, 2,2,2),
#'   time = c(0,1,2, 0,1,2),
#'   value1 = 1:6
#' )
#' y <- data.frame(
#'   group = c(1,1, 2,2),
#'   timestamp = c(0.5,1.5, 0.5,1.5),
#'   value2 = letters[1:4]
#' )
#'
#' # Match to closest preceding value within groups
#' join_closest(x, y, "time", "timestamp", by = "group", direction = ">=")
join_closest <- function(x, y, x_time_col, y_time_col, direction = ">=", by = NULL) {
  # Input validation
  if (!is.data.frame(x)) {
    cli::cli_abort("'x' must be a data frame")
  }
  if (!is.data.frame(y)) {
    cli::cli_abort("'y' must be a data frame")
  }

  if (!x_time_col %in% names(x)) {
    cli::cli_abort("Column {.val {x_time_col}} not found in 'x' data frame")
  }
  if (!y_time_col %in% names(y)) {
    cli::cli_abort("Column {.val {y_time_col}} not found in 'y' data frame")
  }

  if (!is.numeric(x[[x_time_col]])) {
    cli::cli_abort("Column {.val {x_time_col}} in 'x' must be numeric")
  }
  if (!is.numeric(y[[y_time_col]])) {
    cli::cli_abort("Column {.val {y_time_col}} in 'y' must be numeric")
  }

  if (!direction %in% c("<=", ">=")) {
    cli::cli_abort("'direction' must be either '<=' or '>='")
  }

  # Validate grouping columns if provided
  if (!is.null(by)) {
    if (!all(by %in% names(x))) {
      missing <- setdiff(by, names(x))
      cli::cli_abort("Grouping columns {.val {missing}} not found in 'x'")
    }
    if (!all(by %in% names(y))) {
      missing <- setdiff(by, names(y))
      cli::cli_abort("Grouping columns {.val {missing}} not found in 'y'")
    }
  }

  # Check for empty data frames
  if (nrow(x) == 0) {
    cli::cli_warn("'x' has 0 rows")
    return(x)
  }
  if (nrow(y) == 0) {
    cli::cli_warn("'y' has 0 rows")
    return(x)
  }

  # If no grouping, use original function logic
  if (is.null(by)) {
    if (is.unsorted(y[[y_time_col]])) {
      cli::cli_warn("Timestamps in 'y' are not sorted. Sorting automatically.")
      y <- y[order(y[[y_time_col]]), , drop = FALSE]
    }

    if (anyDuplicated(y[[y_time_col]])) {
      cli::cli_warn("Duplicate timestamps found in 'y'. Using first occurrence.")
    }

    return(closest_join_ungrouped(x, y, x_time_col, y_time_col, direction))
  }

  # Split data into groups and apply join within each group
  result <- x
  y_cols <- setdiff(names(y), c(y_time_col, by))
  result[y_cols] <- NA

  # Create group identifiers
  x_groups <- do.call(paste, c(x[by], sep = "\001"))
  y_groups <- do.call(paste, c(y[by], sep = "\001"))

  # For each unique group in x
  for (group in unique(x_groups)) {
    x_idx <- which(x_groups == group)
    y_idx <- which(y_groups == group)

    if (length(y_idx) == 0) next

    # Get subset of data for this group
    x_subset <- x[x_idx, , drop = FALSE]
    y_subset <- y[y_idx, , drop = FALSE]

    # Sort y timestamps within group
    y_subset <- y_subset[order(y_subset[[y_time_col]]), , drop = FALSE]

    # Perform join for this group
    matched <- closest_join_ungrouped(x_subset, y_subset, x_time_col, y_time_col, direction)

    # Update result
    result[x_idx, y_cols] <- matched[y_cols]
  }

  return(result)
}

# Internal function for ungrouped joining
closest_join_ungrouped <- function(x, y, x_time_col, y_time_col, direction) {
  x_times <- x[[x_time_col]]
  y_times <- y[[y_time_col]]

  if (direction == ">=") {
    matches <- findInterval(x_times, y_times)
    matches[matches == 0] <- NA
    matches[matches == length(y_times) & x_times > max(y_times)] <- NA
  } else {
    matches <- findInterval(x_times, y_times, rightmost.closed = TRUE)
    matches[matches < length(y_times)] <- matches[matches < length(y_times)] + 1
    matches[x_times <= min(y_times)] <- 1
  }

  # Create result data frame with NAs where needed
  y_subset <- y[, setdiff(names(y), y_time_col), drop = FALSE]
  result <- x

  # Add each column from y separately to handle NAs properly
  for(col in names(y_subset)) {
    result[[col]] <- y_subset[[col]][matches]
  }

  return(result)
}
