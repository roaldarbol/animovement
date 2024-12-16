#' Classify Movement States Based on Stable Periods
#'
#' @description
#' This function analyzes movement tracking data to identify periods of movement,
#' using the presence of stable periods (plateaus) to distinguish between high and low
#' activity states. It returns a binary classification where 1 indicates high activity
#' and 0 indicates low activity.
#'
#' @details
#' The classification process follows these key steps:
#'
#' 1. Initial Assessment:
#'    - Identifies stable periods in the speed data (plateaus)
#'    - Uses the longest stable period as a baseline for low activity state
#'
#' 2. State Detection:
#'    - Sets a threshold based on the baseline period
#'    - Identifies periods that deviate from this baseline as high activity
#'
#' 3. Optional Refinement:
#'    - If refine_transitions = TRUE, examines transitions between states to
#'      find precise start/end points using plateau detection
#'    - Short duration states can be filtered based on min_low_state_duration
#'      and min_high_state_duration parameters using a majority context approach
#'
#' @param speed Numeric vector of speed or velocity measurements. If velocity is provided,
#'        absolute values will be used automatically
#' @param window_size Number of measurements to consider when calculating variance (default: 30)
#' @param min_plateau_length Minimum length required for a stable period (default: 30)
#' @param threshold_multiplier How many standard deviations above baseline to set threshold (default: 3)
#' @param refine_transitions Whether to refine state transitions using plateau detection (default: TRUE)
#' @param min_low_state_duration Minimum duration for low activity states; shorter periods are merged
#'        using majority context (default: 0, no merging)
#' @param min_high_state_duration Minimum duration for high activity states; shorter periods are merged
#'        using majority context (default: 0, no merging)
#' @param search_window How far to look for movement transitions when refining (default: 90)
#' @param stability_window Window size for checking if movement has stabilized (default: 10)
#' @param stability_threshold Maximum variance allowed in stable state (default: 0.5)
#'
#' @return
#' Numeric vector of the same length as input:
#'   - 1: High activity state
#'   - 0: Low activity state
#'   - NA: Unable to classify (usually due to missing data)
#' @export
classify_movement_plateau <- function(speed,
                                      window_size = 30,
                                      min_plateau_length = 30,
                                      threshold_multiplier = 3,
                                      refine_transitions = TRUE,
                                      min_low_state_duration = 0,
                                      min_high_state_duration = 0,
                                      search_window = 90,
                                      stability_window = 10,
                                      stability_threshold = 0.5) {

  # Convert to absolute values to handle both speed and velocity inputs
  speed <- abs(speed)

  # Handle all-NA case
  if(all(is.na(speed))) {
    return(rep(NA_real_, length(speed)))
  }

  # Calculate rolling variance for initial classification
  roll_var <- as.vector(roll::roll_var(
    speed,
    width = window_size,
    min_obs = ceiling(window_size/4)
  ))

  # Find baseline statistics
  var_threshold <- quantile(roll_var, 0.75, na.rm = TRUE)
  low_var_periods <- !is.na(roll_var) & roll_var < var_threshold

  rle_obj <- rle(low_var_periods)
  run_lengths <- rle_obj$lengths[rle_obj$values]

  if(length(run_lengths) == 0) {
    return(rep(NA_real_, length(speed)))
  }

  # Check for any valid plateaus, but don't return all NAs if none found
  has_valid_plateau <- any(run_lengths >= min_plateau_length)
  if(!has_valid_plateau) {
    # Set a reasonable minimum length if no valid plateau found
    min_plateau_length <- min(window_size, min(run_lengths))
  }

  # Get baseline period
  cum_lengths <- cumsum(rle_obj$lengths)
  run_starts <- cum_lengths[which(rle_obj$values)] -
    rle_obj$lengths[which(rle_obj$values)] + 1

  long_plateaus <- which(run_lengths >= min_plateau_length)
  longest_plateau <- which.max(run_lengths[long_plateaus])
  baseline_start <- run_starts[long_plateaus[longest_plateau]]
  baseline_end <- baseline_start + run_lengths[long_plateaus[longest_plateau]] - 1

  # Calculate threshold from baseline period
  baseline_mean <- mean(speed[baseline_start:baseline_end], na.rm = TRUE)
  baseline_sd <- sd(speed[baseline_start:baseline_end], na.rm = TRUE)
  threshold <- baseline_mean + threshold_multiplier * baseline_sd

  # Initial classification
  state <- character(length(speed))
  state[is.na(speed)] <- NA_character_
  state[!is.na(speed) & speed > threshold] <- "state1"
  state[!is.na(speed) & speed <= threshold] <- "state0"

  # Optional transition refinement
  if(refine_transitions) {
    # Get movement bout information
    state_for_rle <- state
    state_for_rle[is.na(state_for_rle)] <- "NA"
    rle_movement <- rle(state_for_rle)

    bout_info <- data.frame(
      start = numeric(0),
      end = numeric(0)
    )

    pos <- 1
    for(i in seq_along(rle_movement$lengths)) {
      if(!is.na(rle_movement$values[i]) && rle_movement$values[i] == "state1") {
        bout_info <- rbind(bout_info,
                           data.frame(start = pos,
                                      end = pos + rle_movement$lengths[i] - 1))
      }
      pos <- pos + rle_movement$lengths[i]
    }

    # Process each bout to extend boundaries to plateau transitions
    if(nrow(bout_info) > 0) {
      for(i in seq_len(nrow(bout_info))) {
        bout_start <- bout_info$start[i]
        bout_end <- bout_info$end[i]

        # Get local baseline statistics from clear still periods
        get_local_baseline <- function(indices) {
          vals <- speed[indices]
          # Get the lowest sustained section
          roll_mean <- roll::roll_mean(matrix(vals), width = stability_window, min_obs = stability_window/2)
          roll_var <- roll::roll_var(matrix(vals), width = stability_window, min_obs = stability_window/2)

          # Find stable regions
          stable_mask <- !is.na(roll_var) & roll_var < stability_threshold
          if(!any(stable_mask)) return(NULL)

          stable_means <- roll_mean[stable_mask]
          list(
            level = median(stable_means, na.rm = TRUE),
            spread = mad(stable_means, na.rm = TRUE)
          )
        }

        # Function to find where signal stabilizes at baseline
        find_stabilization <- function(values, baseline_stats, forward = TRUE) {
          if(is.null(baseline_stats)) return(NULL)
          if(length(values) < stability_window * 2) return(NULL)

          # Work with clean values
          valid_idx <- which(!is.na(values))
          if(length(valid_idx) < stability_window * 2) return(NULL)

          clean_values <- values[valid_idx]

          # If searching backwards, reverse everything
          if(!forward) {
            clean_values <- rev(clean_values)
            valid_idx <- rev(valid_idx)
          }

          # Function to check if window has stabilized at baseline
          is_at_baseline <- function(window) {
            if(length(window) < stability_window) return(FALSE)
            if(any(is.na(window))) return(FALSE)

            win_mean <- mean(window)
            win_var <- var(window)

            # More stringent criteria for baseline matching
            abs(win_mean - baseline_stats$level) < baseline_stats$spread * 2 &&
              win_var < stability_threshold * 0.8
          }

          # Look for first window that's stable at baseline
          n_required_stable = 3
          stable_count = 0

          for(i in seq(1, length(clean_values) - stability_window + 1)) {
            window <- clean_values[i:(i + stability_window - 1)]
            if(is_at_baseline(window)) {
              stable_count = stable_count + 1
              if(stable_count >= n_required_stable) {
                return(valid_idx[max(1, i - n_required_stable + 1)])
              }
            } else {
              stable_count = 0
            }
          }
          return(NULL)
        }

        # Look backwards
        pre_window <- max(1, bout_start - search_window):bout_start
        pre_baseline <- get_local_baseline(pre_window[1:min(length(pre_window), stability_window * 2)])
        if(!is.null(pre_baseline)) {
          pre_search <- speed[pre_window]
          change_point <- find_stabilization(pre_search, pre_baseline, forward = FALSE)

          if(!is.null(change_point)) {
            new_start <- pre_window[1] + change_point - 1
            valid_indices <- which(!is.na(speed[new_start:bout_start]))
            if(length(valid_indices) > 0) {
              state[new_start:bout_start][valid_indices] <- "state1"
            }
          }
        }

        # Look forwards
        post_window <- bout_end:min(length(speed), bout_end + search_window)
        post_baseline <- get_local_baseline(post_window[max(1, length(post_window) - stability_window * 2):length(post_window)])
        if(!is.null(post_baseline)) {
          post_search <- speed[post_window]
          change_point <- find_stabilization(post_search, post_baseline, forward = TRUE)

          if(!is.null(change_point)) {
            new_end <- post_window[1] + change_point - 1
            valid_indices <- which(!is.na(speed[bout_end:new_end]))
            if(length(valid_indices) > 0) {
              state[bout_end:new_end][valid_indices] <- "state1"
            }
          }
        }
      }
    }
  }

  # Process minimum duration constraints using majority context
  if(min_low_state_duration > 0 || min_high_state_duration > 0) {
    state_for_rle <- state
    state_for_rle[is.na(state_for_rle)] <- "NA"
    rle_obj <- rle(state_for_rle)

    # Function to determine majority state in a window
    get_majority_state <- function(start, end, current_states) {
      # Get states before and after the current run
      before_state <- if(start > 1) current_states[start-1] else current_states[1]
      after_state <- if(end < length(current_states)) current_states[end+1] else current_states[length(current_states)]

      # Count occurrences
      counts <- table(c(before_state, after_state))
      # Return the most frequent state, or the current state if tied
      if(length(counts) == 1 || counts[1] != counts[2]) {
        return(names(which.max(counts)))
      } else {
        return(current_states[start]) # Keep current state if tied
      }
    }

    # Process runs that are too short
    pos <- 1
    for(i in seq_along(rle_obj$lengths)) {
      end_pos <- pos + rle_obj$lengths[i] - 1

      if(!is.na(rle_obj$values[i])) {
        if((rle_obj$values[i] == "state0" && rle_obj$lengths[i] < min_low_state_duration) ||
           (rle_obj$values[i] == "state1" && rle_obj$lengths[i] < min_high_state_duration)) {
          # Determine new state based on majority context
          new_state <- get_majority_state(pos, end_pos, state)
          state[pos:end_pos] <- new_state
        }
      }
      pos <- end_pos + 1
    }
  }

  # Convert character states to numeric
  result <- numeric(length(state))
  result[is.na(state)] <- NA_real_
  result[state == "state1"] <- 1
  result[state == "state0"] <- 0

  return(result)
}
