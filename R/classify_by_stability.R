#' Classify Movement States Based on Stability Analysis
#'
#' @description
#' This function analyzes movement tracking data to identify periods of high and low
#' activity by detecting stable periods in the movement data. It returns a binary
#' classification where 1 indicates high activity and 0 indicates low activity.
#'
#' @details
#' The classification process follows these key steps:
#'
#' 1. Stability Detection:
#'    - Identifies stable periods in the movement data
#'    - Uses the longest stable period to establish a baseline for low activity
#'
#' 2. State Classification:
#'    - Sets an activity threshold based on the baseline period
#'    - Classifies periods that deviate from baseline stability as high activity
#'
#' 3. Optional Refinement:
#'    - If refine_transitions = TRUE, examines transitions between states to
#'      find precise start/end points using stability detection
#'    - Short duration states can be filtered based on min_low_state_duration
#'      and min_high_state_duration parameters using a majority context approach
#'
#' @param speed Numeric vector of speed or velocity measurements. If velocity is provided,
#'        absolute values will be used automatically
#' @param window_size Number of measurements to consider when calculating variance (default: 30)
#' @param min_stable_period Minimum length required for a stable period (default: 30)
#' @param tolerance Tolerance for variance in stable periods (default: 0.1, must be between 0 and 1)
#' @param refine_transitions Whether to refine state transitions using stability detection (default: TRUE)
#' @param min_low_state_duration Minimum duration for low activity states; shorter periods are merged
#'        using majority context (default: 0, no merging)
#' @param min_high_state_duration Minimum duration for high activity states; shorter periods are merged
#'        using majority context (default: 0, no merging)
#' @param search_window How far to look for movement transitions when refining (default: 90)
#' @param stability_window Window size for checking if movement has stabilized (default: 10)
#' @param stability_threshold Maximum variance allowed in stable state (default: 0.5)
#' @param return_type Should the function return "factor" ("high"/"low") or "numeric" (1/0) (default: "numeric")
#'
#' @return
#' Numeric vector of the same length as input:
#'   - 1: High activity state
#'   - 0: Low activity state
#'   - NA: Unable to classify (usually due to missing data)
#' @export
classify_by_stability <- function(speed,
                                  window_size = 30,
                                  min_stable_period = 30,
                                  tolerance = 0.1,
                                  refine_transitions = TRUE,
                                  min_low_state_duration = 0,
                                  min_high_state_duration = 0,
                                  search_window = 90,
                                  stability_window = 10,
                                  stability_threshold = 0.5,
                                  return_type = c("numeric", "factor")) {

  # Input validation for tolerance
  if(tolerance <= 0 || tolerance > 1) {
    stop("tolerance must be between 0 and 1")
  }

  return_type <- match.arg(return_type)

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

  # Find baseline statistics using stable periods
  var_threshold <- quantile(roll_var, 0.75, na.rm = TRUE)
  stable_periods <- !is.na(roll_var) & roll_var < var_threshold

  rle_obj <- rle(stable_periods)
  stable_lengths <- rle_obj$lengths[rle_obj$values]

  if(length(stable_lengths) == 0) {
    return(rep(NA_real_, length(speed)))
  }

  # Check for valid stable periods
  has_valid_stable_period <- any(stable_lengths >= min_stable_period)
  if(!has_valid_stable_period) {
    min_stable_period <- min(window_size, min(stable_lengths))
  }

  # Get baseline period
  cum_lengths <- cumsum(rle_obj$lengths)
  stable_starts <- cum_lengths[which(rle_obj$values)] -
    rle_obj$lengths[which(rle_obj$values)] + 1

  long_stable_periods <- which(stable_lengths >= min_stable_period)
  longest_stable <- which.max(stable_lengths[long_stable_periods])
  baseline_start <- stable_starts[long_stable_periods[longest_stable]]
  baseline_end <- baseline_start + stable_lengths[long_stable_periods[longest_stable]] - 1

  # Calculate baseline statistics
  baseline_mean <- mean(speed[baseline_start:baseline_end], na.rm = TRUE)
  baseline_sd <- sd(speed[baseline_start:baseline_end], na.rm = TRUE)

  # Convert tolerance to threshold using inverse normal CDF
  threshold_multiplier <- qnorm(1 - tolerance)
  threshold <- baseline_mean + threshold_multiplier * baseline_sd

  # Initial classification
  state <- rep("low", length(speed))
  state[is.na(speed)] <- NA_character_
  state[!is.na(speed) & speed > threshold] <- "high"

  # Get activity bout information
  state_for_rle <- state
  state_for_rle[is.na(state_for_rle)] <- "NA"
  rle_activity <- rle(state_for_rle)

  # Create clean pairs of state changes
  pairs <- list()
  current_pos <- 1

  for(i in seq_along(rle_activity$lengths)) {
    if(!is.na(rle_activity$values[i]) && rle_activity$values[i] == "high") {
      start_pos <- current_pos
      end_pos <- current_pos + rle_activity$lengths[i] - 1
      pairs[[length(pairs) + 1]] <- c(start_pos, end_pos)
    }
    current_pos <- current_pos + rle_activity$lengths[i]
  }

  # Optional transition refinement
  if(refine_transitions && length(pairs) > 0) {
    for(i in seq_along(pairs)) {
      bout_start <- pairs[[i]][1]
      bout_end <- pairs[[i]][2]

      # Get local baseline statistics from clear stable periods
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
        is_stable <- function(window) {
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
          if(is_stable(window)) {
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
            state[new_start:bout_start][valid_indices] <- "high"
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
            state[bout_end:new_end][valid_indices] <- "high"
          }
        }
      }
    }
  }

  # Process minimum duration constraints
  if(min_low_state_duration > 0 || min_high_state_duration > 0) {
    state_for_rle <- state
    state_for_rle[is.na(state_for_rle)] <- "NA"
    rle_obj <- rle(state_for_rle)

    pos <- 1
    for(i in seq_along(rle_obj$lengths)) {
      end_pos <- pos + rle_obj$lengths[i] - 1

      if(!is.na(rle_obj$values[i]) && rle_obj$values[i] != "NA") {
        current_state <- rle_obj$values[i]
        min_duration <- if(current_state == "low") {
          min_low_state_duration
        } else {
          min_high_state_duration
        }

        if(rle_obj$lengths[i] < min_duration) {
          # Get surrounding states
          before_state <- if(pos > 1) state[pos-1] else state[1]
          after_state <- if(end_pos < length(state)) state[end_pos+1] else state[length(state)]

          # Handle NA values in surrounding states
          if(!is.na(before_state) && !is.na(after_state) && before_state == after_state) {
            state[pos:end_pos] <- before_state
          }
        }
      }
      pos <- end_pos + 1
    }
  }

  # Convert character states to numeric
  if (return_type == "numeric"){
    result <- dplyr::case_when(is.na(result) ~ NA,
                        result == "high" ~ 1,
                        result == "low", 0)
  }

  return(result)
}
