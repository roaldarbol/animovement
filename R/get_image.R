#' Extract frames from a video file
#'
#' @description
#' Extracts one or more frames from a video file and saves them as JPEG images in a temporary directory.
#' Can handle both consecutive and non-consecutive frame sequences.
#'
#' @param path Character string. Path to the video file.
#' @param frames Numeric vector. Frame numbers to extract. Can be a single number, a sequence (e.g., 20:30),
#'   or non-consecutive frames (e.g., c(20, 30, 40)).
#' @param silent Logical. If TRUE (default), suppresses progress messages and warnings.
#'
#' @return Character vector of paths to the extracted frame files. Files are named as 'frame_X.jpg'
#'   where X is the frame number. Returns empty character vector if extraction fails.
#'
#' @details
#' The function uses the av package to interface with FFmpeg for frame extraction. For consecutive
#' frames it uses FFmpeg's trim filter, while for non-consecutive frames it uses the select filter.
#' Files are temporarily saved with sequential names and then renamed to match the requested frame
#' numbers.
#'
#' @note
#' - Files are saved in the system's temporary directory
#' - Previous files with matching patterns are removed before extraction
#' - Frame numbering in FFmpeg is 0-based, but the function handles the conversion internally
#'
#' @examples
#' \dontrun{
#' # Extract a single frame
#' path <- get_image("video.mp4", 20)
#'
#' # Extract consecutive frames
#' paths <- get_image("video.mp4", 20:30)
#'
#' # Extract specific frames
#' paths <- get_image("video.mp4", c(20, 30, 40))
#'
#' # With progress messages
#' paths <- get_image("video.mp4", 20:30, silent = FALSE)
#' }
#'
#' @export
get_image <- function(path, frames, silent = TRUE) {
  # Input validation
  if (!is.numeric(frames)) {
    stop("'frames' must be numeric")
  }

  if (is.null(frames) || length(frames) == 0) {
    stop("'frames' cannot be NULL or empty")
  }

  if (any(frames < 0)) {
    stop("'frames' must be positive numbers")
  }

  if (!is.character(path) || length(path) != 1) {
    stop("'path' must be a single character string")
  }
  temp_dir <- tempdir()

  # Clean up ANY existing jpg files before starting
  existing_frames <- list.files(
    path = temp_dir,
    pattern = "\\.jpg$",
    full.names = TRUE
  )
  if (length(existing_frames) > 0) {
    file.remove(existing_frames)
  }

  # Early return if file doesn't exist
  if (!file.exists(path)) {
    if (!silent) {
      cli::cli_alert_danger("Video file not found: {path}")
    }
    return(character(0))
  }

  # Handle frame sequences
  if (length(frames) > 1) {
    # Check if frames are consecutive
    is_consecutive <- all(diff(frames) == 1)

    if (is_consecutive) {
      # Use trim filter for consecutive frames
      filt <- paste0("trim=start_frame=", min(frames), ":end_frame=", max(frames) + 1)
    } else {
      # Use select filter for non-consecutive frames
      # Convert to 0-based indexing for ffmpeg and create select conditions
      frame_conditions <- paste(sprintf("eq(n\\,%d)", frames - 1), collapse = "+")
      filt <- paste0("select='", frame_conditions, "',setpts=N/FRAME_RATE/TB")
    }
  } else {
    # Single frame case
    filt <- paste0("trim=start_frame=", frames, ":end_frame=", frames + 1)
  }

  # First extract with temporary names
  temp_output_path <- file.path(temp_dir, "temp_%06d.jpg")

  if (!silent) cli::cli_process_start("Extracting {length(frames)} frame{?s} from video")

  # Suppress the error since it's actually succeeding
  suppressMessages(
    tryCatch({
      av::av_encode_video(
        input = path,
        output = temp_output_path,
        codec = 'mjpeg',
        vfilter = paste(filt, ",format=yuv420p"),
        verbose = !silent
      )
      if (!silent) cli::cli_process_done()
    }, error = function(e) {
      # Only show error if files weren't created
      temp_paths <- list.files(
        path = temp_dir,
        pattern = "temp_.*\\.jpg$",
        full.names = TRUE
      )

      if (length(temp_paths) == 0) {
        if (!silent) {
          cli::cli_process_failed()
          cli::cli_alert_danger("Failed to extract frames: {e$message}")
        }
        return(character(0))
      }
    })
  )

  # Get temporary files in order
  temp_paths <- sort(list.files(
    path = temp_dir,
    pattern = "temp_.*\\.jpg$",
    full.names = TRUE
  ))

  # Rename files to match frame numbers (without padding)
  result_paths <- character(length(frames))
  for (i in seq_along(frames)) {
    new_name <- file.path(temp_dir, sprintf("frame_%d.jpg", frames[i]))
    file.rename(temp_paths[i], new_name)
    result_paths[i] <- new_name
  }

  # Verify we got the expected number of frames
  if (length(result_paths) != length(frames)) {
    if (!silent) {
      cli::cli_alert_warning("Expected {length(frames)} frames but got {length(result_paths)}")
    }
  }

  return(result_paths)
}

# library(devtools)
# path <- "/Users/roaldarbol/Downloads/sam2_masked_video_1730977000046.mp4"
# get_image("/Users/roaldarbol/Downloads/sam2_masked_video_1730977000046.mp4", c(20, 40, 120))
