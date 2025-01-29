get_image <- function(path, frame, silent = TRUE) {
  temp_dir <- tempdir()
  output_path <- file.path(temp_dir, "frame_%06d.jpg")

  # Extract frame
  filt <- paste0("trim=start_frame=", frame, ":end_frame=", frame + 1)

  if (!silent) cli::cli_process_start("Extracting frame from video")

  # Suppress the error since it's actually succeeding
  suppressMessages(
    tryCatch({
      av::av_encode_video(
        input = path,
        output = output_path,
        codec = 'mjpeg',
        vfilter = paste(filt, ",format=yuv420p"),
        verbose = !silent
      )
      if (!silent) cli::cli_process_done()
    }, error = function(e) {
      # Only show error if file wasn't created
      result_path <- list.files(
        path = temp_dir,
        pattern = "frame_.*\\.jpg$",
        full.names = TRUE
      )[1]

      if (is.na(result_path)) {
        if (!silent) {
          cli::cli_process_failed()
          cli::cli_alert_danger("Failed to extract frame: {e$message}")
        }
        return(NULL)
      }
    })
  )

  result_path <- list.files(
    path = temp_dir,
    pattern = "frame_.*\\.jpg$",
    full.names = TRUE
  )[1]

  return(result_path)
}

# library(devtools)
# path <- "/Users/roaldarbol/Downloads/sam2_masked_video_1730977000046.mp4"
get_image("/Users/roaldarbol/Downloads/sam2_masked_video_1730977000046.mp4", 20)
