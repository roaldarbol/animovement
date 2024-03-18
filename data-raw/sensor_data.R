filepaths <- list.files(pattern = ".csv", recursive = TRUE)
sensor_data <- read_trackball_data(filepaths, configuration = "open", format = "csv")
usethis::use_data(sensor_data, overwrite = TRUE)
