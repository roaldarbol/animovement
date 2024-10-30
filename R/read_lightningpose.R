#' Read LightningPose data
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Read csv files from LightningPose (LP).
#'
#' @param path Path to a LightningPose data file
#'
#' @return a movement dataframe
#' @export
read_lightningpose <- function(path) {
  read_deeplabcut(path, multianimal = FALSE)
}
