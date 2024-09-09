# Let's try to implement some general checks of time columns/computations
#' Title
#'
#' @export
validate_time <- function(){
  ## Check time assignment
  if (is.null(col_time) & is.null(sampling_rate)){
    ## Should be updated. Not quite sure how that works right now.
    cli::cli_abort("No way of assigning time. Please provide either `col_time` or `sampling_rate`")
  }
}
