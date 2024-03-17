#' Mode
#'
#' @param x A vector of numbers
#'
#' @return
#' @export
#'
#' @examples
mode <- function(x){
  y <- tabulate(x)
  which(y == max(y, na.rm = TRUE))
}
