#' Title
#'
#' @param x A vector of numbers
#'
#' @return
#' @export
#'
#' @examples
mode <- function(x){
  which.max(tabulate(x))
}
