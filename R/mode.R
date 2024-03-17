#' @title Mode
#'
#' @description
#' In statistics, the mode is the value that appears most often in a set of data values.
#'
#'
#' @param x A vector of numbers
#'
#' @return A vector of the most frequently occuring value(s)
#' @export
#'
#' @examples
#' my_vector <- c(1,2,3,3,4,5)
#' mode(my_vector)
mode <- function(x){
  y <- tabulate(x)
  which(y == max(y, na.rm = TRUE))
}
