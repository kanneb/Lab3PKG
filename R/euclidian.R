#' Euclidian algorithm
#'
#' Function that computes GCD value between two numeric values
#'
#' @param a numeric value
#' @param b numeric value
#'
#' @return A GCD value thats numeric
#'
#' @references https://en.wikipedia.org/wiki/Euclidean
#'
#' @export
#'
#' @examples
#' euclidean(123612, 13892347912)
#' euclidean(101, 10612)

euclidean <- function(a,b) {
  stopifnot(is.numeric(a), is.numeric(b))

  while (b != 0) {
     rem <- a %% b
     a <- b
     b <- rem
  }

  return(a)
}
