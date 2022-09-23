#' Euclidian algorithm
#' 
#' find the greatest common divisor of two numbers
#' 
#' The first algoritm to implement is the Euclidian algorithm. Assert that the arguments are numeric scalars or integers.
#' 
#' @param a A number.
#' @param b A number.
#' @return the greatest common divisor of a and b.
#' @references https://en.wikipedia.org/wiki/Euclidean_algorithm
#' 
#' @export
#' @examples 
#' euclidean(24, 36)

euclidean <-
function(a, b){
  while (b != 0) {
    t <- b
    b <- a %% b
    a <- t
  }
  return(a)
}
