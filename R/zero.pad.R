#' @export zero.pad
#' 
#' @title Zero Pad Integers
#' 
#' @param x a vector of integers.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' x <- c(0, 1, 3, 4, 10) 
#' zero.pad(x)
#' x <- c(x, 11, 12, 100, 1000)
#' zero.pad(x)

zero.pad <- function(x) {
  is.whole <- abs(x - round(x)) < .Machine$double.eps ^ 0.5
  if(!all(is.whole)) stop("'x' must be a vector of integers")
  formatC(x, digits = floor(log10(max(x))), flag = "0")
}