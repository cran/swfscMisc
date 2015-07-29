#' @title Between
#' @description Is a numeric value between two other values?
#' 
#' @param x numeric value to check.
#' @param a,b numeric values describing range. 
#' @param include.ends logical. Should test include \code{a} and \code{b}? Is 
#'   test > and < or >= and <= ?
#' @param na.convert logical. If \code{TRUE} and result of test is \code{NA} 
#'   because either \code{x}, \code{a}, or \code{b} is \code{NA}, 
#'   return \code{FALSE}, otherwise return \code{NA}.
#'
#' @details Order of \code{a} and \code{b} does not matter. If \code{b} is 
#'   \code{NULL} the range will be taken from values in \code{a}.
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @export
#' 
isBetween <- function(x, a, b = NULL, include.ends = FALSE, na.convert = TRUE) {
  rng <- range(c(a, b))
  result <- if(!include.ends) {
    x > rng[1] & x < rng[2]
  } else {
    x >= rng[1] & x <= rng[2]
  }
  if(is.na(result) & na.convert) FALSE else result
}