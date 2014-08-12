#' @export one.arg
#' 
#' @title One Argument
#' @description Does the function have just one argument?

#' @param f a function.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' one.arg(mean)
#' one.arg(one.arg)

one.arg <- function(f) length(formals(f)) == 1