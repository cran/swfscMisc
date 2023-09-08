#' @title Unbiased Estimate of Diversity
#' @description Calculate unbiased estimate of diversity for a vector of items
#' 
#' @param x character or numeric vector or factor 
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples 
#' x <- sample(1:5, 100, replace = TRUE)
#' diversity(x)
#' 
#' @export
#' 
diversity <- function(x) {
  warning(
    "This function has been replaced by sprex::diversity(x, type = 'unb.gini').",
    "It will be removed from swfscMisc after version 1.6.5"
  )
  if(!(is.vector(x) | is.factor(x))) {
    stop("'x' must be a character or numeric vector, or a factor")
  }
  x <- stats::na.omit(x)
  x.freq <- prop.table(table(x))
  n <- length(x)
  n * (1 - sum(x.freq ^ 2)) / (n - 1)
}