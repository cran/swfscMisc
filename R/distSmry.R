#' @title Distribution summary
#' @description Summarize a numerical distribution.
#' 
#' @param x vector of numerical values.
#' @param p percent of distribution to summarized by quantile interval (ci) and 
#'   highest posterior density interval (hdi).
#' @param dens.n number of points used to estimate the mode from the fitted 
#'   density (see \code{\link[stats]{density}}).
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @export
#' 
distSmry <- function(x, p = 0.95, dens.n = 10000) {
  x <- as.numeric(x)
  num.NA = sum(is.na(x))
  x <- stats::na.omit(x)
  if(length(x) == 0) stop("'x' has no non-NA values.")
  if(!isBetween(p, 0, 100, include.ends = TRUE)) {
    stop("'p' must be between 0 and 100.")
  }
  n <- length(x)
  if(dens.n <= n) stop("'dens.n' must be > length(x).")
  
  if(p > 1) p <- p / 100
  lci <- (1 - p) / 2
  uci <- 1 - lci
  
  hdi.x <- unname(HDInterval::hdi(x, credMass = p))
  quant.x <- unname(stats::quantile(x, p = c(lci, uci)))
  dens.x <- stats::density(x, n = dens.n)
  
  c(
    n = n,
    num.NA = num.NA,
    mean = mean(x),
    median = median(x),
    mode = dens.x$x[which.max(dens.x$y)],
    min = min(x),
    max = max(x),
    sd = stats::sd(x),
    ci.lower = quant.x[1],
    ci.upper = quant.x[2],
    hdi.lower = hdi.x[1],
    hdi.upper = hdi.x[2]
  )
}
