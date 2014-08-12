#' @rdname scatterdens
#' @export scatterhist scatterdens
#' 
#' @title Scatter Plot with Density Margins
#' 
#' @param x,y vectors of points to plot.
#' @param xlab,ylab labels for x and y axes.
#' @param dens.frac fraction of screen to be taken up by density plots on margins.
#' @param ... Arguments to be passed to \link{plot}.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @references Original code by Ken Kleiman: \url{http://sas-and-r.blogspot.co.uk/2011/06/example-841-scatterplot-with-marginal.html}
#' 
#' @examples
#' x <- rnorm(100)
#' y <- rlnorm(100)
#' op <- par(ask = TRUE)
#' scatterdens(x, y, xlab = "x", ylab = "y")
#' par(op)

scatterdens <- function(x, y, dens.frac = 1/5, ...) {
  zones <- matrix(c(2,0,1,3), ncol = 2, byrow = TRUE)
  layout(zones, widths = c(1 - dens.frac, dens.frac), heights = c(dens.frac, 1 - dens.frac))
  x.dens <- density(x)
  y.dens <- density(y)
  top <- max(x.dens$y, y.dens$y)
  op <- par(mar = c(3, 3, 1, 1))
  plot(x, y, main = "", ...)
  par(mar = c(0, 3, 1, 1))
  plot(x.dens, axes = FALSE, ylim = c(0, top), main = "")
  par(mar = c(3, 0, 1, 1))
  plot(y.dens$y, y.dens$x, type = "l", axes = FALSE, xlim = c(0, top))
  par(op)
}