#' @rdname scatterdens

scatterhist <- function(x, y, xlab = "", ylab = "", dens.frac = 1/5, ...) {
  zones <- matrix(c(2,0,1,3), ncol = 2, byrow = TRUE)
  layout(zones, widths = c(1 - dens.frac, dens.frac), heights = c(dens.frac, 1 - dens.frac))
  xhist <- hist(x, plot = FALSE)
  yhist <- hist(y, plot = FALSE)
  top <- max(c(xhist$counts, yhist$counts))
  op <- par(mar = c(3, 3, 1, 1))
  plot(x, y, main = "", ...)
  par(mar = c(0, 3, 1, 1))
  barplot(xhist$counts, axes = FALSE, ylim = c(0, top), space = 0)
  par(mar = c(3, 0, 1, 1))
  barplot(yhist$counts, axes = FALSE, xlim = c(0, top), space = 0, horiz = TRUE)
  par(op)
}