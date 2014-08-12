#' @export lat.lon.axes
#' 
#' @title Latitude and Longitude axes
#' @description Add latitude and longitude axes to a map.
#' 
#' @param lon.range,lat.range two-element vectors giving the minimum and maximum longitude and latitude.
#' @param n,lon.n,lat.n the number of tick marks desired. Can be specified separately for longitude (\code{lon.n}) 
#' or latitude (\code{lat.n}). See \code{\link{pretty}} for more details.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}

lat.lon.axes <- function(lon.range, lat.range, n = 5, lon.n = n, lat.n = n) {
  lon <- list(ticks = pretty(lon.range, n = lon.n))
  lon$labels <- parse(text = sapply(lon$ticks, function(i) {
    a <- ifelse(i <= 0, -1 * i, i)
    b <- ifelse(i <= 0, "W", "E")
    paste(a, "*degree~", b, sep = "")
  }))
  
  lat <- list(ticks = pretty(lat.range, n = lat.n))
  lat$labels <- parse(text = sapply(lat$ticks, function(i) {
    a <- ifelse(i <= 0, -1 * i, i)
    b <- ifelse(i <= 0, "S", "N")
    paste(a, "*degree~", b, sep = "")
  }))
  
  axis(1, at = lon$ticks, labels = lon$labels)
  axis(2, at = lat$ticks, labels = lat$labels, las = 1)
  axis(3, at = lon$ticks, labels = lon$labels)
  axis(4, at = lat$ticks, labels = lat$labels, las = 1)
}
