#' @title Distance Conversion
#' @description Convert distances between kilometers, nautical miles, and statute miles.
#' 
#' @param x numeric. The distance to be converted.
#' @param from,to character. Units to convert from and to. Can be "km" (kilometers), 
#' "nm" (nautical miles), or "mi" (statute miles).
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @export
#' 
convert.distance <- function(x, from = "nm", to = "km") {
  x <- switch(from,
    km = x,
    nm = x * 1.852,
    mi = x * 1.609344,
    NA
  )
  
  result <- switch(to,
    km = x,
    nm = x / 1.852,
    mi = x / 1.609344,
    NA
  )
  
  as.numeric(result)
}
