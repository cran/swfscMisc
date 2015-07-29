#' @title Angle Conversion
#' @description Converts angles between radians and degrees.
#' 
#' @param x numeric. The angle to be converted.
#' @param from,to character. Units to convert from and to. Can be 
#' "radians" or "degrees" or any partial match.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' convert.angle(45, "deg", "rad")
#' convert.angle(4.5, "r", "d")
#' 
#' @export
#' 
convert.angle <- function(x, from = "degrees", to = "radians") {
  units <- c("degrees", "radians")
  from <- units[pmatch(tolower(from), units)]
  to <- units[pmatch(tolower(to), units)]
  
  x <- switch(from,
    degrees = x,
    radians = x * 180 / pi,
    NA
  )
  
  result <- switch(to,
    degrees = x,
    radians = x * pi / 180,
    NA
  )
  
  as.numeric(result)
}