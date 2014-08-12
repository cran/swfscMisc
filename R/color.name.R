#' @export color.name
#' 
#' @title Color Name
#' @description Return the name of a color listed given the number.
#'  
#' @param i integer specifying color .
#' 
#' @return character value of 'i' color.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}

color.name <- function(i) colors()[i]