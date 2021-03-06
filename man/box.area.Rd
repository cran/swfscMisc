% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/box.area.R
\name{box.area}
\alias{box.area}
\title{Area of a Box}
\usage{
box.area(lat, lon, edge, units = "nm")
}
\arguments{
\item{lat, lon}{The latitude and longitude of the lower right corner of the box in decimal degrees.}

\item{edge}{The length of one side of the square in decimal degrees.}

\item{units}{units of distance. Can be "km" (kilometers), "nm" (nautical miles), 
or "mi" (statute miles).}
}
\description{
Calculate the area of a square on the earth.
}
\examples{
#What is the area of a 5 degree grid off of San Diego, CA?
box.area(32.87, -117.25, edge = 1, units = "nm")
box.area(32.87, -117.25, edge = 1, units = "km")
box.area(32.87, -117.25, edge = 1, units = "mi")

}
\author{
Eric Archer \email{eric.archer@noaa.gov}
}
