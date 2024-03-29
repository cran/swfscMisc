% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/destination.R
\name{destination}
\alias{destination}
\title{Destination on Sphere or Ellipsoid}
\usage{
destination(
  lat,
  lon,
  brng,
  distance,
  units = c("nm", "km", "mi"),
  ellipsoid = datum(),
  radius = convert.distance(6371, "km", "nm"),
  type = c("ellipsoid", "sphere", "vincenty")
)
}
\arguments{
\item{lat, lon}{numeric. The latitude and longitude of the coordinate in decimal degrees.}

\item{brng}{numeric. The bearing, ranging from 0 to 360 degrees.}

\item{distance}{numeric. The distance travelled, in units specified by \code{units}.}

\item{units}{units of distance. Can be "km" (kilometers), "nm" (nautical miles), 
or "mi" (statute miles), or any partial match thereof (case sensitive).}

\item{ellipsoid}{ellipsoid model parameters as returned from a call to \code{\link{datum}}.}

\item{radius}{numeric. Define the radius for \code{type} = "sphere". In units of \code{units}.}

\item{type}{Character defining type of surface. Can be "sphere", "ellipsoid", "vincenty", or 
partial match thereof (case-sensitive).}
}
\value{
latitude and longitude of destination.
}
\description{
Calculates latitude and longitude of the destination along a sphere or ellipsoid.
}
\examples{
destination(32.87, -117.25, 262, 4174, units = "km", type = "sphere")
destination(32.87, -117.25, 262, 4174, units = "km", type = "ellipsoid")
destination(32.87, -117.25, 262, 4174, units = "km", type = "vincenty")

}
\references{
Ellipsoid code adapted from JavaScript by 
\href{https://adsabs.harvard.edu/full/2000JRASC..94...48B}{Larry Bogan}.\cr
Vincenty code adapted from JavaScript by 
\href{https://www.movable-type.co.uk/scripts/latlong-vincenty.html#direct}{Chris Veness}.\cr
Vincenty, T. 1975.  Direct and inverse solutions of geodesics on the ellipsoid with 
application of nested equations. 
\href{https://www.ngs.noaa.gov/PUBS_LIB/inverse.pdf}{Survey Review 22(176):88-93}.
}
\author{
Eric Archer \email{eric.archer@noaa.gov}
}
