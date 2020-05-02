\name{dchl}
\alias{dchl}
\docType{data}
\title{
 Chlorophyll concentration sample data - Aqua Modis sensor
}
\description{
 Chlorophyll-a concentration (8-day average; 4 km resolution) from March 30 to April 7 2013 off northwest Mexico.
}
\usage{data("dchl")}
\format{
 The format is:
Formal class 'satin' [package "satin2"] with 6 slots
  ..@ lon    : num [1:360] -119 -119 -119 -119 -119 ...
  ..@ lat    : num [1:360] 20 20.1 20.1 20.1 20.2 ...
  ..@ data   : num [1:360, 1:360, 1] 0.0631 0.0696 0.0724 0.0723 0.0705 ...
  ..@ attribs:List of 6
  .. ..$ title             : chr "HMODISA Level-3 Standard Mapped Image"
  .. ..$ longname          : chr "Chlorophyll Concentration, OCI Algorithm"
  .. ..$ name              : chr "chlor_a"
  .. ..$ units             : chr "mg m^-3"
  .. ..$ temporal_range    : chr "8-day"
  .. ..$ spatial_resolution: chr "4.64 km"
  ..@ period :List of 2
  .. ..$ tmStart: POSIXct[1:1], format: "2013-03-30 00:25:01"
  .. ..$ tmEnd  : POSIXct[1:1], format: "2013-04-07 02:59:59"
  ..@ depth  : num(0) 
}
\details{
Source data file (A20130892013096.L3m_8D_CHL_chlor_a_4km.nc) was downloaded from the link below, then imported with the \code{\link{read.nasaoc}} function. The selected area covers from 20 to 35 degrees of latitude North and from 119 to 104 degrees of longitude West.
}
\source{
  \url{http://oceancolor.gsfc.nasa.gov/cgi/l3}
}
\references{
  \url{https://oceancolor.gsfc.nasa.gov/docs/technical/}
}
\examples{
data(dchl)
str(dchl)
summary(dchl)
plot(dchl)

# plot with logarithmic scale
plot(dchl, log = TRUE)
}
\keyword{datasets}


