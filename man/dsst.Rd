\name{dsst}
\alias{dsst}
\docType{data}
\title{
 SST sample data - Aqua Modis sensor 
}
\description{
  Sea Surface Temperature (4 um nighttime; 8-day average; 4 km resolution) from March 29 to April 6 2013 off northwest Mexico.
}
\usage{data("dsst")}
\format{
  The format is:
Formal class 'satin' [package "satin2"] with 6 slots
  ..@ lon    : num [1:360] -119 -119 -119 -119 -119 ...
  ..@ lat    : num [1:360] 20 20.1 20.1 20.1 20.2 ...
  ..@ data   : num [1:360, 1:360, 1] NA NA NA NA 18.2 ...
  ..@ attribs:List of 6
  .. ..$ title             : chr "HMODISA Level-3 Standard Mapped Image"
  .. ..$ longname          : chr "4um Sea Surface Temperature"
  .. ..$ name              : chr "sst4"
  .. ..$ units             : chr "degree_C"
  .. ..$ temporal_range    : chr "8-day"
  .. ..$ spatial_resolution: chr "4.64 km"
  ..@ period :List of 2
  .. ..$ tmStart: POSIXct[1:1], format: "2013-03-29 12:05:08"
  .. ..$ tmEnd  : POSIXct[1:1], format: "2013-04-06 14:50:08"
  ..@ depth  : num(0) 
}
\details{
Source data file (A20130892013096.L3m_8D_SST4_sst4_4km.nc) was downloaded from the link below, then imported with the \code{\link{read.nasaoc}} function. The selected area covers from 20 to 35 degrees of latitude North and from 119 to 104 degrees of longitude West.
}
\source{
  \url{http://oceancolor.gsfc.nasa.gov/cgi/l3}
}
\references{
  \url{https://oceancolor.gsfc.nasa.gov/docs/technical/}
}
\examples{
data(dsst)
str(dsst)
summary(dsst)
plot(dsst)
}
\keyword{datasets}
