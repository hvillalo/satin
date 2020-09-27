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
Formal class 'satin' [package "satin"] with 6 slots
\tabular{ll}{
  ..@ lon \tab   : num [1:360] -119 -119 -119 -119 -119 ...\cr
  ..@ lat \tab   : num [1:360] 20 20.1 20.1 20.1 20.2 ...\cr
  ..@ data \tab   : num [1:360, 1:360, 1] NA NA NA NA 18.2 ...\cr
  ..@ attribs :List of 6 \tab \cr
  .. ..$ title    \tab    : chr "HMODISA Level-3 Standard Mapped Image"\cr
  .. ..$ longname \tab    : chr "4um Sea Surface Temperature"\cr
  .. ..$ name     \tab    : chr "sst4"\cr
  .. ..$ units    \tab    : chr "degree_C"\cr
  .. ..$ temporal_range \tab   : chr "8-day"\cr
  .. ..$ spatial_resolution \tab : chr "4.64 km"\cr
  ..@ period :List of 2 \tab \cr
  .. ..$ tmStart \tab : POSIXct[1:1], format: "2013-03-29 12:05:08"\cr
  .. ..$ tmEnd \tab  : POSIXct[1:1], format: "2013-04-06 14:50:08"\cr
  ..@ depth \tab : num(0) 
  }
}
\details{
Source data file (A20130892013096.L3m_8D_SST4_sst4_4km.nc) was downloaded from the link below, then imported with the \code{\link{read.nasaoc}} function.  The selected area covers from 20 to 35 degrees of latitude North and from 119 to 104 degrees of longitude West.
}
\source{
  \url{ https://oceancolor.gsfc.nasa.gov/cgi/l3 }
}
\references{
  \url{ https://oceancolor.gsfc.nasa.gov/docs/technical/ }
}
\examples{
data(dsst)
dsst
str(dsst)
plot(dsst)
}
\keyword{datasets}

