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
Formal class 'satin' [package "satin"] with 6 slots
\tabular{ll}{
  ..@ lon   \tab : num [1:360] -119 -119 -119 -119 -119 ...\cr
  ..@ lat   \tab : num [1:360] 20 20.1 20.1 20.1 20.2 ...\cr
  ..@ data  \tab : num [1:360, 1:360, 1] 0.0631 0.0696 0.0724 0.0723 0.0705 ...\cr
  ..@ attribs: List of 6 \tab \cr
  .. ..$ title     \tab        : chr "HMODISA Level-3 Standard Mapped Image"\cr
  .. ..$ longname  \tab         : chr "Chlorophyll Concentration, OCI Algorithm"\cr
  .. ..$ name     \tab         : chr "chlor_a"\cr
  .. ..$ units    \tab         : chr "mg m^-3"\cr
  .. ..$ temporal_range \tab   : chr "8-day"\cr
  .. ..$ spatial_resolution \tab : chr "4.64 km"\cr
  ..@ period: List of 2 \tab \cr
  .. ..$ tmStart \tab : POSIXct[1:1], format: "2013-03-30 00:25:01"\cr
  .. ..$ tmEnd \tab  : POSIXct[1:1], format: "2013-04-07 02:59:59"\cr
  ..@ depth \tab : num(0)
  }
}
\details{
Source data file (A20130892013096.L3m_8D_CHL_chlor_a_4km.nc) was downloaded from the link below, then imported with the \code{\link{read.nasaoc}} function.  The selected area covers from 20 to 35 degrees of latitude North and from 119 to 104 degrees of longitude West.
}
\source{
  \url{ https://oceancolor.gsfc.nasa.gov/cgi/l3 }
}
\references{
  \url{ https://oceancolor.gsfc.nasa.gov/docs/technical/ }
}
\examples{
data(dchl)
dchl
str(dchl)

plot(dchl)

# plot with logarithmic scale
plot(dchl, log = TRUE)
}
\keyword{datasets}


