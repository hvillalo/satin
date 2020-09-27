\name{dnpp}
\alias{dnpp}
\docType{data}
\title{
Ocean Net Primary Production sample data
}
\description{
Ocean Net Primary Production (8-day average; 9.26 km resolution) from March 30 to April 6 2013 off northwest Mexico.
}
\usage{data("dnpp")}
\format{
Formal class 'satin' [package "satin"] with 6 slots
\tabular{ll}{
  ..@ lon  \tab  : num [1:180] -119 -119 -119 -119 -119 ...\cr
  ..@ lat  \tab  : num [1:180] 20.1 20.1 20.2 20.3 20.4 ...\cr
  ..@ data  \tab : num [1:180, 1:180, 1] 285 292 270 279 262 ...\cr
  ..@ attribs:List of 6 \tab \cr
  .. ..$ title    \tab         : chr "Ocean Productivity"\cr
  .. ..$ longname \tab         : chr "Ocean Net Primary Production"\cr
  .. ..$ name     \tab         : chr "npp"\cr
  .. ..$ units    \tab         : chr "mgC m-2 day-1"\cr
  .. ..$ temporal_range \tab   : chr "8 day"\cr
  .. ..$ spatial_resolution \tab : chr "9.26 km"\cr
  ..@ period :List of 2 \tab \cr
  .. ..$ tmStart \tab : POSIXct[1:1], format: "2013-03-30"\cr
  .. ..$ tmEnd \tab  : POSIXct[1:1], format: "2013-04-06 23:59:59"\cr
  ..@ depth \tab : num(0)
  }
}
\details{
Source data file (vgpm.2013089.hdf.gz) was downloaded from the link below, decompressed with \code{gunzip} function from \pkg{R.utils} package, then converted to hdf v5 with h4toh5 Conversion Software (\url{ https://support.hdfgroup.org/products/hdf5_tools/h4toh5/download.html }) before being imported with the \code{\link{read.osunpp}} function.  The selected area covers from 20 to 35 degrees of latitude North and from 119 to 104 degrees of longitude West.
}
\source{
  \url{ http://sites.science.oregonstate.edu/ocean.productivity/standard.product.php }
}
\references{
Behrenfeld, M. J. and Falkowski, P. G. 1997 Photosynthetic rates derived from satellite-based chlorophyll concentration, \emph{Limnology and Oceanography}, \bold{42}, 1--20.
}
\examples{
data(dnpp)
dnpp
str(dnpp)
plot(dnpp, col.sep = 100)
}
\keyword{datasets}


