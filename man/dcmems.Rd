\name{dcmems}
\alias{dcmems}
\docType{data}
\title{
 Copernicus Marine Environment Monitoring Service sample data 
}
\description{
  This sample data contains monthly values (November and December 2010) of sea water potential temperature (thetao, degrees C) and sea surface height (zos, m) off northwest Mexico, 1/12 degree horizontal resolution (aprox. 9.26 km).
}

\usage{data("dcmems")}
\format{
  The format is:
List of 2
 $ thetao:Formal class 'satin' [package "satin2"] with 6 slots
  .. ..@ lon    : num [1:181] -119 -119 -119 -119 -119 ...
  .. ..@ lat    : num [1:169] 20 20.1 20.2 20.2 20.3 ...
  .. ..@ data   : num [1:169, 1:181, 1:2, 1:5] 23.5 23.5 23.5 23.4 23.4 ...
  .. ..@ attribs:List of 6
  .. .. ..$ title             : chr "thetao"
  .. .. ..$ longname          : chr "Temperature"
  .. .. ..$ name              : chr "thetao"
  .. .. ..$ units             : chr "degrees_C"
  .. .. ..$ temporal_range    : chr "monthly"
  .. .. ..$ spatial_resolution: chr "9.2 km"
  .. ..@ period :List of 2
  .. .. ..$ tmStart: POSIXct[1:2], format: "2010-11-16 00:00:00" ...
  .. .. ..$ tmEnd  : POSIXct[1:2], format: "2010-11-16 00:00:00" ...
  .. ..@ depth  : num [1:5] 0.494 9.573 29.445 92.326 318.127
 $ zos   :Formal class 'satin' [package ".GlobalEnv"] with 6 slots
  .. ..@ lon    : num [1:181] -119 -119 -119 -119 -119 ...
  .. ..@ lat    : num [1:169] 20 20.1 20.2 20.2 20.3 ...
  .. ..@ data   : num [1:169, 1:181, 1:2, 1] 0.336 0.34 0.343 0.345 0.347 ...
  .. ..@ attribs:List of 6
  .. .. ..$ title             : chr "zos"
  .. .. ..$ longname          : chr "Sea surface height"
  .. .. ..$ name              : chr "zos"
  .. .. ..$ units             : chr "m"
  .. .. ..$ temporal_range    : chr "monthly"
  .. .. ..$ spatial_resolution: chr "9.2 km"
  .. ..@ period :List of 2
  .. .. ..$ tmStart: POSIXct[1:2], format: "2010-11-16 00:00:00" ...
  .. .. ..$ tmEnd  : POSIXct[1:2], format: "2010-11-16 00:00:00" ...
  .. ..@ depth  : num 0.494
}
\details{
Source data file was downloaded from the link below, then imported with the \code{\link{read.cmems}} function. The selected area covers from 20 to 35 degrees of latitude North and from 119 to 104 degrees of longitude West. Potential temperature includes five depth levels (0.494, 9.573, 29.445, 92.326, 318.127).
}
\source{
  \url{http://marine.copernicus.eu/}
}
\references{
  Fernandez E and Lellouche JM. 2018. Product Manual for the Global Ocean Physical Reanalysis product, issue 1.1. EU Copernicus Marine Service.
}
\examples{
data(dcmems)
str(dcmems)
plot(dcmems$thetao)

# potential temperature at 318 m
plot(dcmems$thetao, depth = 5) 

# sea surface height
plot(dcmems$zos) 
}
\keyword{datasets}
