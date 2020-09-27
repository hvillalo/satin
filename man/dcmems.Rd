\name{dcmems}
\alias{dcmems}
\docType{data}
\title{
Copernicus Marine Service sample data
}
\description{
This sample data contains monthly values (November and December 2010) of sea water potential temperature (thetao, degrees C) and sea surface height (zos, m) off northwest Mexico, 1/12 degree horizontal resolution (aprox. 9.26 km).
}

\usage{data("dcmems")}
\format{
List of 2\cr
$ thetao:Formal class 'satin' [package "satin"] with 6 slots
\tabular{ll}{ 
  .. ..@ lon \tab   : num [1:181] -119 -119 -119 -119 -119 ...\cr
  .. ..@ lat \tab   : num [1:169] 20 20.1 20.2 20.2 20.3 ...\cr
  .. ..@ data \tab  : num [1:169, 1:181, 1:2, 1:5] 23.5 23.5 23.5 23.4 23.4 ...\cr
  .. ..@ attribs:List of 6 \tab \cr
  .. .. ..$ title     \tab        : chr "thetao"\cr
  .. .. ..$ longname  \tab        : chr "Temperature"\cr
  .. .. ..$ name      \tab        : chr "thetao"\cr
  .. .. ..$ units     \tab        : chr "degrees_C"\cr
  .. .. ..$ temporal_range  \tab  : chr "monthly"\cr
  .. .. ..$ spatial_resolution \tab : chr "9.2 km"\cr
  .. ..@ period :List of 2 \tab \cr
  .. .. ..$ tmStart \tab : POSIXct[1:2], format: "2010-11-16 00:00:00" ...\cr
  .. .. ..$ tmEnd  \tab : POSIXct[1:2], format: "2010-11-16 00:00:00" ...\cr
  .. ..@ depth  \tab  : num [1:5] 0.494 9.573 29.445 92.326 318.127
  }
 $ zos   :Formal class 'satin' [package "satin"] with 6 slots
\tabular{ll}{  
  .. ..@ lon  \tab  : num [1:181] -119 -119 -119 -119 -119 ...\cr
  .. ..@ lat  \tab  : num [1:169] 20 20.1 20.2 20.2 20.3 ...\cr
  .. ..@ data \tab   : num [1:169, 1:181, 1:2, 1] 0.336 0.34 0.343 0.345 0.347 ...\cr
  .. ..@ attribs:List of 6 \tab \cr
  .. .. ..$ title     \tab        : chr "zos"\cr
  .. .. ..$ longname  \tab        : chr "Sea surface height"\cr
  .. .. ..$ name      \tab        : chr "zos"\cr
  .. .. ..$ units     \tab        : chr "m"\cr
  .. .. ..$ temporal_range  \tab  : chr "monthly"\cr
  .. .. ..$ spatial_resolution \tab : chr "9.2 km"\cr
  .. ..@ period :List of 2 \tab \cr
  .. .. ..$ tmStart \tab : POSIXct[1:2], format: "2010-11-16 00:00:00" ...\cr
  .. .. ..$ tmEnd \tab : POSIXct[1:2], format: "2010-11-16 00:00:00" ...\cr
  .. ..@ depth  \tab : num 0.494
  }
}
\details{
Source data corresponds to Global Ocean Physical Reanalysis database (GLOBAL_REANALYSIS_PHY_001_030), and was downloaded from the link below, then imported with the \code{\link{read.cmems}} function.  The selected area covers from 20 to 35 degrees of latitude North and from 119 to 104 degrees of longitude West.  Potential temperature includes five depth levels (0.494, 9.573, 29.445, 92.326, 318.127).
}
\source{
  \url{ https://marine.copernicus.eu/ }
}
\references{
Fernandez, E. and Lellouche, J. M. 2018 \emph{Product Manual for the Global Ocean Physical Reanalysis product, issue 1.1}. EU Copernicus Marine Service.
}
\examples{
data(dcmems)
dcmems

plot(dcmems$thetao)

# potential temperature at 318 m
plot(dcmems$thetao, depth = 5) 

# sea surface height
plot(dcmems$zos) 
}
\keyword{datasets}
