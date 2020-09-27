\name{read.nasaoc}
\alias{read.nasaoc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read ocean data from NASA's Oceancolor web files
}
\description{
Read, for a user defined area, data from netCDF files downloaded from NASA's Oceancolor web page.
}
\usage{
read.nasaoc(nc, lons, lats)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nc}{ name of the netCDF file(s) to be read.}
  \item{lons}{ a vector with western- and eastern-most longitudes. }
  \item{lats}{ a vector with southern- and northern-most  latitudes. }
}
\details{
This function can read data from one or multiple netCDF files downloaded from NASA's oceancolor web page (\url{ https://oceancolor.gsfc.nasa.gov/l3/ }).  It has been tested mainly with sea surface temperature and chlorophyll concentration data from MODIS-Aqua and SeaWiFS sensors.  It also works with Pathfinder AVHRR files v 5.2 and 5.3 (\url{ https://www.ncei.noaa.gov/products/avhrr-pathfinder-sst }).  The user must specify the minimum and maximum longitude and latitude limits within the interval (-180, 180) and (-90,  90) respectively. 
}
\value{
 An object of class \code{"satin"}. See \code{\link{satin-class}} for details.
}
\author{
Héctor Villalobos and Eduardo González-Rodríguez
}
\seealso{
\code{\link{read.ghrsst}} for JPL MUR SST data, \code{\link{read.osunpp}} for reading ocean productivity data, and \code{\link{read.cmems}} for data from Copernicus Marine Service.
}
\examples{
if(interactive()){
 # read a single file, look at it and plot
 sst <- read.nasaoc("A20130892013096.L3m_8D_SST4_sst4_4km.nc", 
                        lats=c(20, 30), lons=c(-130, -105))
 sst
 str(sst)
 plot(sst)

 # read several files residing in the working directory 
 files <- list.files(pattern = glob2rx("*.nc"))
 lats <- c(20, 30)
 lons <- c(-130, -105)
 mSST <- read.nasaoc(files, lats, lons)      

 # plotting the first processed file
 plot(mSST) 

 # plotting the second processed file
 plot(mSST, period = 2)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }% use one of  RShowDoc("KEYWORDS")
\keyword{ file }% __ONLY ONE__ keyword per line
