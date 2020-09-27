\name{read.ghrsst}
\alias{read.ghrsst}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read sea surface temperature data from JPL Multi-scale Ultra-high Resolution (MUR) SST Project
}
\description{
Read, for a user defined area, SST data from netCDF files downloaded from JPL's Physical Oceanography Distributed Active Archive Center (podaac) web page.
}
\usage{
read.ghrsst(nc, lons, lats)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nc}{ name of the netCDF file(s) to be read.}
  \item{lons}{ a vector with western- and eastern-most longitudes. }
  \item{lats}{ a vector with southern- and northern-most  latitudes. }
}
\details{
This function can read SST data from one or multiple netCDF files downloaded from Caltech Jet Propulsion Laboratory web page (\url{ https://podaac-opendap.jpl.nasa.gov/opendap/allData/ghrsst/data/GDS2/L4/GLOB/JPL/MUR/v4.1/ }).  The user must specify the minimum and maximum latitude and longitude limits within the interval (-90,  90) and (-180, 180) respectively. 
}
\value{
 An object of class \code{"satin"}. See \code{\link{satin-class}} for details.
}
\references{
\url{ https://www.ghrsst.org/ }
}
\author{
Héctor Villalobos and Eduardo González-Rodríguez
}
\seealso{
\code{\link{read.nasaoc}} for reading NASA's oceancolor web page data, \code{\link{read.osunpp}} for reading ocean productivity data, and \code{\link{read.cmems}} for data from Copernicus Marine Service.
}
\examples{
if(interactive()){
 # read a single file, look at its structure and plot
 sst1km <- read.nasaoc("20130101090000-JPL-L4_GHRSST-SSTfnd-MUR-GLOB-v02.0-fv04.1.nc", 
                        lats=c(20, 30), lons=c(-130, -105))
 sst1km
 str(sst1km)
 plot(sst1km)
 
 # read several files residing in the working directory 
 files <- list.files(pattern = glob2rx("*.nc"))
 lats <- c(20, 30)
 lons <- c(-130, -105)
 mSST <- read.ghrsst(files, lats, lons)      

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
