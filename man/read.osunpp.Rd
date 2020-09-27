\name{read.osunpp}
\alias{read.osunpp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read ocean productivity data from Oregon State University files
}
\description{
Read, for a user defined area, net primary production data from hdf files downloaded from OSU's Ocean Productivity home page.
}
\usage{
read.osunpp(h5, lons, lats)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{h5}{ name of the hdf file(s) to be read.}
  \item{lons}{ a vector with western- and eastern-most longitudes. }
  \item{lats}{ a vector with southern- and northern-most  latitudes. }
}
\details{
This function reads Oregon State University ocean productivity data files (\url{http://sites.science.oregonstate.edu/ocean.productivity/standard.product.php}).  However, downloaded files should be first converted from hdf version 4 to version 5 with h4h5tools (\url{https://portal.hdfgroup.org/display/support/Download+h4h5tools}).  The user must specify the minimum and maximum latitude and longitude limits within the interval (-90,  90) and (-180, 180) respectively.  
}
\value{
 An object of class \code{"satin"}. See \code{\link{satin-class}} for details.
}
\author{
Héctor Villalobos and Eduardo González-Rodríguez
}
\seealso{
\code{\link{read.nasaoc}} for reading NASA's oceancolor web page data, \code{\link{read.ghrsst}} for JPL MUR SST data, and \code{\link{read.cmems}} for data from Copernicus Marine Service.
}
\examples{
if(interactive()){
 # read a single file, look at its structure and plot
 npp <- read.osunpp("vgpm.2013089.h5", 
                        lats=c(20, 30), lons=c(-130, -105))
 npp
 str(npp)
 plot(npp)

 # read several files residing in the working directory 
 files <- list.files(pattern = glob2rx("*.h5"))
 lats <- c(20, 30)
 lons <- c(-130, -105)
 mNPP <- read.osunpp(files, lats, lons)      

 # plotting the first processed file
 plot(mNPP) 

 # plotting the second processed file
 plot(mNPP, period = 2)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }% use one of  RShowDoc("KEYWORDS")
\keyword{ file }% __ONLY ONE__ keyword per line
