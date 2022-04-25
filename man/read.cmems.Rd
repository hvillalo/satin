\name{read.cmems}
\alias{read.cmems}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read ocean products data from CMEMS
}
\description{
Read data from netCDF files downloaded from Copernicus Marine Service (CMEMS).
}
\usage{
read.cmems(nc)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nc}{ name of the netCDF file to be read.}
}
\details{
The function can read data from netCDF files downloaded from CMEMS web page (\url{ https://marine.copernicus.eu/ }).  It has been tested with Global Ocean Physical Reanalysis, and Global Ocean Biogeochemistry Hindcast databases.  When downloading Copernicus data files, the user can select one or several variables (see references below), depth levels, daily or monthly means, time and spatial coverage.  This makes every Copernicus netCDF file unique.  The \code{read.cmems} function attempts to read all the variables and depth levels at once.  When only one variable is included in the file, a \code{"satin"} object with one additional dimension in the data array to accommodate the different depth levels, is produced.  On the other hand, when several variables are present, the function returns a named list, with every element being a \code{"satin"} object corresponding to each variable. 
}

\value{
A named list, with elements corresponding to each variable included in the netCDF file.  Each list element (or when only one variable is present) is an object of class \code{"satin"}. See \code{\link{satin-class}} for details.
}
\references{
\url{ https://resources.marine.copernicus.eu/?option=com_csw&task=results }

Fernandez, E. and Lellouche, J. M. 2018 Product User Manual for the Global Ocean Physical Reanalysis product, issue 1.1. EU Copernicus Marine Service.
  
Garric, G. and Parent, L. 2018 Product User Manual for the Global Ocean Reanalysis Products, issue 3.4. EU Copernicus Marine Service.
  
Perruche, C. 2019 Product User Manual for the Global Ocean Biogeochemistry Hindcast, issue 1.1. EU Copernicus Marine Service.
}
\author{
Héctor Villalobos
}
\seealso{
\code{\link{read.nasaoc}} for reading NASA's oceancolor web page data,  \code{\link{read.ghrsst}} for JPL MUR SST data, and \code{\link{read.osunpp}} for reading ocean productivity data.
}
\examples{
if(interactive()){
 # read Copernicus netcdf file with one variable
 cmems <- read.cmems("global-reanalysis-phy-001-030-monthly_1553098099635.nc")
 cmems
 str(cmems)
 
 # plotting the first period
 plot(cmems) 

 # plotting the second period 
 plot(cmems, period = 2)

 # read Copernicus netCDF file with several variables and depth levels
 cmems2 <- read.cmems("global-reanalysis-phy-001-030-monthly_1553051256930.nc")      
 cmems2 # it is a list
 
 # readed variables
 names(cmems2)
 
 # example plots for potential temperature
 thetao <- cmems2$thetao
 thetao
 thetao$depth # depth levels
 plot(thetao) # potential temperature at first depth level (0.49 m) and first month 
 plot(thetao, period = 2, depth = 5) # for fifth depth level (5.078 m), second month
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }% use one of  RShowDoc("KEYWORDS")
\keyword{ file }% __ONLY ONE__ keyword per line
