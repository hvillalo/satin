\name{climatology}
\alias{climatology}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Climatology of a satin object
}
\description{
This function calculates the percent coverage, mean, standard deviation, minimum and maximum for every pixel in a set of images stored in a single satin data object.
}
\usage{
climatology(X, depth = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
  a satin object as returned by \code{\link{read.nasaoc}}, \code{\link{read.ghrsst}}, \code{\link{read.osunpp}}, or \code{\link{read.cmems}}. }
  \item{depth}{
  an integer referring to a given  depth level. Only meaningful for Copernicus data that includes variables at different depths.}
}
\details{
For Copernicus data files containing more than one variable, the \code{\link{read.cmems}} function returns a named list, with elements corresponding to each variable.  In order to use these objects with \code{climatology} the appropriate indexing should be applied (see examples below).
}
\value{
 An object of  class \code{"satin"} (see \code{\link{satin-class}} for details) where the third dimension in the data array accomodates coverage, mean, standard deviation, minimum and maximum.  Also, an extra element (\code{label}) is included in the slot \code{attribs} to summarize the start and end times used for the climatology.
}
\author{
Héctor Villalobos and Eduardo González-Rodríguez
}
\examples{
if(interactive()){
csst <- climatology(sst) # sst is a satin object with 12 monthly images 
                         # of sea surface temperature

# -- plots --

# coverage
plot(csst, period = 1)

# mean
plot(csst, period = 2)

# standard deviation
plot(csst, period = 3)

# minimum
plot(csst, period = 4)

# maximum
plot(csst, period = 5)
}
# For Copernicus data
\donttest{
data(dcmems) # load sample data
names(dcmems) # available variables 

# mean potential temperature (thetao) at 0.49 m
# using labels stored in slot period
cthetao1 <- climatology(dcmems$thetao, depth = 1)
plot(cthetao1, period = 2, main = cthetao1@period$label[2])

# standard deviation of thetao at 318 m
cthetao2 <- climatology(dcmems$thetao, depth = 5)
plot(cthetao2, period = 3, main = cthetao1@period$label[3])
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }% use one of  RShowDoc("KEYWORDS")
\keyword{ manip }% __ONLY ONE__ keyword per line
