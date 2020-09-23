\name{pixelate}
\alias{pixelate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Spatial re-scaling of ocean data }
\description{
This function down-scales ocean data by quadrants of user defined size (in degrees) according to a given function.
}
\usage{
pixelate(X, extent = 0.25, FUN = mean)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{ a satin object as returned by \code{\link{read.nasaoc}}, \code{\link{read.ghrsst}}, \code{\link{read.osunpp}}, or \code{\link{read.cmems}}. }
  \item{extent}{ size in degrees of the squared quadrant for the new spatial scale.}
  \item{FUN}{ function to be applied for obtaining the quadrants' values, defaults to \code{mean}.}
}
\details{
The main interest of this function is to obtain smooth isolines for ocean data (e.g. isotherms).  In order to use this function with Copernicus data returned from \code{\link{read.cmems}}, the appropriate indexing should be done.
}
\value{
 An object of class \code{"satin"}. See \code{\link{satin-class}} for details.
}
\author{ Héctor Villalobos }

\seealso{ \code{\link{isolines}} }

\examples{
# load and plot sample SST data 
data(dsst)
plot(dsst)

# change spatial resolution to 0.5 degrees
sst0.5 <- pixelate(dsst, extent = 0.5) 
plot(sst0.5)
}

\keyword{ array }
\keyword{ manip }