\name{satin-class}
\Rdversion{1.1}
\docType{class}
\alias{satin-class}
\alias{satin}
\alias{print.satin}
\alias{show,satin-method}
\title{Class "satin"}
\description{
class for ocean data derived from satellites, imported from netCDF files.
}
\section{Objects from the Class}{
Objects of class \code{"satin"} are created by importing functions: \code{read.nasaoc}, \code{read.ghrsst}, \code{read.cmems}, and \code{read.osunpp}.  They can also be created, though not recommended, by direct calls to \code{satin}.
}
\section{Slots}{
  \describe{
    \item{lon}{Object of class \code{"numeric"}; vector of longitudes of the area of interest (aoi).}
    \item{lat}{Object of class \code{"numeric"}; vector of latitudes of the aoi.}
    \item{data}{Object of class \code{"array"}; three or four dimensions array of ocean data corresponding to the lengths of \code{lat}, \code{lon}, \code{period$tmStart}, and \code{depth} vectors. The fourth dimension associated to depth levels will only be present in \code{"satin"} objects derived from Copernicus data files.}
    \item{attribs}{Object of class \code{"list"}; attributes of the data stored in the netCDF file:   \code{title}: title attribute from netCDF file; \code{longname}: long name of ocean data variable; \code{name}: short name of ocean data variable; \code{units}: ocean data variable measurement units; \code{temporal_range}: averaging period length, i.e. daily, monthly, etc.; \code{spatial_resolution}: spatial resolution of the ocean data variable.}
    \item{period}{Object of class \code{"list"}; a vector with start time (\code{tmStart}) and a vector with end time (\code{tmEnd}) of the averaging period of the data. Both vectors are of class \code{"POSIXct"}.}
    \item{depth}{Object of class \code{"numeric"}; vector of depth levels in the data. Only meaningful for \code{"satin"} objects derived from Copernicus data files.}
  }
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "satin", y = "missing")}: plot method for \code{"satin"} objects. See details in \code{plot.satin}}
    \item{show}{\code{signature(object = "satin")}: display a summary of the object. See example below}
  }
}
\author{
Héctor Villalobos
}
\seealso{
See \code{\link{read.nasaoc}}; \code{\link{read.osunpp}}; \code{\link{read.ghrsst}}; and \code{\link{read.cmems}} for functions producing \code{"satin"} objects, and \code{plot.satin} for a complete description of plotting arguments.
}
\examples{
data(dsst)
class(dsst)
dsst
str(dsst)
}
\keyword{classes}
