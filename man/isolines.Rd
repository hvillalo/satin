\name{isolines}
\alias{isolines}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Interpolate isolines from ocean data }
\description{
User defined isolines are interpolated from ocean data stored in a satin object and returned as polylines.
}
\usage{
isolines(X, levels, period = 1, depth = 1, plot = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{a satin object as returned by \code{\link{read.nasaoc}}, \code{\link{read.ghrsst}}, \code{\link{read.osunpp}}, or \code{\link{read.cmems}}. }
  \item{levels}{a vector of desired isolines in data units. }
  \item{period}{ an integer referring to the image number to process. }
  \item{depth}{ an integer referring to a depth level. Only for Copernicus data that includes variables at different depths. }
  \item{plot}{ logical, if \code{TRUE} a plot is produced (the default). }
}
\details{
Polyline coordinates of desired isolines are calculated via \code{contourLines} and \code{convCP} (from \pkg{PBSMapping} package).  Perhaps only interesting with sea surface temperatures (isotherms).  For smoother isolines, the function \code{\link{pixelate}} can be used to spatialy re-scale the data.  In order to use this function with Copernicus data returned from \code{\link{read.cmems}}, the appropriate indexing should be previously done.
}
\value{
A list with two data frames of class \code{"PolySet"} and \code{"PolyData"}. See \code{help(PolySet)} and \code{help(PolyData)} for a complete description.
}
\author{ Héctor Villalobos, from code in examples of \pkg{PBSMapping} package}
\references{
Schnute, J. T., Boers, N. M., Haigh, R. and Couture-Beil, A. 2011 PBS Mapping 2.62: user's guide revised from Canadian Technical Report of Fisheries and Aquatic Sciences 2549:vi + 112 p. Last updated Feb 29, 2011.
}
\seealso{ \code{\link{contourLines}}, \code{\link{convCP}}, \code{\link{PolySet}} }
\examples{
library(PBSmapping)
data(dsst)

# spatial re-scaling of the sst data for smoother isotherms
sst <- pixelate(dsst, extent = 0.2) 
isotherms <- isolines(sst, levels = c(13, 17, 21))
plot(dsst, colbar = FALSE)
addLines(isotherms$PolySet, col = "black", lwd = 1.5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ manip }% __ONLY ONE__ keyword per line