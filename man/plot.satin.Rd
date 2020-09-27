\name{plot.satin}
\alias{plot.satin}
\alias{plot,satin,missing-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Visualise ocean data
}
\description{
Visual representation of ocean data from satellite images (MODIS, AVHRR, etc.) or derived from models (ocean productivity; CMEMS).
}
\usage{
\method{plot}{satin}(x, period = 1, depth = 1, xlim = NULL, ylim = NULL, zlim = NULL, 
           map = NULL, map.col = "grey", map.outline = "black", scheme = "default",
           col.sep = 0.1, colbar = TRUE, main = NULL, main.pos = "topright",
           log = FALSE, units = NULL,  xaxt = "s", yaxt = "s", atx = NULL, aty = NULL,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a satin object as returned by  \code{\link{read.nasaoc}}, \code{\link{read.ghrsst}}, \code{\link{read.osunpp}}, or \code{\link{read.cmems}}. }
  \item{period}{ an integer referring to the image number to plot. }
  \item{depth}{ an integer referring to a depth level. Only for Copernicus data that includes variables at different depths. }
  \item{xlim}{ minimum and maximum longitude values for the map. }
  \item{ylim}{ minimum and maximum latitude values for the map. }
  \item{zlim }{ minimum and maximum ocean data  values. }
  \item{map}{ a user defined map of class "SpatialPolygons". }
  \item{map.col}{ color for the map. }
  \item{map.outline}{ color for the map outline. }
  \item{scheme}{ color scheme for the ocean data, either: "default" or a vector of valid color names.}
  \item{col.sep}{ separation between colors in data units. }
  \item{colbar}{ logical. If \code{TRUE} a color bar is added to the map.  }
  \item{main}{ title for the map, defaults to the corresponding selected image \code{period$tmStart} in \code{x}. }
  \item{main.pos}{ position for the title: "topright", "topleft", "bottomright" or "bottomleft".  }
  \item{log}{ logical. If \code{TRUE}, the ocean variable is log transformed (base 10). This is useful for chlorophyll concentration data.}
  \item{units}{ color bar label, defaults to \code{units} in \code{x}. }
  \item{xaxt, yaxt}{ "n" to supress axis. }
  \item{atx, aty}{ specify tick-mark locations for fine tuning axes annotations. }
  \item{\dots}{ further arguments to pass to \code{\link{plot}}. }
}
\details{
The only mandatory argument for this function to produce a map is an object of class "satin" (\code{x} in this case).  All the other arguments are optional and can be used to customize the plot.  For chlorophyll concentration data, when \code{log = TRUE}, the color scale is fixed to a maximum value of 20, which implies that values above it are set to this maximum before applying logarithms.
}
\value{
 A plot including a map, of the corresponding parameter is produced.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{ Héctor Villalobos }
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{read.nasaoc}}, \code{\link{read.ghrsst}}, \code{\link{read.osunpp}}, and \code{\link{read.cmems}} %% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# SST data (Aqua Modis)
library(sp)
data(dsst, dmap) #load sample data and map

# default plot
plot(dsst)

# adding a custom map
plot(dsst, map = dmap)

# Chl-a concentration data (Aqua Modis) in actual units and in logarithmic scale
data(dchl)
plot(dchl, map = dmap, xlim = c(-120, -105), ylim = c(20, 35))
dev.new()
plot(dchl, map = dmap, xlim = c(-120, -105), ylim = c(20, 35), log = TRUE)  

# Copernicus data
data(dcmems)
plot(dcmems$thetao, map = dmap, period = 2, depth = 5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
