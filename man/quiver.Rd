\name{quiver}
\alias{quiver}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Vectors of ocean currents velocities 
}
\description{
 Plot vectors of ocean current velocities of varying size or color for Copernicus data.
}

\usage{
quiver(u, v, period = 1, depth = 1, xlim = NULL, ylim = NULL, scale = 1, 
       length = 0.05, colarrow = NULL, scheme = "default", ra.pos = NULL, 
       ra.speed = NULL, map = NULL, map.col = "grey", map.outline = "black", 
       colbar = FALSE, main = NULL, main.pos = "topright", add2map = FALSE, ...) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{u}{ a satin object with eastward ocean current velocities (in m/s). }
  \item{v}{ a satin object with northward ocean current velocities (in m/s). }
  \item{period}{ an integer referring to the image number to plot. }
  \item{depth}{ an integer referring to a depth level. }
  \item{xlim}{ minimum and maximum longitude values for the map. }
  \item{ylim}{ minimum and maximum latitude values for the map. }
  \item{scale}{ this argument controls the arrow length. }
  \item{length}{ this value controls the size of the arrow head. }
  \item{colarrow}{ color(s) for the arrows; see details. }
  \item{scheme}{ color scheme for the arrows. Either: "default" or a vector of valid color names.}
  \item{ra.pos}{ position (lon, lat) for a reference arrow. }
  \item{ra.speed}{ speed (in m/s) of the reference arrow. The mean is used by default.}
  \item{map}{ a user defined map of class "SpatialPolygons". }
  \item{map.col}{ color for the map. }
  \item{map.outline}{ color for the map outline. }
  \item{colbar}{ logical. If \code{TRUE} a color bar is added to the plot.  }
  \item{main}{ title for the map, defaults to the corresponding selected image \code{period}. }
  \item{main.pos}{ position for the title: "topright", "topleft", "bottomright" or "bottomleft".  }
  \item{add2map}{ a logical value. If \code{FALSE} an entire new plot is created. If \code{TRUE} ocean current velocity arrows are added to an existing plot. }
  \item{\dots}{ further arguments to pass to \code{arrows}. }
}

\details{
Usage and behavior of this function closely resembles that of \code{plot.satin}.  It differs in that two different satin objects are needed, one for each component of ocean current velocities.  Also, the ocean current vectors can be overlayed to an existing plot of another variable, as the temperature, for instance.
}
\value{
 A map of the ocean current velocities is produced.
}
\author{ Héctor Villalobos }
\examples{
if(interactive()){
# import copernicus data with potential temperatures and ocean current velocities
oc <- read.cmems("global-reanalysis-phy-001-030-daily_1560792767602_GC_2013-14.nc")

# Default plot
quiver(u = oc$uo, v = oc$vo)

# Using default colors for the arrows, color bar and reference arrow
quiver(u = oc$uo, v = oc$vo, colarrow = TRUE, colbar = TRUE,
       ra.pos = c(-108, 30))

# Adding custom map and colors, and plotting another period and depth level
quiver(u = oc$uo, v = oc$vo, image = 4, depth = 10, colbar = TRUE,
       colarrow = TRUE, scheme = c("mediumblue", "lightcyan", "red"), 
       map = dmap, ra.pos = c(-108, 30), ra.speed = 0.2)


# Overlay arrows to an existing plot of the potential temperature
plot(oc$thetao, map = dmap, xlim = c(-112, -108), ylim = c(24, 28), 
     colbar = FALSE)
quiver(u = oc$uo, v = oc$vo, colarrow = TRUE, ra.pos = c(-108.5, 27), 
       ra.speed = 0.15, main = "", length=0.02, scale = 0.7, add2map = TRUE)	   
}
}
\keyword{ hplot }



