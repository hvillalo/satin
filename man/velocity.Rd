\name{velocity}
\alias{velocity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Ocean currents velocities 
}
\description{
 Calculate ocean current speed and direction from eastward and northward components (Copernicus data).
}

\usage{
velocity(u, v, depth = 1) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{u}{ a satin object with eastward ocean current velocities (in m/s). }
  \item{v}{ a satin object with northward ocean current velocities (in m/s). }
  \item{depth}{ an integer referring to a depth level. }
}

\details{
This function calculates the resulting velocity vectors in the same way as the \code{"quiver"} function. In addition, these values are returned togheter with their direction in degrees.
}
\value{
 A list with 2 elements, ``speed'' and ``rhumb'', both of class \code{"satin"}. The former contains the resulting velocity vectors as drawn by \code{"quiver"}, while the second their direction in degrees.
}
\author{ Héctor Villalobos }
\examples{
if(interactive()){
# import copernicus data with potential temperatures and ocean current velocities
oc <- read.cmems("global-reanalysis-phy-001-030-daily_1560792767602_GC_2013-14.nc")

vel <- velocity(u = oc$uo, v = oc$vo)

# Plots overlaying velocity vectors 
plot(vel$speed)
quiver(u = oc$uo, v = oc$vo, main = "", length=0.02, scale = 0.7, add2map = TRUE)	   

plot(vel$rhumb, col.sep = 90, scheme = c('#d7301f', '#fc8d59','#fdcc8a', '#fef0d9'))
quiver(u = oc$uo, v = oc$vo, main = "", add2map = TRUE)
}
}
\keyword{ manip }