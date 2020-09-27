\name{crop}
\alias{crop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Cut a satin object
}
\description{
 Select and cut a polygon shaped area of interest from a satin object.
}
\usage{
crop(X, polygon = NULL, return.poly = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
  a satin object as returned by \code{\link{read.nasaoc}}, \code{\link{read.ghrsst}}, \code{\link{read.osunpp}}, or \code{\link{read.cmems}}.  
}
  \item{polygon}{
  a polygon defining the area of interest (aoi) to be cutted.
}
\item{return.poly}{
  if TRUE, \code{crop} returns a list with the cutted satin object and the polygon drawn (a data frame).
}
}
\details{
If provided, the polygon defining the aoi must have two columns with x and y coordinates.  The function verifies that the last point is exactly the same as the first one.  When not specified, the user can draw a polygon by clicking on a previously displayed image.  Note that for drawing the polygon in this manner, the plot of the satin object has to be created using \code{colbar = FALSE}.  The result is a satin object where the pixels outside the aoi are set to NA.  In order to cut Copernicus data returned from \code{\link{read.cmems}}, the appropriate indexing should be previously done.
}
\value{
A cutted satin object.

When \code{return.poly = TRUE}, a list with two components:
 \item{aoi}{cutted satin object.}
 \item{polygon}{polygon coordinates defining the aoi.}
}
\author{
 Héctor Villalobos
}
\seealso{
 \code{\link{getpoly}} 
}
\examples{
# load and display sample data
data(dsst)
plot(dsst)
  
# define polygon, cut the image and display the aoi
polyg <- data.frame(x = c(-110, -105, -113, -116, -110), y = c(23, 23, 32, 32, 23))
dsst.clip <- crop(dsst, polygon = polyg)
plot(dsst.clip)

if(interactive()){ 
# the same, but polygon is defined by clicking on the image
plot(dsst, colbar = FALSE)
dsst.clip2 <- crop(dsst, return.poly = TRUE) # *** draw the aoi polygon ***
dsst.clip2$polygon # show polygon coordinates
plot(dsst.clip2$aoi) # plot the selected aoi
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ manip }% __ONLY ONE__ keyword per line
