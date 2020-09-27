\name{extractPts}
\alias{extractPts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract ocean data values from a satin object
}
\description{
Extract data values for pixels selected from an image of a satin object.
}
\usage{
extractPts(X, points = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{X}{a satin object as returned by \code{\link{read.nasaoc}}, \code{\link{read.ghrsst}}, \code{\link{read.osunpp}}, or \code{\link{read.cmems}}.
}
\item{points}{ a data frame with point(s) longitude and latitude coordinates where to extract data values. }
}
\details{
If provided, \code{points} must have two columns with x and y coordinates.  When not specified, \code{points} can be  selected by clicking on the image.  Note that for selecting the points in this manner, the plot of the satin object has to be created using \code{colbar = FALSE}.
}
\value{
A data frame with at least 7 columns: For each point, a sequential \code{id} number; (\code{x, y}) coordinates, either specified via the \code{points} argument or selected by clicking on the image; (\code{lon} and \code{lat}) coordinates of pixels closer to the desired points; and for quality control purposes, the distance between them (\code{d}, km).  The remaining column(s) are the parameter values for every image in the satin object.
}
\author{
 Héctor Villalobos
}
\examples{
# load data, define coordinates and extract values
data(dsst)

plot(dsst)
coord <- data.frame(x = seq(-130, -114, 2), y = seq(20, 36, 2))
values <- extractPts(dsst, points = coord)
values

# if no points are given the user can select them by clicking on the image
if (interactive()){
plot(dsst, colbar = FALSE)
values2 <- extractPts(dsst) # *** click on the image ***
values2
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ manip }% __ONLY ONE__ keyword per line
