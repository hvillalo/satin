\name{satinDataframe}
\alias{satinDataframe}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reshape a satin object into a data frame
}
\description{
A satin object is reshaped as a data frame with longitude, latitude and remote sensed data column(s).
}
\usage{
satinDataframe(X, reverse = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{ a satin object as returned by  \code{\link{read.nasaoc}}, \code{\link{read.ghrsst}}, \code{\link{read.osunpp}}, \code{\link{read.cmems}}, or a data frame produced with this function. }
  \item{reverse}{logical, \code{TRUE} is used to create a satin object from a data frame previously created with this function (see details).}
}
\details{
This is  mainly an utility function called by \code{\link{crop}}, \code{\link{extractPts}}, and \code{\link{pixelate}} functions.  However, it may be useful when the user wants to arrange the ocean data variables stored in a satin object as a data frame, with rows representing pixels whose longitude and latitude coordinates are in the  first two columns and their data values in the remaining column(s).  The attributes describing the data (name, units, period, etc.) are preserved.  Specifying \code{reverse = TRUE} reverses the operation.
}
\value{
When \code{reverse = FALSE}, a data frame with at least three columns: "x", "y" for longitude and latitude coordinates, and data for the variable(s).  When \code{reverse = TRUE} a satin object as produced.
}
\author{
 Héctor Villalobos
}
\examples{
# load sample SST data
data(dsst)

# reshape into a data frame
X <- satinDataframe(dsst)
head(X)

# reverse the operation
Y <- satinDataframe(X, reverse = TRUE)
Y
plot(Y)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
