\name{satinPalette}
\alias{satinPalette}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Define custom color palettes for echograms
}
\description{
This function allows to design and visualise color palettes to be used in ocean data plots. 
}
\usage{
satinPalette(zmin, zmax, col.sep = 0.1, scheme = "default", visu = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{zmin}{lower limit in data units.}
  \item{zmax}{upper limit in data units.}
  \item{col.sep}{separation between colors in data units.}
  \item{scheme}{color scheme, either: "default" or a vector of valid color names.}
  \item{visu}{logical. If \code{TRUE}, a visual representation of the palette is displayed.}
}
\details{This function is mainly intended to be called by \code{\link{plot.satin}}, however it is possible to use it directly in order to have a first impression of a custom color palette.}
\value{A list with two elements
  \item{palette}{a vector of colors.}
  \item{breaks}{a vector of color breaks.}
}
\author{
Héctor Villalobos
}

\seealso{
\code{\link{plot.satin}}
}
\examples{
satinPalette(zmin = 10, zmax = 35, visu = TRUE)
satinPalette(zmin = 10, zmax = 35, col.sep = 0.2, scheme=c("white", "blue", "black"), visu = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ color }

