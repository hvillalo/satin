\name{dmap}
\alias{dmap}
\docType{data}
\title{Map of northwest Mexico}
\description{
Map of northwest Mexico obtained from GSHHG database (v2.3.7).
}
\usage{data("dmap")}
\details{
This intermediate resolution map was imported from the Global Self-consistent, Hierarchical, High-resolution Geography Database by means of \code{Rgshhs} function from \pkg{maptools} package.
}
\source{
  \url{ftp://ftp.soest.hawaii.edu/gshhg/gshhg-bin-2.3.7.zip}
}
\references{
Wessel, P. and Smith, W. H. F. 1996 A Global Self-consistent, Hierarchical, High-resolution Shoreline Database, \emph{Journal of Geophysical Research}, \bold{101}, 8741--8743.
}
\seealso{ \code{\link{Rgshhs}} }
\examples{
library(sp)
data(dmap)
plot(dmap, xlim=c(-120, -105) , ylim=c(20, 35), xaxs="i", yaxs="i", 
     axes=TRUE, col="beige", lty=1, border="grey70"); box()
}
\keyword{datasets}
