\name{anomaly}
\alias{anomaly}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Anomalies of a satin object
}
\description{
This function returns ocean data anomalies of a satin object.  Anomalies are calculated subtracting a conforming average satin object.
}
\usage{
anomaly(X, Y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
 a satin object obtained with \code{\link{satinMean}} for which anomalies are to be calculated. It must have identifying \code{labels} in the slot \code{attribs}.
}
  \item{Y}{
 also a satin object returned by \code{\link{satinMean}} with the means to be subtracted from \code{X}. It must have the same \code{labels} as \code{X}.
}
}
\details{
The anomalies of a satin object represent, for every pixel, the values above and below a reference average.  A typical example can be monthly sea surface temperature anomalies for various years.  These are calculated subtracting the corresponding average month for the whole period (see example below).

In principle, anomalies can be calculated for other ocean variables and time periods, but is up to the user to determine if that makes sense.  For example, let say that \code{Z} is a satin object with daily chlorophyll-a concentration data for several years.  In order to calculate quarterly anomalies, we will need first to obtain \code{X <- satinMean(Z, "\%Y-\%qtr")} and \code{Y <- satinMean(Z, "\%qtr")}.
}
\value{
 An object of  class \code{"satin"} (see \code{\link{satin-class}} for details) where the third dimension in the data array accomodates the calculated anomalies.  An extra element (\code{labels}) is included in the slot \code{attribs} to identify the time period of the anomalies.
}
\author{
Héctor Villalobos and Eduardo González-Rodríguez
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{satinMean}} for calculating ocean data averages needed, and \code{\link{climatology}} for climatologies.
}
\examples{
\dontrun{
# Calculate monthly sea surface temperature from weekly data.
# sst contains 240 weekly images for five years.

# This will produce the 12 average months for the five years
sst.m <- satinMean(sst, by="\%m") 

# and here we have 60 monthly periods, 12 for each one of the five years
sst.ym <- satinMean(sst, by="\%Y-\%m") 

# monthly sea surface temperature anomalies
anom <- anomaly(X = sst.ym, Y = sst.m)

# the dimensions of the data slots in sst.ym and anom are the same.
dim(sst.ym@data); dim(anom@data)

# plots
op <- par(no.readonly = TRUE)
lab <- paste(month.name, rep(2014:2018, each=12))
for (m in 1:60){
  plot(anom, period = m, zlim=c(-10, 10), col.sep = 0.2, main = lab[m],
       scheme = c("blue", "cyan", "white", "yellow", "red"))
}
par(op)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{ array }
\keyword{ manip }
