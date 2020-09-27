\name{satinMean}
\alias{satinMean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Averaging ocean data by time period
}
\description{
This function allows to average ocean data by a selected time period.  For example, from daily data, the user may want to obtain weekly, monthly, quarterly or even yearly averages. 
}
\usage{
satinMean(X, by = "\%m", depth = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
 a satin object as returned by \code{\link{read.nasaoc}}, \code{\link{read.ghrsst}}, \code{\link{read.osunpp}}, or \code{\link{read.cmems}}. 
}
  \item{by}{
 a character string specifying the averaging time period, see details. 
}
  \item{depth}{
 an integer referring to a given  depth level (defaults to 1). Only meaningful for Copernicus data that includes variables at different depths.
}
}
\details{
Accepted strings in \code{by} follow the notation of conversion specifications described in \code{\link{strptime}}.  Currently implemented:  \code{"\%Y"}, \code{"\%m"}, \code{"\%Y-\%m"}, \code{"\%j"}, \code{"\%qtr"}, \code{"\%Y-\%qtr"}, \code{"\%sem"}, \code{"\%Y-\%sem"}, \code{"\%U"}, \code{"\%V"}, \code{"\%W"}, \code{"\%Y-\%U"}, \code{"\%Y-\%V"},  \code{"\%Y-\%W"}.  This includes two non-standard strings and their combinations with year: \code{"\%qtr"}, and \code{"\%sem"}, for quarter and semester, respectively, which are passed to lubridate's functions of the same names.  This function is meant to be used in combination with \code{\link{anomaly}} to obtain anomalies with respect to an average period.
}

\value{
 An object of  class \code{"satin"} (see \code{\link{satin-class}} for details) where the third dimension in the data array accomodates averages by time period.  An extra element (\code{labels}) is included in the slot \code{attribs} to identify the time period used for the averages.
}
\author{
Héctor Villalobos and Eduardo González-Rodríguez
}
\seealso{
\code{\link{anomaly}} for calculating anomalies of ocean data, and \code{\link{climatology}} for climatologies.
}
\examples{
if(interactive()){
sst.ym <- satinMean(sst, by = "\%Y-\%m") # sst is a satin object with 240 weekly
                                         # images, totalling five years of data.

sst.ym # this will have 60 time periods, one for every month and year.

sst.m <- satinMean(sst, by = "\%m") # this will produce 12 monthly periods
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{ array }
\keyword{ manip }
