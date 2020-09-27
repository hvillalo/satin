\name{verticalProfiles}
\alias{verticalProfiles}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
CTD-like vertical profiles for Copernicus variables
}
\description{
Extract ocean data available at different depth levels (Copernicus data) and plot CTD-like vertical profiles.
}

\usage{
verticalProfiles(X, point = NULL, xlim = NULL, ylim = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{ a satin object with Copernicus data registered at different depths (i.e. potential temperature (thetao), salinity (so), etc.).}
  \item{point}{a data frame with point longitude and latitude coordinates where to extract data values.} 
  \item{xlim}{ limits for data variable. }
  \item{ylim}{ vertical depth limits. }  
%%     ~~Describe \code{X} here~~
}
\details{
Selected Copernicus data should be available at several depths, otherwise is meaningless.  Also, if more than one point is selected, only the first will be processed.
}
\author{
Héctor Villalobos
}
\examples{
# load sample Copernicus data
data(dcmems)
# available variables
names(dcmems)

# plot potential temperature
plot(dcmems$thetao)

# define point for extracting and plotting vertical profile
pt <- data.frame(lon = -108.5, lat = 23.33)
vp <- verticalProfiles(dcmems$thetao, point = pt)
# inspect extracted data
vp
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }% use one of  RShowDoc("KEYWORDS")
