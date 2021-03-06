\name{xts-package}
\alias{xts-package}
\docType{package}
\title{
xts: extensible time-series
}
\description{
Extensible time series class and methods, extending and behaving
like zoo.
}
\details{
\tabular{ll}{
Package: \tab xts\cr
Type: \tab Package\cr
Version: \tab 0.8-0\cr
Date: \tab 2011-02-22\cr
License: \tab GPL (>= 2)\cr
}
Easily convert one of \R's many time-series (and non-time-series)
classes to a true time-based object which inherits all of zoo's
methods, while allowing for new time-based tools where appropriate.

Additionally, one may use \pkg{xts} to create new objects which
can contain arbitrary attributes named during creation as name=value
pairs.
}
\author{
Jeffrey A. Ryan and Josh M. Ulrich

Maintainer: Jeffrey A. Ryan <jeff.a.ryan@gmail.com>
}
\keyword{ package }
\seealso{
   \code{\link{xts}}
   \code{\link{as.xts}}
   \code{\link{reclass}}
   \code{\link[zoo:zoo]{zoo}}
}
