\name{xtsdfn}
\alias{xtsdfn}
\alias{is.xtsdfn}
\alias{as.xtsdfn}
\title{ Create Or Test For An xtsdfn Time-Series Object }
\description{
Constructor function for creating an extensible time-series data.frame object.

\code{xtsdfn} is used to create an \code{xtsdfn} object from raw data inputs and \code{as.xtsdfn} is used for coercion to the same.
}
\usage{

as.xtsdfn(x, ...)

is.xtsdfn(x)
}

\arguments{
  \item{x}{An object to be tested or coerced to class \code{xtsdf}.}
}
\details{
An \code{xtsdfn} object creates a time ordered \code{data.frame} like object; internally it is implemented as a list of \code{xts} objects, but it should behave like a \code{data.frame} rather transparently. The class is still under heavy development and feedback is welcome. Imporantly, this now allows \code{xts}-like behavior for objects with different column classes.



Subsetting inherits ISO8601 subsetting from \code{xts} as well as standard R subsetting.
}
\value{
An S3 object of class \code{xtsdfn}.
}

\author{Anton Samoylov}

\seealso{ \code{help("xts")} }
\examples{
data(sample_matrix)

xtsdfn.obj <- as.xtsdfn(as.xts(sample_matrix, dateFormat = "Date"))

class(xtsdfn.obj)

## basic POSIXct subset
xtsdfn.obj[as.POSIXct("2007-01-03", "GMT"), ]

## date range subset
xtsdfn.obj["2007-01-03/2007-02-01", ]

## March 2007
xtsdfn.obj["2007-03", ]

## From the start of the data set to 13th March of 2007
xtsdfn.obj["/2007-03-13", ]

## column subsetting
xtsdfn.obj[1:10, c("Open", "Low")]

## column logical subsetting
xtsdfn.obj[1:10, c(TRUE, FALSE, FALSE, TRUE)]

}
\keyword{ utilities }
