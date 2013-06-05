\name{capsrcs.ex}
\alias{capsrcs.ex}
\docType{data}
\title{
Example data frame of air dispersion model source information
}
\description{
Example data frame of air dispersion model source information
}
\usage{data(capsrcs.ex)}
\format{
  A data frame with 5 observations on the following 4 variables.
  \describe{
    \item{\code{Location}}{Location names}
    \item{\code{x}}{Location east-west coordinate}
    \item{\code{y}}{Location north-south coordinate}
    \item{\code{type}}{Type of source}
  }
}
\details{
This is an example of a data frame to be supplied to the getcon()
function 'source.coords' argument. The 'site' argument to getcon() must
be one of the location names.

The locations in this data frame and in the data frame of sampler
locations (see for example \link{caplocs.ex}) must be in the same
coordinate system.
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(capsrcs.ex)
}
\keyword{datasets}
