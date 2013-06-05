\name{caplocs.ex}
\alias{caplocs.ex}
\docType{data}
\title{
Example data frame of ambient air sampling information for Rneshaps package
}
\description{
Annual average air concentrations, location names and coordinates.
}
\usage{data(caplocs.ex)}
\format{
  A data frame with 8 observations on the following 6 variables.
  \describe{
    \item{\code{locs}}{Location names}
    \item{\code{n}}{Number of ambient air sample results (i.e., 12 for
      monthly sampling)}
    \item{\code{x}}{Sampler location east-west coordinate}
    \item{\code{y}}{Sampler location north-south coordinate}
    \item{\code{tavg}}{}
    \item{\code{ravg}}{a numeric vector}
  }
}
\details{
This example includes the annual average calculated two different ways,
to illustrate the fact that the Rneshaps package user specifies the
column name of the averages.

The locations in this data frame and in the data frame of source
locations (see for example \link{capsrcs.ex}) must be in the same
coordinate system.
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(caplocs.ex)
}
\keyword{datasets}
