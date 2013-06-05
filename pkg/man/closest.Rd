\name{closest}
\alias{closest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Interpolate by finding the closest location
}
\description{
For each location in the first arg, find the closest location in the
second arg, and return the z value (concentration) at that location.
}
\usage{
closest(xyz.inp, xy.out, info = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  See ?aterp

  WARNING: this function has not been maintained in a long time, and
  should not be relied upon.
  
  \item{xyz.inp}{
%%     ~~Describe \code{xyz.inp} here~~
}
  \item{xy.out}{
%%     ~~Describe \code{xy.out} here~~
}
  \item{info}{
%%     ~~Describe \code{info} here~~
}
}
\details{
Intended as an alternative to aterpfun() in aterp(). Not used by default.
}
\value{
  See \code{\link{aterpfun}}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Don MacQueen (macqueen1@llnl.gov)
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{aterp}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
