\name{dist.xy}
\alias{dist.xy}
\title{
Calculate the cumulative distance along a path.
}
\description{
Given a list(x,y) that represents a path that is defined by a sequence
of connected line segments, calculate the cumulative distance along the path.
}
\usage{
dist.xy(xy)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xy}{A list with elements x,y. x and y should be numeric vectors of the
    same length.
}
}
\details{
If xy$x and xy$y do not have the same length, the longer of them will be truncated.
}
\value{
A numeric vector of cumulative distances along the path.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Don MacQueen
}
\note{
  This function is used internally by aterp().

  locator() is a good way to create the path.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{fill.xy}},\code{\link{aterp}},\code{\link{locator}}
}
\examples{
   tmp <- list(x=c(1,2,7), y=c(1,4,5))
   dist.xy(tmp)

   foo <- fill.xy(tmp,3)
   dist.xy(foo)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
