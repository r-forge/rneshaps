\name{aterpfun}
\alias{aterpfun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to interpolate from x,y,z grid to x,y, points.
}
\description{
  Use by aterp() to interpolate from a grid of concentration values to
  arbitrary x,y locations. Uses a fortran routine for interpolating.
  Not normally called by users.
}
\usage{
aterpfun(xyz.inp, xy.out, info = FALSE, use.original = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xyz.inp}{
    A list whose elements include \code{x,y,z} with structure suitable for plotting
    with \code{image()} or \code{fields::image.plot()}.  \code{x} and \code{y} are
    vectors of unique coordinates and \code{z} is a matrix of values at
    those locations. The \code{z} values are considered to be concentrations.
}
  \item{xy.out}{
    A \code{list(x,y)} of coordinates at which interpolated
    values are desired. }
  \item{info}{
If TRUE, include additional information in the return value.
}
  \item{use.original}{
An option to use an earlier version of fortran. Not advised.
}
}
\details{
  Restructures the input (xyz.inp) and creates additional vectors and
  matrices to pass to the fortran (aterp.f). The lengths of xyz.inp$x
  and xyz.inp$y can be a most 800, since this is hardcoded in the
  fortran. Therefore, xyz.inp$z can have at most 640000 elements (also
  hardcoded in the fortran).
}
\value{
  When info is FALSE, return a list
  \item{x}{xy.out$x}
  \item{y}{xy.out$y}
  \item{z}{interpolated values at x,y}

  When info is TRUE return additional variables internal to the fortran.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Don MacQueen (macqueen1@llnl.gov)
}
\note{
aterpfun() should not need to be called by the user.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{aterp}}, \code{\link{closest}}.
}
\examples{
%% ## The aterpfun() function is not normally called by the user.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
