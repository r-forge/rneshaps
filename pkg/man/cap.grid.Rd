\name{cap.grid}
\alias{cap.grid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a grid that encompasses all of the supplied capcon objects.
}
\description{
Creates a "capgrid" object for use by cap.add().
}
\usage{
cap.grid(..., ngrid = 600)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
One or more capcon objects, or a list of capcon objects. (capfield
objects may also be supplied)
}
  \item{ngrid}{
The number of unique x and y values in the output grid.
}
}
\details{
  Defines the input needed for grid argument of the cap.add() function.
  The range of x values output is the range of all of the x values
  input; similarly for y values.

  In effect defines an ngrid by ngrid grid set of points; see the use of
  expand.grid() in the examples.
}
\value{
A list of values suitable for input to cap.add().
\item{x }{A vector of length ngrid encompassing all of the x values in
  the inputs}
\item{y }{A vector of length ngrid encompassing all of the y values in
  the inputs}
\item{xr }{The range of the x values}
\item{yr }{The range of the y values}
\item{plim }{The range of both x and y.}
}
\references{
  http://www.epa.gov/radiation/assessment/CAP88/index.html (accessed 2012-10-26)
}
\author{
Don MacQueen
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  %% ~~objects to See Also as \code{\link{help}}, ~~~
  \code{\link{getcon}},\code{\link{cap.add}}
}
\examples{
data(capsrcs.ex)

## get output from two models centered at different locations
cp1n <-  getcon( file.path(.libPaths(),'Rneshaps','AAAAAM.CON'), site='Pnt 1',  source.coords=capsrcs.ex)
cp2n <-  getcon( file.path(.libPaths(),'Rneshaps','AAAAAK.CON'), site='Pnt 2',  source.coords=capsrcs.ex)

cp1n$offset
cp2n$offset

## plot their radial grids
plot(cp1n$x, cp1n$y, asp=1, cex=0.5, xlim=range(cp1n$x, cp2n$x), ylim=range(cp1n$y, cp2n$y))
points(cp2n$x, cp2n$y, pch=3, cex=0.5, col='red')

## create a "reference grid" that overlays both of them
## ngrid should be 400 to 800 for useful applications
refg <- cap.grid( cp1n, cp2n , ngrid=5)

## overlay the reference grid on the radial grids
points(expand.grid(refg$x, refg$y), col='green', cex=0.75, pch=20)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
