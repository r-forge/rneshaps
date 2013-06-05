\name{fill.xy}
\alias{fill.xy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Fill a path with intermediate points along the path.
}
\description{
  Given a list(x,y) that represents a path that is defined by a sequence
of connected line segments, fill the path with additional points along
the path.
}
\usage{
fill.xy(xy, nfill = 20)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xy}{
A list with numeric elements x,y. Additional elements will be ignored. x
and y should be of the same length.
}
  \item{nfill}{
The number of intervals to insert in each segment.
}
}
\details{
Since nfill is the number of intervals to insert, the number of points
inserted will be nfill-1.

The intended use of fill.xy is to support the calculation of
concentration profiles as a function of distance along at path. For
example, the fenceline around a facility is a path, and this function is
used to help calculate fenceline concentrations, and find the maximum
concentration along the fenceline.

To do this, the output of this function is used as input to aterp (the 
\code{xy} argument), and as input to the \code{fill.xy} function. Then
the  "z" ouput from aterp() can be plotted relative to
the cumulative distances calculated by dist.xy(). Therefore, nfill
should be made large enough that the plot will be smooth.

If xy$x and xy$y do not have the same length, the longer of them will be
truncated.
}
\value{
  A list with elements x,y
  \item{x }{Vector of x coordinates}
  \item{y }{Vector of y coordinates}
  The input points are included.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Don MacQueen
}
\note{
  This function is intended for internal use by aterp().

  Since each segment is filled in with the same number of points, and the
  segments might not be the same length, the spacing of points is likely
  to be different in different segments.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{dist.xy}},\code{\link{aterp}},\code{\link{locator}},\code{\link{approx}}
}
\examples{
tmp <- list(x=1:3, y=c(1,4,5))
plot(tmp)
foo <- fill.xy(tmp,5)
points(foo,col=2,pch=3)

data(capsrcs)
acon <- getcon( file.path(.libPaths(),'Rneshaps','AAAAAM.CON'),
   site='Pnt 1',
   source.coords=capsrcs)
acong <- cap.i(acon)
image(acong, asp=1)

## by default, aterp calls locator
pth <- aterp(acong)
plot(pth, xlab='Distance along path',ylab='log10 concentration')

## x,y, elements of pth are the same as in pth$path.filled
points(pth$path.filled, cex=0.75, pch=3, col='red')

## build the path profile manually, instead of letting aterp() do the work
require(fields)
image.plot(acong, asp=1)
tmp <- locator(type='l')

## suppress aterp's default behavior of filling the path
tmpi <- aterp(acong,tmp,nfill=0)
points(tmpi$points)
plot(tmpi$path)

## since nfill was 0, the filled path elements are NULL
print(tmpi$path.filled)
print(tmpi[c('x','y')])

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
