\name{cap.join}
\alias{cap.join}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Join two or more capcon objects into a single capcon object.
}
\description{
This function joins the model output from two or more CAP88-PC runs. The
assumption is that the model runs use different sets of distances but
are otherwise identical. (Results are meaningless if this is not the case.)
}
\usage{
cap.join(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
Two or more "capcon" objects, that is, objects created by getcon().
}

Arguments may also be supplied in a list, for example
\code{cap.join(list(obj1,obj2))}.
}
\details{
Uses c(), rbind(), or cbind() as appropriate on the elements of each
input object to create the output object. Informational elements are
copied from the first object.

Results are sorted by distance if
necessary, and duplicate distances, if any, are removed.
}
\value{
  A list with the same elements as returned by getcon().
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
  \code{\link{getcon}}
}
\examples{

data(capsrcs)

## near field
cpn <- getcon( file.path(.libPaths(),'Rneshaps','AAAAAM.CON'),
       site='Pnt 1',  source.coords=capsrcs)
plot(cpn$x, cpn$y, asp=1)

## far field
cpf <- getcon( file.path(.libPaths(),'Rneshaps','AAAAAN.CON'),
       site='Pnt 1',  source.coords=capsrcs)
plot(cpf$x, cpf$y, asp=1)
## add the near field
points(cpn$x, cpn$y, pch=3, col='red')

## join near and far
cpd <- cap.join(cpn,cpf)
plot(cpd$x, cpd$y, asp=1)

## interpolate to prepare for image plots
cpni <- cap.i(cpn)
cpfi <- cap.i(cpf)
cpdi <- cap.i(cpd)

## near field
image(cpni, asp=1)

## check plot range
print( diff(par()$usr[1:2]) )

## image plot of far field by itself is misleading
## because it  interpolates across the missing near field data in the center
image(cpfi, asp=1)

## correct
image(cpdi, asp=1)

## check plot range
print( diff(par()$usr[1:2]) )

## simple checks
## > diff(range(cpni$x))
## [1] 4000
## > diff(range(cpfi$x))
## [1] 8000
## > diff(range(cpdi$x))
## [1] 8000


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
