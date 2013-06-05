\name{cap.add}
\alias{cap.add}
\title{
Add the concentrations from multiple capcon objects onto a common grid.
}
\description{
Takes two or more capcon objects [created by getcon()], interpolates
each of them onto a common grid, then adds their concentrations at
the grid points. Returns a capfield object.
}
\usage{
cap.add(..., grid = ref.grid, linear = TRUE, extrap = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
Two or more capcon objects, or a list of two or more capcon objects.
}
  \item{grid}{
A 'capgrid' object created by cap.grid(). (The default value, ref.grid,
is for the package author's convenience.)
}
  \item{linear}{
Passed to akima::interp().
}
  \item{extrap}{
Passed to akima::interp().
}
}
\details{
If a facility has more than one source location, and CAP88-PC is run
separately for each of them, then no model by itself represents the
concentration at various receptor locations. They have to be added. See
the Note for more information.

Since CAP88-PC output is on a radial grid centered on the source
location, the model outputs can not be directly added. In order to add
them, it is necessary to interpolate from each of them to a common set
of locations, and then add. This function performs the interpolation and
addition, with the common set of locations defined by the cap.grid()
function.

\emph{Important:} the function takes log10(z) before interpolating, and
  returns values in log10 units. Currently this is hardcoded, and is not
  user-changeable. Note that other package functions assume (require)
  log10 units for their input.
}
\value{
  A list of S3 class "capfield" whose first three components are x,y,z,
  where the z values have been added from each input object.
  x and y are vectors, z is a matrix; together they are suitable for
  input to the image() and 
  fields::image.plot() functions.

Additional elements are copied from the input objects:
 \item{site }{Concatenated sites from input objects}
 \item{name }{Concatenated names from input objects}
 \item{offset }{rbind of the offsets from the input ojects}
 \item{ctrs }{The "centers" of the various sources (currently poorly structured)}
 \item{xr }{The xr element from the grid argument (x range)}
 \item{yr }{The yr element from the grid argument (y range)}
 \item{plim }{The plim element from the grid argument (x,y range)}
 \item{zr }{The range of added concentrations, in log10 units}

 The list is assigned an S3 class, "capfield".
}
\references{
  http://www.epa.gov/radiation/assessment/CAP88/index.html (accessed 2012-10-26)
}
\author{
Don MacQueen (macqueen1@llnl.gov)
}
\note{
The returned value is assigned an S3 class "capfield". Note, however,
that S3 classes do 
not enforce a particular structure; hence the list returned by this
function does not have all the same elements as the list returned by
cap.i(). But it does have the essential x,y,z elements for subsequent plotting.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{cap.grid}},\code{\link{getcon}}
}
\examples{
data(capsrcs)

## get output from two models centered at different locations
cp1n <-  getcon( file.path(.libPaths(),'Rneshaps','AAAAAM.CON'), site='Pnt 1',  source.coords=capsrcs)
cp2n <-  getcon( file.path(.libPaths(),'Rneshaps','AAAAAK.CON'), site='Pnt 2',  source.coords=capsrcs)

refg <- cap.grid( cp1n, cp2n , ngrid=800)

## interpolate to the common grid (refg) and add
cpi <- cap.add(cp1n, cp2n, grid=refg)

## alternate usage
cpi.alt <- cap.add(list(cp1n, cp2n), grid=refg)

## should be TRUE
all.equal(cpi, cpi.alt)

## plot
image(cpi, asp=1)

require(fields)
image.plot(cpi, asp=1)

## will load the fields package if possible
plot(cpi)

## overlay the radial grids from each model
## see that they are added only where they overlap
image(cpi, asp=1)
points(cp1n$x, cp1n$y, cex=0.5, pch=3)
points(cp2n$x, cp2n$y, cex=0.5, pch=3)

## show the centers
points(cp1n$offset['x'],cp1n$offset['y'], pch=20, cex=1.5)
points(cp2n$offset['x'],cp2n$offset['y'], pch=20, cex=1.5)

## compare the concentration ranges
##  concentrations stored in data frame
range(cp1n$capdf$conc)
range(cp2n$capdf$conc)
##  concentrations stored in matrix
range(cp1n$cap)
range(cp2n$cap)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
