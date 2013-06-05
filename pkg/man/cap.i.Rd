\name{cap.i}
\alias{cap.i}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interpolate a capcon object onto a grid.
}
\description{
Takes the output of getcon(), a list of class "capcon", and interpolates
the concentrations from the radial grid onto a regular (rectangular)
grid. Output structure is suitable for input to functions image or
fields::image.plot. 
}
\usage{
cap.i(dat, ngrid = 60, xlims = range(dat$x), ylims = range(dat$y),
linear = TRUE, extrap = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
    An object created by getcon().
  }
  \item{ngrid}{
    The number of grid points to use in each direction.
  }
  \item{xlims}{
    (optional) The extent of the grid in the x dimension.
  }
  \item{ylims}{
    (optional) The extent of the grid in the y dimension.
  }
  \item{linear}{
    Passed to akima::interp().
  }
  \item{extrap}{
    passed to akima::interp()
  }
}

\details{
Based on the values of ngrid, xlims, and ylims, creates a regular grid
of points and interpolates concentrations from the input onto the
regular grid. Uses the akima package function interp() for
interpolation. The radial grids in "capcon" objects are stored in
cartesian coordinates, and are therefore suitable input for interp().

\emph{Important:} the function takes log10(z) before interpolating, and
  returns values in log10 units. Currently this is hardcoded, and is not
  user-changeable. Note that other package functions assume (require)
  log10 units for their input.
}
\value{
A list whose first three components are x,y,z, directly from interp(). The list
is given an S3 class, "capfield".

Additional elements are copied from the input object:
 \item{site }{User supplied site name.}
 \item{name }{Information from CON file header lines}
 \item{date }{Information from CON file header lines}
 \item{windfile }{Windfile name from CON file headerlines}
 \item{offset }{Offset of center relative to 0,0 (i.e., real-world
   coordinates of model center if the site found in source.coords$Location)}
 \item{measure }{"Conc" for concentration file input}
}
\references{
  http://www.epa.gov/radiation/assessment/CAP88/index.html (accessed 2012-10-26)
}
\author{
Don MacQueen (macqueen1@llnl.gov)
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  code{\link{getcon}},\code{\link{cap.ii}},\code{\link{cap.add}}
}
\examples{
data(capsrcs.ex)
acon <- getcon( file.path(.libPaths(),'Rneshaps','AAAAAN.CON'),
   site='Pnt 1',
   source.coords=capsrcs.ex)
acong <- cap.i(acon)
image(acong, asp=1)

require(fields)
image.plot(acong, asp=1)

## will load the fields package if possible
plot(acong)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
