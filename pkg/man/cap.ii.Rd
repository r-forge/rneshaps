\name{cap.ii}
\alias{cap.ii}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interpolate two capcon objects onto a single common grid and adds their concentrations.
}
\description{
Takes the output of two capcon objects [created by getcon()], interpolates
them from their respective radial grids onto a common regular
(rectangular) grid, and then adds them. It is recommended to use
cap.add() instead.
}
\usage{
cap.ii(dat1, dat2, ngrid = 60, xlims = range(dat$x), ylims = range(dat$y), linear = TRUE, extrap = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat1}{
    An object created by getcon().
  }
  \item{dat2}{
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
of points. Uses the akima package function interp() to interpolate from
the radial grid to the new grid. Despite being radial, the radial grid
is defined in cartesian coordinates, and is therefore suitable input for
interp().

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
Don MacQueen
}
\note{
Use cap.add() instead.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  code{\link{getcon}},\code{\link{cap.i}},\code{\link{cap.add}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
