\name{aterp}
\alias{aterp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Interpolate concentrations from a CAP88-PC capfield object.
}
\description{
This function interpolates from concentrations on a grid to specified
locations. The locations may be considered arbitrary, or they may be
treated as a path within the grid area. In the latter case, the profile
of concentrations along the path is calculated. Default is to use locator() to
identify locations and consider them to be a path.
}
\usage{
aterp(xyz.inp, xy = NULL, nfill = NULL, nfillmin = 25, logz = TRUE,
out = TRUE, info = FALSE, use.aterp = TRUE,
locator.type = "l", xlab = "Distance on path",
ylab = xyz.inp$measure, plot.it = FALSE, type = "l", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xyz.inp}{
    A "capfield" object created by cap.i() or cap.add().
    A list whose elements include \code{x,y,z} in a structure suitable for plotting
    with \code{image()} or \code{fields::image.plot()}.  \code{x} and \code{y} are
    vectors of unique coordinates and \code{z} is a matrix of values at
    those locations. The \code{z} values are considered to be concentrations.
  }
  \item{xy}{
    If supplied, a \code{list(x,y)} of coordinates at which interpolated
    values are desired. If not supplied, the function will prompt the user
    to choose coordinates using \code{locator(type=locator.type,col=2)}. The latter
    identifies a path along which to interpolate.
  }
  \item{nfill}{
    When interpolating on a path, fill the lines between the points with
    this many points. This smooths the plotted concentration profile along
    the path. If \code{NULL} (the default), see \code{nfillmin}.

    Set nfill=0 to treat the input locations as arbitrary, rather than
    as a path.
  }
  \item{nfillmin}{
    When \code{nfill} is \code{NULL}, calculate \code{nfill} based on \code{nfillmin}.
  }
  \item{logz}{
    If TRUE return results in log10 units (after interpolating).
    if FALSE return results in original units (after interpolating).
  }
  \item{out}{
    If TRUE use \code{assign()} to assign \code{xy.out, xyf.out, xya.out,
      and xyp.out} to .GlobalEnf.
  }
  \item{info}{
    Passed to \code{aterpfun} to cause it to return extra information.
  }
  \item{use.aterp}{
    If TRUE use \code{aterpfun} to interpolate, otherwise use \code{closest}
    to return the nearest z value for each x,y pair.
  }
  \item{locator.type}{
    Passed to the \code{type} argument of \code{locator}.
  }
  \item{xlab}{
    x-axis label when plotting a path.
  }
  \item{ylab}{
    y-axis label when plotting a path.
  }
  \item{plot.it}{
    TRUE to plot an interpolated path.
  }
  \item{type}{
    Passed to \code{plot} when plotting a path.
  }
  \item{\dots}{
    Additional arguments to pass to \code{plot}.
  }
}
\details{
  Users should not normally call \code{aterpfun()} directly.
  Value is returned invisibly.
}
\value{
  \item{x }{Distance along path, or NULL if not on a path}
  \item{y }{Interpolated concentrations along path, in log10 units, or NULL}
  \item{xyz }{Is the input path \code{x,y} (not filled) with interpolated concentrations. A list with elements:}
  \item{xyz$x }{The input path x coordinates}
  \item{xyz$y }{The input path y coordinates}
  \item{xyz$z }{The interpolated concentrations, log10 units}
  \item{xyzfilled }{Like xyz, but using the filled path}
  \item{maxloc }{Location and log10(concentration) of the maximum along
  the filled path}
  \item{grspc }{The minimum distance between points on the filled path}
  \item{nfill }{The supplied or calculated nfill argument}
  \item{path }{A list(x,y) containing the path concentration profile,
    not filled}
  \item{path.filled }{A list(x,y) containing the filled path
    concentration profile (A copy of the x,y elements)}
  \item{points }{A list(x,y,z) if then input points and their
  interpolated concentrations (log10 units).}
  \item{points.filled }{Like points, but using the filled path.}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Don MacQueen
}
\note{
The $path element of the returned value is included primarily as a
  convenient name by which to refer to the concentration profile along
  the path.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(capsrcs.ex)
print(capsrcs.ex)
acon <- getcon( file.path(.libPaths(),'Rneshaps','AAAAAM.CON'),
   site='Pnt 1',
   source.coords=capsrcs.ex)
acong <- cap.i(acon)
image(acong, asp=1)
require(fields)
image.plot(acong, asp=1)
title(acong$site)

tmp <- aterp(acong)
points(tmp$points)

## suggest starting a second graphics device
plot(tmp$path.filled)
points(tmp$path, col='red')

## optionally immediately plot the path profile
image.plot(acong, asp=1)
aterp(acong, plot.it=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
