\name{getcon}
\alias{getcon}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Function to read a CAP88PC version 1 concentration output file into R.
}
\description{
  This function reads a CAP88PC CON file into R and organizes the
  concentration values into a structure suitable for further processing
  with other package functions.
}
\usage{
getcon(infile = "path.to.file", site = "", source.coords = srcs.lm,
site.var = "Location", convert.to.dose = FALSE,
dose.to.conc = 134.7 * 0.37, to.bq=FALSE, datadir = NULL, infodir = NULL)
}
\arguments{
  \item{infile}{
    Full path to the CON file.
  }
  \item{site}{
    (optional) A name for the site (see the source.coords argument). See
    details. 
  }
  \item{source.coords}{
    (optional) A data frame or SpatialPointsDataFrame containing locations
    and their coordinates.  If a data frame, must have variables named
    x and y. See details. 
  }
  \item{site.var}{
    (optional) The name of the variable in source.coords that contains site
    names. Defaults is "Location".
  }
  \item{convert.to.dose}{
    If TRUE, convert concentrations to doses using the conversion factor in
    the \code{dose.to.conc} argument.
  }
  \item{dose.to.conc}{
    Factor for converting concentration to dose.
  }
  \item{to.bq}{
    Set TRUE to multiply concentrations by 0.037. Valid only if model
    output is in pCi.
  }
  \item{datadir}{
    (optiona) If supplied, identifies the director that the infile is found
    in. In this case, infile should contain the file name only, without the
    path. This argument is retained for backward capability with scripts
    that used previous versions of this package.
  }
  \item{infodir}{
    (optional) If supplied, limited information from the CON file will be
    written to files in this directory.
  }
}

\details{
  A CAP88PC "CON" output file consists consists of a number of header rows
  followed by a data frame-like table of model output. The output file
  is paginated according to legacy fortran rules (such as ^L for a page
  break), and column headers are repeated on each page. For an example
  CON file see
    file.path(.libPaths(),'Rneshaps','AAAAAM.CON').

  Model output is calculated (by default) on a radial grid of 16
  directions and up to 20 distances in each direction, specified by the
  user in the CAP88PC GUI. At each location, the output file includes a
  concentration value and some other model output. getcon() reads only
  the concentration values. The radial grid is centered on (0,0).

  After parsing the file and extracting the concentrations, the model
  values are organized into a matrix and stored in the 
  $cap element of the list that is (invisibly) returned. The matrix has
  16 rows for the 16 compass directions and as many columns as
  distances. The associated x and y coordinates are in the $x and $y 
  elements respectively. x, y, and cap are the main elements used in
  subsequent process [i.e., by functions such as cap.i()].

  If a site name and data frame of sites is supplied, the results will
  be shifted to center on the coordinates of the site provided in the
  data frame (a SpatialPointsDataFrame may be supplied instead).
  
}
\value{
  A list with S3 class "capcon".
  (currently there are no S3 methods defined)
 \item{dirs }{Standard compass directions}
 \item{dists }{Radial distances in the CON output}
 \item{ndir }{Number of directions}
 \item{ndist }{Number of distances}
 \item{dmat }{Matrix of concentrations; rows=distances, column=directions}
 \item{ddf }{Data frame of concentrations; rows=distances, column=directions}
 \item{info }{Character vector of information from the CON file header lines}
 \item{convert.to.dose }{Copy of arg conver.to.dose}
 \item{site }{User supplied site name.}
 \item{name }{Information from CON file header lines}
 \item{date }{Information from CON file header lines}
 \item{windfile }{Windfile name from CON file headerlines}
 \item{desc }{Information from CON file header lines}
 \item{x }{Matrix of x coords of each of the ndir*ndists locations; rows=directions,
   columns=distances}
 \item{y }{Matrix of y coords of each of the ndir*ndists locations; rows=directions,
   columns=distances}
 \item{compass }{The compass directions (again)}
 \item{angle }{The angle of each compass direction, relative to 0 =
   east, going counter-clockwise}
 \item{dist }{Radial distances (again)}
 \item{cap }{Matrix of concentration values, rows=directions,
   columns=distances (i.e., transposed of dmat)}
 \item{measure }{"Conc" for concentration file input}
 \item{offset }{Offset of center relative to 0,0 (i.e., real-world
   coordinates of model center if the site found in source.coords$Location}
}
\references{
  http://www.epa.gov/radiation/assessment/CAP88/index.html (accessed 2013-05-15)
}
\author{
Don MacQueen
}
\section{Warning}{
Use with later versions of CAP88 has not been evaluated, but should be
ok if concentration output files (.CON) have the same structure.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

data(capsrcs.ex)
acon <- getcon( file.path(.libPaths(),'Rneshaps','AAAAAM.CON'),
   site='Pnt 1',
   source.coords=capsrcs.ex)
plot(acon$x, acon$y, asp=1)

aconi <- cap.i(acon)
class(aconi)
print(aconi)
plot(aconi)

image(aconi, asp=1)

require(fields)
image.plot(aconi, asp=1)
title(aconi$site)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
