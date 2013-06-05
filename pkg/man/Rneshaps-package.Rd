\name{Rneshaps-package}
\alias{Rneshaps-package}
\alias{Rneshaps}
\docType{package}
\title{
Functions for working with CAP88 PC version 1 concentration output files.
}
\description{
Basic capabilities include loading a CON file into R and assigning it to
real-world coordinates, combining two sets of output that have different
distances but otherwise use the same model parameters, and adding model
output from different source locations. CON files should be for a single
radionuclide only.
}
\details{
  This package was developed using concentration (.CON) output files from CAP88 PC Version
  1.0. It should work with later versions, provided the concentration
  output file structure is the same.

  The basic process is to load concentration output into R, interpolate
  onto a rectangular grid, and visualized. Output from more than one
  source can be added if interpolated onto a common grid.
}
\author{
Don MacQueen
Maintainer: Don MacQueen <macqueen1@llnl.gov>
}
\references{
  http://www.epa.gov/radiation/assessment/CAP88 (accessed 2013-05-15)
}
%%~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
\keyword{ package }
% \seealso{
% ~~ Optional links to other man pages, e.g. ~~
% ~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
% }
\examples{
data(capsrcs.ex)
acon <- getcon( file.path(.libPaths(),'Rneshaps','AAAAAM.CON'),
   site='Pnt 1',
   source.coords=capsrcs.ex)
acong <- cap.i(acon)
image(acong, asp=1)
require(fields)
image.plot(acong, asp=1)
title(acong$site)
## suggest starting a second graphics device
aterp(acong, plot.it=TRUE)

%% ~~ need at least two more example input put files to illustrate
%% cap.add() and cap.join()
}
