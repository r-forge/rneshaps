\name{predict.caplist}
\alias{predict.caplist}
\title{
  Predicts CAP88 model concentrations at x,y locations
}
\description{
Takes a list of "capcon" objects and calculates a predicted
concentration at supplied x,y coordinates.
}
\usage{
predict.caplist(caplist, predict.at.xy = at.lm, coef = rep(1, length(caplist)), refgrid = ref.grid)
}
\arguments{
  \item{caplist}{
    A list of "capcon" objects created by getcon(). For convenience, use
    caplist() to create the list.
}
  \item{predict.at.xy}{
    A list or data frame with elements 'x' and 'y'; coordinates
    of points at which to predict. May instead be
    a SpatialPoints or SpatialPointsDataFrame object, in which
    case predictions will be at the coordinates of the spatial points.
    If NULL, the user will be prompted to choose points using locator().
}
  \item{coef}{
    Scaling factors for each capcon object in the caplist arg. The model
    concentrations will be multiplied by these before interpolating and adding.
}
  \item{refgrid}{
    A "capgrid" object created by capgrid(). Used for interpolation.
}
}
\details{
This function takes concentration output from one or more CAP88 model
runs, interpolates each onto a common grid, adds them, then interpolates
from the added values to the supplied x,y coordinates.

If predict.at.xy is NULL, assumes and requires that there is an active
graphics device on which the use can indicate locations using the mouse.
}
\value{
  A "cappred" object. Internally, a list.
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Don MacQueen
}
\note{
  Used by capfit()
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{getcon}} , \code{\link{caplist}} , \code{\link{cap.add}},
 \code{\link{aterp}}
}
\examples{

 data(capsrcs.ex)
     
 ## get output from two models centered at different locations
 cp1n <-  getcon( file.path(.libPaths(),'Rneshaps','AAAAAM.CON'), site='Pnt 1',  source.coords=capsrcs.ex)
 cp2n <-  getcon( file.path(.libPaths(),'Rneshaps','AAAAAK.CON'), site='Pnt 2',  source.coords=capsrcs.ex)

## create a "caplist" object
 cap.cons <- caplist(cp1n,cp2n)

refg <- cap.grid( cp1n, cp2n , ngrid=500)

predict.caplist( cap.cons, caplocs.ex, c(1,10), refg)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
