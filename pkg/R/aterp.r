## aterp()
## Wrapper function for aterpfun()

## LLNL-CODE-637312

## this function is a convenience function for preparing input
## for aterpfun()

## interpolate from a grid onto a set of arbitrary locations
## or onto a path through the grid area
## optionally plot the interpolated results

## aterpfun() does interpolation only
## aterpfun() uses a fortran routine to interpolate

## this function supplies some flexibility in types of
## input, e.g., interpolate on a path or on arbitrary
## x,y locations, and sets up to use aterpfun()

## 4/17/08 rename aterp()     to aterpfun()
##         rename aterp.pl()  to aterp()

## 6/20/10
## rename the arguments so that names make it obvious
## which is in input (interpolate from) and which is the
## output (interpolate to)
##    xy.out     was   xy0
##    xyz.inp    was   iout  

## 10/25/12
## minor cleanup, improve comments

## 12/6/12
## changed dist.xy() to return 0 when only one point is supplied
## using nfill makes no sense with only one point
## aterp() works with nfull=NULL and only one point, but will crash if nfill is supplied

## Given an input grid of x,y points with z values at each
## and a set of new x,y, points
## estimate a z value for each new x,y, location
## 
## Interpolate between x,y locations
## (see also closest.r)

## arguments are
##  xyz.inp    The data to interpolate from
##             A list with a structure suitable for use in image()
##             vectors x,y that define a grid
##             matrix z of values on that grid
##             e.g.:
##               list returned by cap.add()
##               list returned by interp()
##
##  xy    list(x=,y=)
##        points to interpolate to
##        corresponds to xyz.inp in aterpfun()
##
## logz   as of 4/18/08, cap.add() always and only returns values in log10 units
##        TRUE  return results in log10 units (after interpolating)
##        FALSE return results in original units (after interpolating)
##
## nfill  Fill in the path with this many points between the first and last
##        (or was it in each segment?)
##         Set nfill to 0 to not fill (do not use the input points as a path)

## there are two kinds of output we are interested in:
##
##   "path"
##      the input x,y pairs are interpreted as an arbitrary path
##      through x,y space, and we want to plot value as a function
##      of distance along that path (example, the site perimeter)
##
##   "points"
##      the input x,y pairs are arbitrary points and we just want the
##      values at those points. Example: sampling locations [see
##      function sta.on() for a source of such locations]
##      (nfill = 0)

##  output:
##  A list with elements:
##    x,y  "path" results; x is distance along path, y is values at those distances
##    xyz  A list(x , y , z) where x,y are the input locations, and z the interpolated
##         values at those locations
##    xyzfilled  The x,y pairs of the path, filled in with x,y pairs between them,
##         and their corresponding z values
##    maxloc  The x,y,z values corresponding to the max location along the path
##    grspc   The spacing between filled in points on the path, calculated when
##            nfill is NULL

## 10/29/12
## rework the ouput somewhat
## when input is a path, there are four kinds of output

##   (1) list(x,y,z) the input x,y points and the interpolated concentrations at them
##   (2) list(x,y)   path profile; the distances along the path and their interpolated concentrations

## (3) and (4) are like (1) and (2) but the path is filled
##   (3) list(x,y,z) the input x,y points and the interpolated concentrations at them
##   (4) list(x,y)   path profile; the distances along the path and their interpolated concentrations

## when input is arbitrary locations, not a path (set nfill to zero for this case)
## [really only want (1) above, but call it (5)
##   (5) list(x,y,z) the input x,y points and their interpolated concentrations

## call the output object "out"
##   (1) out$xyz

##   (2) out$path

##   (3) out$xyzfilled
##   (3) out$points.filled [a copy of out$xyzfilled, with an easier to remember name]

##   (4) out$x, out$y (y is actually a "z", but named y so that plot(out) plots the path profile)
##   (4) out$path.filled  = list(x=out$x, y=out$y) ; an easier name by which to refer to the filled path profile

##   (5) out$xyz
##   (5) redundant: out$xyzfilled, out$points, out$points.filled
##       out$x, out$y, out$path.filled$x, out$path.filled$y are all NULL

## we are assuming that the input data (xyz.inp) is on a fairly fine grid,
## the default is to fill in the path with points having approximately
## the same spacing. Hence, no need to adjust for curvature on the surface
## in order to improve interpolation.

## note that xyz.inp is the first arg, in order to make it easier
## to use the function when using locator() to choose points

## assumes that all points in xy are inside the input grid
## if xy$x and xy$y are not the same length, use the shorter

aterp <- function(xyz.inp,
                  xy=NULL,
                  nfill=NULL,
                  nfillmin=25,
                  logz=TRUE,
                  out=TRUE,
                  info=FALSE,
                  use.aterp=TRUE,
                  locator.type='l',
                  xlab='Distance on path',
                  ylab=xyz.inp$measure,
                  plot.it=FALSE,
                  type='l',
                  ...) {

  if (is.null(xy)) {
    cat('[aterp] Use locator to select points\nMiddle button when done\n')
    xy <- locator(type=locator.type,col=2)
  } 

  if (is.data.frame(xy)) {
    if (all( c('x','y') %in% names(xy))) {
      xy <- as.list(xy[,c('x','y')])
    } else stop('[aterp] xy must include elements "x" and "y"',call.=FALSE)
  }
  if (!is.list(xy)) stop('[aterp] Argument xy must be NULL or list(x=,y=)',call.=FALSE)

  ## note, no check for xy$x and xy$y being the same length
  npts <- length(xy$x)
  
  ## fill in additional points between the supplied points (in a straight line between each pair)
  ## (for a finer-grained interpolation)

  ## figure out how to calculate nfill from spacing of the input x,y values
  ## when nfill is null.
  grspc <- min( unique( diff( sort(xyz.inp$x))), unique( diff( sort(xyz.inp$y))) )
  if (is.null(nfill)) {
    ## add lengths of individual segments in the path
    tmpdx <- diff(xy$x)
    tmpdy <- diff(xy$y)
    pathlen <- sum(sqrt(tmpdx^2 + tmpdy^2))
    ## get grid spacing
    nfill <- floor(max(nfillmin, 1.2*pathlen/grspc ))
    ## 4/23/08 get problems when nfill is too large; until solved, prevent
    nfill <- min(nfill,75)
  } 
  
  ## need a way to make nfill exactly what I want it to be
  ## the code above, when nfill = NULL, is pretty good at automatic spacing
  ## so, comment out the line with max()
  if (nfill > 0 & npts > 1) {
    xyf <- fill.xy(xy,nfill=nfill)
  } else xyf <- xy
  
  ## when finding a z value for any given x,y pair, either
  ##  (1) take the closest, or (2) interpolate
  if (use.aterp) xya <- aterpfun(xyz.inp,xyf,info=info)
  else  xya <- closest(xyf,xyz.inp,info=info)

  if (is.character(xya) && xya=='Grid too large') stop(xya,call.=FALSE)

  ## 10/29/12
  ## profile along the unfilled path (normally, unfilled path is only a few points)
  ## redundant when nfill=0
  if (use.aterp) xyd <- aterpfun(xyz.inp,xy,info=info)
  else  xyd <- closest(xy,xyz.inp,info=info)
  xyd <- list(x=dist.xy(xy),y=xyd$z)

  if (logz)  {
    y <- xya$z
  } else {
    y <- 10^xya$z
    xyd$z <- 10^xyd$z
  }
  ## 'y' here is really a 'z' value, i.e., a concentration or dose,
  ## but calling it 'y' and putting it in a list(x=, y=) form
  ## allows to be plotted as a line along the track defined by the x,y, input locations
  ismax <- y==max(y)

  ## 9/28/12 add elements to the output list named "path" and "points" corresponding to
  ## how those are described in the comments above. 'path' and 'points' are completely
  ## redundant, just there for convenience
  
  xyp <- list(
           ## interpolated values along the path defined by the input x,y locations
           ## x is distance along the path, y is interpolated value (z) at that distance
           x=if(nfill > 0) dist.xy(xyf) else NULL,
           y=if (nfill > 0) y else NULL,
           ## interpolated x,y,z pairs for just the input x,y
           ## the %in% assumes that numerical imprecision hasn't crept in
           xyz=list(x=xy$x, y=xy$y, z=y[ xyf$x %in% xy$x & xyf$y %in% xy$y]),
           ## interpolated x,y,z pairs for then input x,y, and the filled in locations
           ## between successive input locations
           xyzfilled=list(x=xyf$x, y=xyf$y, z=y),
           ## find the max along the path
           maxloc=list(x=xyf$x[ismax], y=xyf$y[ismax], z=y[ismax]),
           grspc=grspc,
           nfill=nfill
           )
  xyp$path <- xyd
  xyp$path.filled <- xyp[c('x','y')]
  xyp$points <- xyp$xyz
  xyp$points.filled <- xyp$xyzfilled

  if (out) {
    assign('xy.out',xy,  envir=.GlobalEnv)
    assign('xyf.out',xyf,envir=.GlobalEnv)
    assign('xya.out',xya,envir=.GlobalEnv)
    assign('xyp.out',xyp,envir=.GlobalEnv)
  }
  
  if (plot.it) plot(xyp,type=type,xlab=xlab,ylab=ylab,...)

  invisible(xyp)
}

