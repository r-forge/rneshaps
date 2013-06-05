## cap.i
##   interpolate a single capcon object [created by getcon()] onto a grid

## LLNL-CODE-637312

## cap.ii
##  (see cap.ii.r)
##   interpolate two capcon objects [created by getcon()] onto a common grid and add them
## for both of them, the grid is defined inside the function

## cap.add() is a more general version of cap.ii(), allows more than 2 capcon objects

## 4/18/12
## assign a class to the output

## 4/18/08
##  ---- Important change ----
## make
##    cap.i(), cap.ii(), and cap.add()
## consistent in how they handle taking logs
## Namely: all take log10() before interpolating,
## then all return interpolated values in log10 units.
## Before today, all took log10() before interpolating,
## but cap.add() returned log10 units, the others converted
## back to original units.


##   cap.i()
##
##   dat      object returned from getcon() or getsum() functions
##            a 'capcon' object
##   ngrid    grid dimension (grid will be ngrid x ngrid)
##   xlims    range of grid, defaults to x range of polar grid
##   ylims    range of grid, defaults to y range of polar grid
##   ncp      linear argument to interp()
##   extrap   extrap argument to interp()
##
## Returns
##   List with components suitable for use in contour() and image()
##   x        vector of x coordinates
##   y        vector of y coordinates
##   z        ngrid x ngrid matrix of concentrations or doses
##   name     name object of dat
##
cap.i <- function(dat,ngrid=60,xlims=range(dat$x),ylims=range(dat$y),linear=TRUE,extrap=FALSE) {
  ## could/should add test for class capcon (also "capsum"?)
  require(akima)
  xo <- seq(xlims[1],xlims[2],len=ngrid)
  yo <- seq(ylims[1],ylims[2],len=ngrid)
  tmp <- interp(dat$x,dat$y,log10(dat$cap),xo=xo,yo=yo,linear=linear,extrap=extrap)
##  tmp$z <- 10^(tmp$z)   ## 4/18/08 change to return log10 units, like cap.add()
  tmp$plims <- c( min(xlims[1],ylims[1]), max(xlims[2],ylims[2]) )
  tmp$measure <- dat$measure
  tmp <- c(tmp, dat[c('site','name','date','windfile','offset','measure')])
  class(tmp) <- 'capfield'
  tmp
}
##
## additional notes on cap.i()
##
## cap.i() should give the same result as cap.add() when cap.add() is
## supplied with only a single input object. Hence, this
## function can be considered superceded by cap.add(), but is perhaps
## more convenient than cap.add() because cap.add() required the grid
## to be created in a separate step.
##
## prior to 4/19/08 I had both cap.grd() and cap.i() which served
## the same purpose. Keep only cap.grd() because it had a more
## general way of defining the grid, but use the name cap.i()
## interpolate from CAP88 output (values on a polar grid)
## to a rectangular or square grid

