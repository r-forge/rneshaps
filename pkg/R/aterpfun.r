## function aterpfun()

## LLNL-CODE-637312

## Given an input grid of x,y locations and a z value at each
## and a set of new x,y, pairs
## estimate a z value for each new x,y, location
## 
## Interpolate between x,y locations
## (see also closest.r)

## 4/17/08 rename aterp()     to aterpfun()
##         rename aterp.pl()  to aterp()

## 10/25/12
## misc clean up, improve comments

## 6/21/10
## corrected indexing of zg in fortran subroutine
## set
##   use.original to TRUE
## to use the previous (i.e., original) fortran

## 6/20/10
## rename the arguments so that names make it obvious
## which is in input (interpolate from) and which is the
## output (interpolate to)
##    xy.out     was   xy0
##    xyz.inp    was   iout  

## arguments are
##
##  xyz.inp    list with a structure suitable for use in image()
##          vectors x,y that define a grid
##          matrix z of values on that grid
##          e.g.:
##            list returned by cap.add()
##            list returned by interp()
##
##  xy.out   list(x=,y=)
##        points for which we need to interpolate a z value

## These objects are passed to a fortran routine, defined in ../src/aterp.f

## in the fortran ../src/aterp.f
##   xg, yg, zg   are for xyz.inp
##   x0, y0       are for xy.out
##   z0           is for interplolated values at x0,y0
## as of 10/25/12 these are:
##      double precision x0(800),y0(800),z0(800)
##      double precision xg(800),yg(800),zg(640000)
##
## therefore the lengths of the elements of xyz.inp and xy.out must match
## currently (10/25/12) 800, 640000
## (i.e., input can be up to an 800x800 grid)

## The R package building process takes care of building the object code
## (see ../src)
##    aterp.o
##    (optionally a legacy copy, aterporig.o)

## fortran subroutine assumes that all points in xy.out are inside the
## region defined by xyz.inp

## 10/25/12 not sure why this is/was needed:
## if xy.out$x and xy.out$y are not the same length, use the shorter

## This package is and was developed in Mac OS X
## originally:   f77 -c -cg89 aterp.f
##        now:   gfortan

aterpfun <- function(xyz.inp, xy.out, info=FALSE, use.original=FALSE) {
  
  n0 <- as.integer(min(length(xy.out$x),length(xy.out$y)))
  x0 <- as.double(xy.out$x[1:n0])
  y0 <- as.double(xy.out$y[1:n0])
  z0 <- as.double(rep(-999999.,n0))
  ixf <- iyf <- as.integer(rep(0,n0))
  fxf <- fyf <- as.double(rep(0,n0))
  z11f <- z12f <- z21f <- z22f <- as.double(rep(0,n0))

  nx <- as.integer(length(xyz.inp$x))
  ny <- as.integer(length(xyz.inp$y))

  ## see notes at end
  xg <- as.double(xyz.inp$x)
  yg <- as.double(xyz.inp$y)
  zg <- as.double(as.vector( xyz.inp$z ))

  ## prior to 4/24/08
  ##  zg[is.na(zg)] <- as.double(0)
  ## 4/25/08
  ##  Deal with the case when the path we are interpolating onto goes
  ##  outside the range of available data.
  ##  fortran can't handle NA, but need to have a way to force
  ##  find out which interpolated values are NA, if the input x,y pair
  ##  is in a location where any of the nearest grid locations are NA
  ##  Therefore, replace NA with a number I expect to never, ever see.
  ##    nacon
  ##  Then, in the fortran, check for adjacent grid values = nacon, and
  ##  return nacon if any are.

  nacon <- -1e30
  zg[is.na(zg)] <- as.double(nacon)

  if ( length(zg) > 640000 ) return('[aterpfun] Grid too large (hardcoded at max of 640,000 points')
  if ( n0 > 800 ) return(paste('[aterpfun] Trying to interpolate more than 800 points',n0))

  ##      subroutine aterp(x0,y0,z0,n0,
  ##                       xg,yg,zg,
  ##                       nx,ny,
  ##                       ixf,iyf,
  ##                       fxf,fyf,
  ##     .                 z11f,z12f,z21f,z22f,
  ##                       nacon)
  
  ## 6/21/10 option to use the previous fortran
  ##  retn.full <- .Fortran("aterp",
  if (use.original) fsub <- 'aterporig' else fsub <- 'aterp'

  retn.full <- .Fortran(fsub,
                        x0,y0,z0,n0,
                        xg,yg,zg,
                        nx,ny,
                        ixf,iyf,
                        fxf,fyf,
                        z11f,z12f,z21f,z22f,
                        nacon
                        )
  ## numerical precision is iffy, I send -1e-30 to the fortran
  ## and get back a mix, some of which includes -9.999999e+29
  ## therefore 
  ##   note that it is assumed nacon is negative
  retn.full[[3]][retn.full[[3]] <= nacon/100] <- NA
  if (info) {
    retn <- list(
              x=retn.full[[1]],
              y=retn.full[[2]],
              z=retn.full[[3]],
              ixf=retn.full[[10]],
              iyf=retn.full[[11]],
              fxf=retn.full[[12]],
              fyf=retn.full[[13]],
              z11f=retn.full[[14]],
              z12f=retn.full[[15]],
              z21f=retn.full[[16]],
              z22f=retn.full[[17]]
              )
  } else retn <- list(x=retn.full[[1]],y=retn.full[[2]],z=retn.full[[3]])
  retn
}


## this little bit of test code shows that simply unwrapping z,
## as in
##     zg <- as.double(as.vector( xyz.inp$z ))
## is the right thing to do

##    x <- c(2,4,6)
##    y <- c(3,6,9)

##    tmpf <- function(x,y) x + y

##    z <- outer(x,y, tmpf)

##    image.plot(x,y,z)

##    xyg <- expand.grid(x=x,y=y)
##    xyg$z <- xyg$x+xyg$y
##    xyg$zmat <- as.vector(z)
