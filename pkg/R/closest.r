## Given an input grid of x,y,z pairs
## and a set of new x,y, pairs
## get the z of closest location on grid to the input x,y

## LLNL-CODE-637312

## (see also aterp.r)

## 8/25/10
## rename the arguments so that names make it obvious  (did this earlier for aterp.r)
## which is in input (interpolate from) and which is the
## output (interpolate to)
##    xy.out     was   xy0
##    xyz.inp    was   iout  

## arguments are
##  xy.out   list(x=,y=)
##        points for which we need to interpolate a z value
##
##  xyz.inp    list with a structure suitable for use in image()
##          vectors x,y that define a grid
##          matrix z of values on that grid
##          e.g.:
##            list returned by cap.add()
##            list returned by interp()


## fortran subroutine assumes that all points in xy.out are inside the
## interpolated region
## if xy.out$x and xy.out$y are not the same length, use the shorter

## use f77 -c -cg89 closest.f

closest <- function(xyz.inp,xy.out,info=FALSE) {

  ## 4/25/08 NOTE: fix for points in xy.out outside the range where the grid has data
  ## has not been tested
  cat('[closest.r] WARNING:  not tested for xy.out points outside range of data\n')
  
  ## have to make the location of the fortran code more general
  ##  if (!is.loaded(symbol.For("closest")))
  ##  print(dyn.load( paste(getenv('SPLUSLIB'),'/delaunay/deldir.o',sep='') ))
  ##    print(dyn.load('/us/macq/epd/neshaps/afuns/closest.o'))
  
  ## later, make a package, then this will be automatic
  ##  if (!is.loaded('closest')) dyn.load('/home/macqueen1/epd/neshaps/afuns/closest.so')
  
  n0 <- as.integer(min(length(xy.out$x),length(xy.out$y)))
  x0 <- as.double(xy.out$x[1:n0])
  y0 <- as.double(xy.out$y[1:n0])
  z0 <- as.double(rep(0,n0))
  ixf <- iyf <- as.integer(rep(0,n0))
  fxf <- fyf <- as.double(rep(0,n0))
  z11f <- z12f <- z21f <- z22f <- as.double(rep(0,n0))

  nx <- as.integer(length(xyz.inp$x))
  ny <- as.integer(length(xyz.inp$y))

  xg <- as.double(xyz.inp$x)
  yg <- as.double(xyz.inp$y)
  zg <- as.double(as.vector( xyz.inp$z ))

  ##  zg[is.na(zg)] <- as.double(0)
  ## 4/25/08
  ##  Deal with the case when the path we are interpolating onto goes
  ##  outside the range of available data. See aterpfun.r
  nacon <- -1e30
  zg[is.na(zg)] <- as.double(nacon)

  nmax <- 800  ## see closest.f
  
  if ( length(zg) > nmax*nmax ) return('[Rneshaps::closest] Grid too large')
  if ( n0 > nmax ) return(paste('[Rneshaps::closest] Trying to interpolate too many points',n0,' (max =',nmax,')'))

##      subroutine closest(x0,y0,z0,n0,xg,yg,zg,nx,ny,ixf,iyf,fxf,fyf,
##     .     z11f,z12f,z21f,z22f,nacon)

  
  retn.full <- .Fortran("closest",
                        x0,y0,z0,n0,
                        xg,yg,zg,
                        nx,ny,
                        ixf,iyf,
                        fxf,fyf,
                        z11f,z12f,z21f,z22f,
                        nacon
                        )
  retn.full[[3]][retn.full[[3]] <= nacon/100] <- NA

  retn.many <- list(
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
                 z22f=retn.full[[17]],
                 nacon=nacon
                 )
  retn.few <- list(
                   x=retn.full[[1]],y=retn.full[[2]],z=retn.full[[3]]
		   )
  if (info) return(retn.many)
  retn.few
}
