## see also cap.i.r for more comments

## LLNL-CODE-637312

##
## function cap.ii() interpolates two CAP88PC runs from polar
## grid onto rectangular grid, and adds them
##
## superceded by cap.add()
## (but cap.ii() is perhaps more convenient when there are only two
##  runs to be added, because it includes the grid definition)
##
## Arguments are
##   dat1      object returned from cap() function
##   dat2      object returned from cap() function
##   ngrid    grid dimension (grid will be ngrid x ngrid)
##   ncp      ncp argument to interp()
##   extrap   extrap argument to interp()
## Returns
##   List with components suitable for use in contour() and image()
##   x        vector of x coordinates
##   y        vector of y coordinates
##   z        ngrid x ngrid matrix of concentrations or doses
##   name     concatenation of name objects of dat1 and dat2
##
cap.ii <- function(dat1,dat2,ngrid=60,xlims=range(dat$x),ylims=range(dat$y),linear=TRUE,extrap=FALSE) {
  xo <- seq(xlims[1],xlims[2],len=ngrid)
  yo <- seq(ylims[1],ylims[2],len=ngrid)
  tmp.1 <- interp(dat1$x,dat1$y,log(dat1$cap),xo=xo,yo=yo,linear=linear,extrap=extrap)
  tmp.2 <- interp(dat2$x,dat2$y,log(dat2$cap),xo=xo,yo=yo,linear=linear,extrap=extrap)
   if (any(tmp.1$x!=tmp.2$x) | any(tmp.1$y!=tmp.2$y) ) return("Coords don't match\n")
  tmp <- tmp.1
  ## have to add in original units
  tmp$z <- exp(tmp.1$z)+exp(tmp.2$z)
  ## 4/18/08 change to return log10 units, like cap.add()
  tmp$z <- log10(tmp$z)
  tmp$name <- paste(dat1$name,'  +  ',dat2$name)
  class(tmp) <- 'capfield'
  tmp
}
