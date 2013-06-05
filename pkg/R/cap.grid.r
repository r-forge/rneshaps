## 4/18/12
## assign a class to the output

## LLNL-CODE-637312

## 5/7/09 extracted from capfuns.r
##
## create a grid definition that encompasses more than one run
## note, gives just the x and y vectors, not the entire set of x,y pairs
##

## intended to be used for the grid argument of cap.add()

## function is designed for input of form  (form 1)
##   cap.grid(obj1, obj2)
## where each object is of class capcon, created by getcon()
## but it also works for an alternate form (form 2)
##   cap.grid( list(obj1, obj2) )
## and an adjustment if found

## The input ojbects are intended to be of class "capcon", but
## may also be of class "capfield" since both types have $x and $y elements

## for useful output, ngrid should be several hundred
## max is currently 800, see aterp.f

cap.grid <- function(...,ngrid=600) {

  objs <- list(...)

  ## option for user to provide a list of capcon objects (form 2)
  if (length(objs) == 1 && is.list(objs) && class(objs) != 'capcon') {
    objs <- objs[[1]]
  }
 
  xr <- range(unlist(lapply(objs,function(ob)range(ob$x,na.rm=TRUE))))
  yr <- range(unlist(lapply(objs,function(ob)range(ob$y,na.rm=TRUE))))

  out <- list(x=seq(xr[1],xr[2],len=ngrid),
              y=seq(yr[1],yr[2],len=ngrid),
              xr=xr,
              yr=yr,
              plim=range(xr,yr)
              )
  class(out) <- 'capgrid'
  out
}
