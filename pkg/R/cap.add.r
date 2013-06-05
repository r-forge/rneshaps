##
## cap.add
##

## LLNL-CODE-637312

## add the concentrations from one or more capcon objects [created by getcon()]
## cap.add() is an extension of cap.ii(), in that cap.ii() handles exactly two capcon objects
## cap.add() handles an arbitrary number of capcon objects

##   ...    one or more objects created by getcon()
##   grid   object created by cap.grid()
##          (ref.grid is my default name, but other users won't necessarily use it)
## 5/22/13  it has been convenient to have a default value of ref.grid,
## but this is not appropriate for a distributed package

## interpolates each onto the common grid, and adds

## See the file cap.i.r for additional documentation

## 4/18/12
## assign a class to the output

cap.add <- function(..., grid, linear=TRUE, extrap=FALSE) {
  require(akima)

  if (missing(grid)) {
    cat('[cap.add] A reference grid created by cap.grid() must be supplied for the "grid" arg. Stopping.\n')
    stop('',call.=FALSE)
  }
  
  if (class(grid) != 'capgrid') {
    cat('[cap.add] grid argument is not of class "capgrid". Stopping\n')
    stop('',call.=FALSE)
  }
  
  caps <- list(...)

  ## test and adjust for form 2
  if (length(caps) == 1 && is.list(caps) && class(caps) != 'capcon') {
    caps <- caps[[1]]
  }
  
  nmodel <- length(caps)
  ctrs <- list()
  length(ctrs) <- nmodel
  tmp <- interp(caps[[1]]$x,caps[[1]]$y,log10(caps[[1]]$cap),
		xo=grid$x,yo=grid$y,linear=linear,extrap=extrap)
  tmp$name <- caps[[1]]$name
  tmp$site <- caps[[1]]$site
  tmp$offset <- caps[[1]]$offset
  zlin <- 10^tmp$z
  if (nmodel >= 2) {
    for (i in 2:nmodel) {
      tmpi <- interp(caps[[i]]$x,caps[[i]]$y,log10(caps[[i]]$cap),
		     xo=grid$x,yo=grid$y,linear=linear,extrap=extrap)
      zlin <- zlin+10^tmpi$z
      tmp$name <- paste(tmp$name,', ',caps[[i]]$name)
      tmp$site <- paste(tmp$site,', ',caps[[i]]$site)
      tmp$offset <- rbind(tmp$offset,caps[[i]]$offset)
    }
    ## we interpolated in log units, back-transformed to original units for adding,
    ## then took log again for return value (so that the image plot is in log units)
    tmp$z <- log10(zlin)
  }
  tmp$ctrs <- as.list(tmp$offset)
  tmp <- c(tmp, grid[c('xr','yr','plim')])
  tmp$zr <- range(tmp$z,na.rm=TRUE)

  class(tmp) <- 'capfield'
  
  tmp
}
