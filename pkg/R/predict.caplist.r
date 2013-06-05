## 4/19/12
## similar to cfitfun()
## but takes a list of capcon objects created like:
##    cap.scld <- cap.cons <- list(bxx1d, bxx2d, bxx3d, bxx4d, bxx5d,   bxx1p, bxx2p, bxx6p)
##

## LLNL-CODE-637312

## example usage
##
## in a run file (e.g., run3.r
## there will be a statement like these, that set up the fit
##    cap.scld <- cap.cons <- list(bxx1d, bxx2d, bxx3d, bxx4d, bxx5d,   bxx1p, bxx2p, bxx6p)
##    nsrcfit <- 5

## after the fit is complete, the estimated parameters (coefficients) will be in a vector of
## length nrscfit. For example, from fitit.r:
##   ref.grid <- cap.grid(cap.cons , ngrid=400)
##   scls <- rtn.fit$par

## Then:
##   predict.caplist(cap.cons, at.lm, ref.grid, scls)
##   predict.caplist(cap.cons, offsite.locs.lm, ref.grid, scls)
## will produce predicted values

## Also, in fitit.r, after the fit has been found, cap.scld is created from cap.cons using
## the fitted coefficients. Hence:
##   predict.caplist(cap.scld, at.lm, ref.grid)
##   predict.caplist(cap.scld, offsite.locs.lm, ref.grid)

## Both of the above are only good after a run has finished, and before some other
## run has changed cap.cons, cap.scls, or scls

## The reference grid is needed in order to interpolate the individual models onto a common grid
## from which the predicted values will be interploted
## There is no check for prediction locations being within the extent of the grid

## scls and ref.grid defaults are from the most recent run of fitit.r (from one of run*.r)

## 5/22/13 changing name of first arg from 'caplist' to 'object', to conform to generic predict(object, ...)

predict.caplist <- function(
                            object,
                            predict.at.xy = at.lm,
                            coef = rep(1,length(object)),             ## created in fitit.r
                            refgrid = ref.grid          ## created in fitit.r
                            ) {

  ## coef is a vector of scaling factors that are adjusted to get the best fit
  ## (equivalent to source terms, since we're using models run using a unit source)

  ## object is a list() of cap model concentration output objects created by neshaps::getcon
  ## (possibly by using cap.join() to join a couple of such objects

  ## predict.at.xy are the locations at which we need to get predicted values

  ## lengths of object, coef must be the same
  ## if coef is too short, it will be filled out with 1.0 as many as needed


  ncons <- length(object)

  cat('[predict.object] number of capcon objects (ncons) is',ncons,'\n')
  
  ## scale the concentrations
  ## default scaling is one, which assumes that diffuse sources were scaled outside the function
  ##   if coef is too short, fill out with 1.0
  if (is.null(coef)) coef <- rep(1.0,ncons)

  ## if fewer coef provided than capcon objects, assume the remainder have coef=1
  ## (normally, I put diffuse sources first in the capcon list)
  if (length(coef) < ncons) coef <- c(coef, rep(1,ncons-length(coef)))
  cat('class(coef)',class(coef),'\n')

  cat('[predict.caplist] coef',coef,'\n')
  
  for (id in seq(ncons)) object[[id]]$cap <- object[[id]]$cap * coef[id]

  ## 2/6/13 option to prompt user for points
  ## assumes that a plot is available on which to use locator(), but does not check for this
  if (is.null(predict.at.xy)) {
    cat('[aterp] Use locator to select points\nMiddle button when done\n')
    predict.at.xy <- locator(type=locator.type,col=2)
  } 

  
  if (class(predict.at.xy) == 'SpatialPoints' | class(predict.at.xy)=='SpatialPointsDataFrame') {
    predict.at <- list(x=coordinates(predict.at.xy)[,1],
                   y=coordinates(predict.at.xy)[,2]
                   )
  } else {
    ## requires predict.at.xy is a list(x,y)
    predict.at <- predict.at.xy
  }
  
  ## interpolate all of them to common grid (ref.grid) and add
  ## then interpolate concentrations at sampler locations (at.xy)
  a.adj <- cap.add( object , grid=refgrid)
  aterp.at.xy <- aterp(a.adj, predict.at, nfill=0)

  predicted <- 10^aterp.at.xy$xyz$z

  ## return full information about scaled models
  names(object) <- unlist(lapply(object, function(x) x$site))
  names(coef) <- names(object)[1:length(coef)]
  ## return a list 
  ##    first element, cap.fitted is the best fit (i.e., scaled) model runs
  ##    remaining elements are copies of input objects and output objects
  ##    4/19/12 also returned the model prediction at the input locations as element predicted
  ##            keep element cap.avg for backwards compatibility
  tmp <- list(
              cap.added=a.adj,
              refgrid=refgrid,
              aterp.at.xy=aterp.at.xy,
              predict.at.xy=predict.at.xy,
              predict.at=predict.at,
              predicted=predicted,
              ndata=length(predict.at$x),
              nscaled=length(coef),
              ncons=ncons,
              coef=coef
              )
  class(tmp) <- 'cappred'
  tmp
}


## 5/22/13
## took print.cappred() out of this file and put it its own file
