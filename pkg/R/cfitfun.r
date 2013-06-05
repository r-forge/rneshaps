## function to pass to optimizer routines
##   when returnall=FALSE
##   in which case it returns sum of squares

## LLNL-CODE-637312

## or to calculate additional model results from user-supplied  scaling factors
##   when returnall=TRUE
## set up as function for optimizaton type searches

cfitfun <- function(scls ,
                    cap.crnt=cap.cons,
                    fit.at.xy=at.xy,
                    sampler.avgs=avgs.with.names,
                    refgrid = ref.grid,
                    runcd=run.code,
                    runds=run.description,
                    returnall=FALSE) {

  ## every time this function is called, the initial value of cap.crnt must be the same
  ## because the concentration objects in it will be multiplied by the parameters it is
  ## called with (scls)

  ## might be faster to supply it with two copies, one of which gets modified ????
  ## would have to do performance testing
  
  ## scls is a vector of scaling factors that are adjusted to get the best fit
  ## (equivalent to source terms, since we're using models run using a unit source)

  ##
  ## some of the following comments are out of date (9/20/12), because there is now only
  ## a single input list among the function args

  ## 10/3/12 renaming data.avgs to sampler.avgs
  ##
  ## this function version does not use a separate object named cap.input
  ## cap.input is a list() of cap model concentration output objects created by Rneshaps::getcon
  ## cap.crnt  are identical copies of cap.input, but are the ones that are changed inside this function
  ## cap.crnt should be a list of 'capcon' objects
  ##    each element is created by a sequence like (see getcon.r)
  ##       getcon('name.CON.file', site='some site name', datadir='directory.containing.CON.file')
  ##    or combine complementery CON files like this:
  ##       tmp1 <- getcon( {near file} )
  ##       tmp2 <- getcon( {far file} )
  ##       cap.join( list(tmp1,tmp2) )

  ## fit.at.xy are the locations at which we need to get the best fit
  ##    ("at" is the word "at", not an abbreviation for "air tritium")
  ## sampler.avgs is a vector of values at the locations

  ## lengths of cap.crnt, cap.input must be the same
  ## lengths of them must be >= length(scls)
  ## lengths of fit.at.xy$x, fit.at.xy$y, and sampler.avgs must be the same
  ## if length(scls) < length(cap.input) then optimization is on just a subset of the sources

  ## to use this function to get predicted concentrations at arbitrary locations
  ##    supply desired coords in fit.at.xy
  ##    set returnall=TRUE
  ##    look at the $cap.conc element of the return
  
  ## scale the concentrations -- make a cap object list where the concentrations
  ## represent the supplied parameters
  ##   the $cap element is in orginal units (not log10)
  ## ("cap.crnt" because  they are the "current" parameters each time an optimizing function calls this function)
  for (id in 1:length(scls)) cap.crnt[[id]]$cap <- cap.crnt[[id]]$cap * scls[id]

  ##   should put the next three expressions together in a single Rneshaps package function
  
  ## calculate the predicted sampler concentrations based on the current parameters
  ## by interpolating all of them to common grid (ref.grid), adding them,
  ##   10/4/12 rename cap.added to grid.added (and in afitfun() renaming aer.added to grid.added

  ## cap.add() takes a list of class capcon objects [i.e., created by getcon()], interpolates
  ## log10( the $cap element) of each to a common grid, then adds [hence, logz=TRUE in aterp()].
  ## returns a class 'capfield' object, which is a list including x,y,z elements suitable for
  ## input to image.plot()
  grid.added <- cap.add( cap.crnt , grid=refgrid)

  ## then interpolating concentrations at sampler locations (fit.at.xy)  (aterp in in package Rneshaps)
  aterp.at.xy <- aterp(grid.added, fit.at.xy, nfill=0, logz=TRUE)

  ## anti-log since logz=TRUE in call to aterp()
  ## 10/4/12 changing name cap.avg to model.avgs (like in afitfun())
  model.avgs <- 10^aterp.at.xy$xyz$z

  ## When this function is called by an optimzation routine, need to return the value
  ## to be minimized, namely, the sum of squared differences
  ## Otherwise return full info
  
  if (returnall) {
    ## return full information about scaled models
    ## otherwise, returns the sum of squares (which is what is being minimized)
    names(cap.crnt) <- unlist(lapply(cap.crnt, function(x) x$site))
    names(scls) <- names(cap.crnt)[seq(scls)]
    names(model.avgs) <- names(sampler.avgs)
    ss <- sum( (model.avgs-sampler.avgs)^2 )
    rms <- sqrt(ss/length(model.avgs))
    ## return a list 
    ##    first element, cap.fitted is the best fit (i.e., scaled) model runs
    ##    remaining elements are copies of input objects and output objects
    ##    4/19/12 also returned the model prediction at the input locations as element cap.conc
    ##            keep element model.avgs for backwards compatibility
    ## 1/11/13 change name grid.added to fitfield in output list (see capfit function)
    tmp <- list(
             run.code=runcd,
             run.description=runds,
             cap.fitted=cap.crnt,
             fitfield=grid.added,
             refgrid=refgrid,
             aterp.at.xy=aterp.at.xy,
             fit.at.xy=fit.at.xy,
             sampler.avgs=sampler.avgs,
             fit.to.names=names(sampler.avgs),
             model.avgs=round(model.avgs,3),
             cap.conc=model.avgs,
             ndata=length(sampler.avgs),
             nscaled=length(scls),
             nmodels=length(cap.crnt),
             ss=ss,
             rms=rms,
             scls=scls)
    class(tmp) <- 'cfitobj'
    return(tmp)
  }
  
  ## return the sum of squared differences, the value that is to be minimized
  sum( (model.avgs-sampler.avgs)^2 )  
}
