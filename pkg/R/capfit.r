## 1/11/13
## Function to "fit" multiple 'capcon' objects to field data

## LLNL-CODE-637312

## args may be supplied in either of two ways:
##   1) individual arguments
##   2) a list of named args

## In the latter case, the arg names are as I have been naming them in my runc*.r scripts
##   (which I may revise to match the function arg names)
## In the former case, a few of them have more user-friendly names that will make more sense
## to other users (I hope)

## 1/10/13
## Decision:
##   internal to all Rneshaps package functions, work with simple x,y coordinates, no Spatial class.

## The only input that might have a Spatial class is the ambient air monitoring (sampling)
## data. The class that makes sense is SpatialPointsDataFrame.

## If the input sampling data is a SpatialPointsDataFrame, convert it to a simple
## data frame, with the coordinates in variables named x,y. Record the fact that
## it is spatial, and save the CRS (proj4string).

## If the input sampling data was Spatial, it's reasonable to return the fitted values
## as a SpatialPointsDataFrame with the same CRS.

## air dispersion model concentration input is direct from CAP88 PC output CON files and is not assigned a
## spatial class in getcon().

## The input sampling data and the concentration data in the  'capcon' objects must be in the same
## cartesian coordinate system. This is assumed and not checked.

## 1/11/13 some significant changes to object names
##  notably: grid.added changed to fitfield in the output (I had trouble remembering the name 'grid.added')

## requires supporting functions
##   cfitfun()
##   csrcpct()
##   get.cavgs()

capfit <- function(params=NULL, ngrid=400, optimizer.to.use=2,
                   assign.out=FALSE) {

  param.arg.name <- deparse(substitute(params))

  if (class(params) != 'cappar') {
    cat('\n')
    cat('[capfit] WARNING:  params was not prepared by function cappar() and may have missing elements\n')
    cat('\n')
  }

  ## used in scripts or functions that post-process the results (fitdoc.r, fitplot.r, others)
  trun <- Sys.time()
  runtime <- format(trun)

  ## 9/20/12
  ## updating for changes in where the averages come from
  ## averages will not always be a spatial points object

  ## seem I can do away with cap.scld for the fitting process
  ## replace it with cap.fitted for summaries and reports calculatd after the fitting

  ## 5/12/11
  ## handling of dataframes of sampler averages improved
  ## using fewer dataframes (previously had redundancy)

  ## 5/21/09
  ## set up to be source from, for example, fit1.r, fit2.r, etc.

  ## 5/13/09
  ## Search on any or all source terms by setting nsrcfit

  ## REQUIRES that
  ##   getcon.r
  ##   getmon.r
  ## have been run first

  ## most important inputs:
  ##    model output, using unit source for diffuse sources and known source for stack sources
  ##    sampling results

  ## for fits based on CAP88 PC output, the model ouput is a list of 'capcon' objects, one element per source
  ## each 'capcon' object contains the results from a CAP88 PC model run
  ##   names are:  cap.scld, cap.cons  (identical at first)

  ## Create list() of 'capcon' concentration output objects (see getcon.r)
  ## cfitfun() REQUIRES that the ones to be optimized come first, followed by those that
  ## are to be held fixed. But it is ok to optimize on all of them
  ## (5/18/09, not sure I need really need two copies, depends on whether cfitfun() changes either of them)

  ## these lines goes away when cfitfun() and cfitinfo() are in the package along with capfit()
  ## if (!exists('cfitfun') | !exists('cfitinfo') | !exists('get.cavgs')) {
  ##   cat('[capfit] cfitfun, cfitinfo, or get.cavgs not found\n')
  ##   cat('[capfit] Must source cfitfun.r, cfitinfo.r, or get.cavgs.r. Stopping.\n')
  ##   stop('',call.=FALSE)
  ## }

  ##
  ## playing with and checking scope
  ##
  
  ## convenient but potentially dangerous:
  ## Assign the value of elemens of params to objects with the same name inside the function
  ## This makes it convenient to refer to them within the function, for example, as 'strt' rather than 'params$strt'
  ## If an object of the same name exists outside the function, it is not changed
  for (nm in names(params)) {
    ##    cat('[capfit] assigning',nm,'to GlobalEnv inside capfit\n')
    assign(nm, params[[nm]])
  }

  ## as a result of the above loop I can, inside this function,
  ## reference elements of the list params by their element name only
  ## without the prefix params$

  
  ## check for elements in the params list for which there is an object of the same name outside the function
  ## if there are any, the user risks using them by accident, so provide a way to remove them from .GlobalEnv
  ##  obj.local <- names(params)
  obj.local <- ls()
  obj.glbl <- ls('.GlobalEnv')
  obj.bth <- intersect(obj.local, obj.glbl)

  if (length(obj.bth) > 0) {
    cat('[capfit] Note: found object(s) outside the function with the same name name as a params element\n')
    print(obj.bth,quote=FALSE)
    cat('\n')
    cat('[capfit] assigning the list of such objects to tmp.obj.bth [so that you can remove them',
        'from .GlobalEnv with rm(list=tmp.obj.bth)]\n')
    assign('tmp.obj.bth',obj.bth,'.GlobalEnv')
  } else assign('tmp.obj.bth',NULL,'.GlobalEnv')

  ##  cat('---- local objs ----\n')
  ##  print(obj.local)

  if (FALSE) {
    ##  this whole bit has extra printing
    ##  used for trying to understand scope and search path?
    cat('\n')
    cat('[capfit] search list\n')
    print(search())
    cat('\n')

    cat('[capfit] supposedly global objects\n')
    print(obj.glbl)
    cat('\n')
  
    cat('[capfit] local objects [from ls()]\n')
    print(obj.local)
    cat('\n')
    cat('[capfit] intersect\n')
    print(obj.bth )
    cat('\n')
    cat('[capfit] Where are they:\n')
    for (ob in obj.bth) cat(ob,find(ob),'\n')
    cat('\n')
  }
  
  cat('\n')
  cat('------------------------------------------------\n')
  cat('[capfit] ----------- capfit starting -----------\n')
  cat('------------------------------------------------\n')

  cat('[capfit] run code:',run.code,'\n')
  cat('[capfit]',run.description,'\n')
  if (is.final) cat('\n[capfit] This is a "final" run, with run.code =',run.code,'\n')

  ##
  ## start work
  ##

  ## validity checks
  ## cappar() enforces these, I think, but needed if params were created manually
  if (nsrcfit != length(strt)) stop('Number of start params != number of sources being fit',call.=FALSE)
  if (nsrcfit != length(lb)) stop('Number of lower bounds != number of sources being fit',call.=FALSE)
  if (nsrcfit != length(ub)) stop('Number of upper bounds != number of sources being fit',call.=FALSE)
  if (nsrcfit > nsources) stop('Tried to fit more sources than were supplied (nsrcfit > nsources)',call.=FALSE)

  ##
  ## get average to be fit
  ##
  avgs.list <- get.cavgs(params)

  ## not absolutely necessary to make local objects, but it's easier for now (11/19/12)
  avgs.with.names <- avgs.list$avgs.with.names
  avgs.to.fit <- avgs.list$avgs.to.fit
  locs.to.use <- avgs.list$locs.to.use

  navgs.to.fit <- nrow(avgs.to.fit)

  ## required inputs for cfitfun() include
  ##    location coordinates as list(x,y)
  ##    sampler averages at those locations
  at.xy <- list(x=avgs.to.fit$x, y=avgs.to.fit$y)

  ## source names
  ## the source names were defined when the CAP88 PC concentration files were read using getcon():
  ##   bxx1pn <- getcon(bxx1p.file,site='BXX1 stack', datadir=bxx1p.dir)
  ## run.srcs corresponds to aermod runa.srcs (i.e., as specified in a runa*.r file)
  ##   cap.desc is identical to run.srcs
  ##run.srcs <- unlist(lapply(cap.cons, function(x) x$site))
  ## this is now c.params$run.srcs, placed in .GlobleEnv by the assign at the top

  ##
  ## Create a grid that encompasses all of the model runs being used
  ##
  ref.grid <- cap.grid(cap.cons , ngrid=ngrid)
  ref.field <- cap.add(cap.cons , grid=ref.grid)


  cat('[capfit] Sources: ',paste(run.srcs, collapse=', '),'\n')
  cat('[capfit] nsources =',nsources,'  nsrcfit =',nsrcfit,'  nfixed =',nfixed,'\n')
  cat('[capfit] starting values: ',strt,'\n')
  cat('[capfit] lower bounds: ',lb,'\n')
  cat('[capfit] upper bounds: ',ub,'\n')
  cat('[capfit] fitting: ',locs.to.use,'\n')
  cat('\n')


  if (psearch) {

    ## ---------------------------------------------------------------------------
    ## run an optimizer
    ## ---------------------------------------------------------------------------

    ## note: using defaults for other args of cfitfun()
    ##   cap.to.scale = cap.scld
    ##   cap.input = cap.cons
    ##   fit.at.xy = at.xy
    ##   fit.to.avgs = avgs.with.names
    ##   refg = ref.grid
    ## 11/21/12 these were previously found in the global env (.GlobalEnv) but
    ## now that I'm making function-based version instead of a script-based version, have to pass them

    optim.name <- switch(optimizer.to.use,
                         '1'='nlminb() with default method',
                         '2'='nlminb() with method L-BFGS-B',
                         '3'='optim() with method SANN',
                         '4'='optim() with default method',
                         'invalid value for optimizer.to.use')
    
    cat('[capfit] Starting optimizer using',optim.name,' (be patient)\n\n')

    if (optimizer.to.use != 2) {
      cat('[capfit] Note: only optimizer 2 supports upper and lower bounds on the search\n')
    }
    
    if (optimizer.to.use==1) {
      tmp2 <- nlminb( strt, cfitfun, lower=lb, upper=ub,
                     cap.crnt=cap.cons,
                     fit.at.xy=at.xy,
                     sampler.avgs=avgs.with.names,
                     refgrid = ref.grid,
                     runcd=run.code,
                     runds=run.description,
                     control=list(trace=5) )
      rtn.fit <- tmp2
      rms.fit <- sqrt( rtn.fit$objective/navgs.to.fit )
    }

    if (optimizer.to.use==2) {
      ## this is the only optim() method that implements upper and lower bounds
      tmp3 <- optim( strt, cfitfun, lower=lb, upper=ub,
                    cap.crnt=cap.cons,
                    fit.at.xy=at.xy,
                    sampler.avgs=avgs.with.names,
                    refgrid = ref.grid,
                    runcd=run.code,
                    runds=run.description,
                    control=list(trace=3,REPORT=5) , method='L-BFGS-B')
      rtn.fit <- tmp3
      rms.fit <- sqrt( rtn.fit$value/navgs.to.fit )
    }

    if (optimizer.to.use==3) {
      ## doesn't allow bounds
      tmp4 <- optim( strt, cfitfun,
                    cap.crnt=cap.cons,
                    fit.at.xy=at.xy,
                    sampler.avgs=avgs.with.names,
                    refgrid = ref.grid,
                    runcd=run.code,
                    runds=run.description,
                    control=list(trace=2) , method= 'SANN')
      rtn.fit <- tmp4
      rms.fit <- sqrt( rtn.fit$value/navgs.to.fit )
    }

    if (optimizer.to.use==4) {
      ## doesn't allow bounds
      tmp5 <- optim( strt, cfitfun,
                    cap.crnt=cap.cons,
                    fit.at.xy=at.xy,
                    sampler.avgs=avgs.with.names,
                    refgrid = ref.grid,
                    runcd=run.code,
                    runds=run.description,
                    control=list(trace=4))
      rtn.fit <- tmp5
      rms.fit <- sqrt( rtn.fit$value/navgs.to.fit )
    }

    ## ---------------------------------------------------------------------------
    ## optimizer done
    ## ---------------------------------------------------------------------------

    cat('\n')
    cat('---------------------------------------------------------------\n')
    cat('[capfit] optimization done, starting to build summary objects\n')
    cat('---------------------------------------------------------------\n')
    cat('[capfit] optimization message:\n')
    print(rtn.fit$message)
    cat('\n')

    ## fitted params as vector
    ##    cat('[capfit] Saving parameters to scls (estimated source term scaling factors)\n')
    scls <- rtn.fit$par
    names(scls) <- run.srcs[1:nsrcfit]

    ## end of psearch TRUE
  } else {
    cat('[capfit] Using input parameters (strt) as supplied\n')
    rtn.fit <- list(message='cap88 using input parameters as supplied, no search')
    scls <- strt
    rms.fit <- NA
  }


  ## summary info
  ## 1/11/13 rename fit.detail to fit.results
  
  ## don't print fit.results !!
  ## note: using defaults for other args of cfitfun()
  ##   cap.to.scale = cap.scld
  ##   cap.input = cap.cons
  ##   fit.at.xy = at.xy
  ##   fit.to.avgs = avgs.with.names
  ##   refg = ref.grid

  ## calling cfitfun() with returnall=TRUE returns information including
  ##   the input caplist object, but with concentrations scaled by the final fit (cap.fitted)
  ##   the estimated concentrations on the regular grid (fitfield) (named grid.added before 1/11/13)
  ##   ss
  ##   rms
  ##
  ##  cat('[capfit] Saving full final info to fit.results\n')
  fit.results <- cfitfun(scls ,
                         cap.crnt=cap.cons,
                         fit.at.xy=at.xy,
                         sampler.avgs=avgs.with.names,
                         refgrid = ref.grid,
                         runcd=run.code,
                         runds=run.description,
                         returnall=TRUE)

  
  if (assign.out) assign('current.fit.results',fit.results,'.GlobalEnv')
  
  ## relative contributions from each source to each sampler to fit.info
  ## using default arg csrcpct( , type='pct'); returns a data frame
  ## 1/11/13 rename fit.info to src.pcts
  src.pcts <- csrcpct(fit.results)
  rownames(src.pcts) <- NULL  ## 1/11/13

  if (assign.out) assign('current.src.pcts',src.pcts,'.GlobalEnv')

  ##
  ## some of the next can be replaced by csrcpct()   ????
  ##

  cap.fitted <- fit.results$cap.fitted    ## an object of class cfitobj 

  ##
  ## basic goal for the remainder appears to be to construct the "fitpred" object
  ##

  ## may want to include column ord.x
  ## 1/10/13 fitpred is now non-Spatial (but may be converted to Spatial below)
  ##    fitpred <- avgs.to.fit.sptl[,c(loc.col.name,'n',avgs.col.name)]
  fitpred <- avgs.to.fit[,c(loc.col.name,'x','y','n')]
  ## fit.results$model.avgs should be in same order since input to cfitfun() was direct from avgs.to.fit.sptl
  fitpred$model.avgs <- fit.results$model.avgs
  fitpred[[avgs.col.name]] <- avgs.to.fit[[avgs.col.name]]
  fitpred$diff <- signif(fitpred$model.avgs - fitpred[[avgs.col.name]],3)
  fitpred$pct.diff <- signif(100*fitpred$diff/fitpred[[avgs.col.name]],2)
  fitpred$ratio <- signif(fitpred$model.avgs/fitpred[[avgs.col.name]],3)

  fitpred$inout <- with(fitpred, ifelse(abs(pct.diff) < pct.diff.criterion , 'in','out'))
  
  ## 1/11/13 I have been removing any use of uncertaintites on the averages
  ## if (any(names(fitpred)==unc.col.name)) {
  ##   ## within +/- two sigma standard error the field weighted average
  ##   ## 4/28/11 change to use variable names defined in run*.r
  ##   fitpred$iou <- with(fitpred ,
  ##                       ifelse(model.avgs >= fitpred[[avgs.col.name]] - 2*fitpred[[unc.col.name]] &
  ##                              model.avgs <= fitpred[[avgs.col.name]] + 2*fitpred[[unc.col.name]] ,
  ##                              'in','out'))
  ## }

#  if (!psearch) {
#    ## calculate rms.fit
#    rms.fit <- sqrt( sum( (fitpred$model.avgs - fitpred[[avgs.col.name]])^2 ) )
#  }
  
  ## 1/10/13 if input averages were Spatial, convert fitpred to Spatial
  if (avgs.list$Spatial.input) {
    cat('[capfit] Input averages were Spatial, converting output fitted values to Spatial\n')
    require(sp) ## just in case, but it should have happened previously
    ## convert coordinate names back to what they were in input (colnames of matrix in @coords slot)
    names(fitpred)[names(fitpred) %in% c('x','y')] <- avgs.list$coln
    coordinates(fitpred) <- avgs.list$coln
    proj4string(fitpred) <- avgs.list$prj
  }
  
  ## cat('[capfit] fitpred\n')
  ## print(fitpred)
  ## cat('\n')

  ## redundant; done again later
  ## cat('[capfit] scaling (estimated parameters)\n')
  ## print(signif(fit.results$scls,3))
  ## cat('\n')

  ## 11/21/12
  ## The original script-based version used scripts cfitrpt.r, cfittex.r cfittexp.r
  ## and Rweave to a nicely formattad pdf report.
  ## I have not yet written a function to create such reports to work with this function's output.
  ## However, when I do, I will want a basename for the output files that identifies the run

  ## fbase is the "basename" intended for use in naming files associated with such pdf reports
  fbase <- run.code
  ##  the datestamp option allows to distinguish multiple runs with the same run code on the same day
  if (datestamp.output=='y') fbase <- paste(run.code,'_',format(trun,'%Y_%m_%d_%H_%M'),sep='')
  ## option to label output files as final
  if (is.final) fbase <- paste(run.code,'_final',sep='')

  
  ## 9/26/12 add more results to fit.results
  c.results <- c(fit.results,
                 
                 list(
                   locs.to.use=locs.to.use,
                   fitpred=fitpred,
                   src.pcts=src.pcts,
                   rtn.fit=rtn.fit,
                   rms.fit=rms.fit,
                   scls=scls,
                   runtime=runtime,
                   fbase=fbase,
                   param.arg.name=param.arg.name,
                   optimizer.used=optimizer.to.use,
                   to.bq=cap.cons[[1]]$to.bq,              ## assumes/requires all in the same units
                   Spatial.input=avgs.list$Spatial.input,
                   prj=avgs.list$prj
                   )
                 
                 )

  ## add (most) input params to c.results
  ## keep many but not all of the inputs from a.params; remove some redundant ones
  ##  10/3/12 there are still some redundancies, and probably some renaming that would be worth doing
  ## 11/19/12 locs.to.use can be 'all' in the input params, so use the one in c.results instead
  c.results <- c(
                 params[ setdiff(names(params),c('lb','ub','dummy','locs.to.use')) ],   ## params is a list
                 c.results[ setdiff(names(c.results), c(names(params),'rms.fit') ) ]    ## c.results is a list
                 )


  ## 10/24/12 create a "current.results" object that will have the most recent run results
  ## regardless of model type (i.e., same assignment in capfit)
  if (assign.out) assign('current.results', c.results, '.GlobalEnv')

  
  ## as of 11/21/12 this is completely ignored in this function version
  ## if (is.final) {
  ##   ## save some key objects
  ##   cat('[capfit] Saving "final" copies of selected key objects\n')
  ##   c.final <- list(scls=scls,
  ##                   caplist=cap.cons,               ## the input 'capcon' objects
  ##                   capscld=cap.fitted,             ##
  ##                   run.srcs=run.srcs,
  ##                   nsrc=nsrcfit,
  ##                   run.code=run.code,
  ##                   param.arg.name=param.arg.name)
  ##   cat('\n')
  ## }

  
  cat('[capfit] timestamp is ',runtime,'\n')
##  cat('[capfit] Details of fit stored in "current.fit.results"  (do not print!)\n')
##  cat('[capfit] Sampler vs. source info stored in "current.src.pcts"\n')
##  cat('[capfit] Model vs data stored in "fitpred"\n')
##  cat('[capfit] scale factors are stored in "scls"\n')
  cat('[capfit] Fitted values (scaling factors):\n')
  print(signif(scls,3), quote=FALSE)
  cat('[cafit] RMS:',c.results$rms,'\n')
  cat('\n')
  ##  cat('[capfit] nlminb message:',rtn.fit$message,'\n\n')
  ##  cat('[capfit] To document with an output file and plot, source("fitdoc.r")\n')

  cat('[capfit] Done\n')
  cat('\n')


  ## can do, for example
  ##   image.plot(c.results$fit.results$cap.added)
  class(c.results) <- 'capfit'

  c.results
}
