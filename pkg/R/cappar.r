## 11/19/12
## this function creates a list for input to capfit()

## LLNL-CODE-637312

## args may be supplied in either of two ways:
##   1) individual arguments
##   2) a list of named args

## In the latter case, the arg names are as I have been naming them in my runc*.r scripts
##   (which I may revise to match the function arg names)
## In the former case, a few of them have more user-friendly names that will make more sense
## to other users (I hope)

##  see cf-test.r

cappar <- function(run.code='no.run.code',
                   run.description='no run description',
                   
                   params=NULL,
                   
                   ## CAP88 PC concentrations
                   cap.cons=NA,
                   
                   ## model fitting specs
                   nsrcfit=NA,
                   strt=1,
                   lb=0.01,
                   ub=15,
                   psearch=TRUE,
                   
                   ## data (averages)
                   avgs.name='',
                   avgs.col.name='avg',
                   loc.col.name='loc',
##                   unc.col.name=NA,  ## probably not used anymore
                   locs.to.use='all',
                   ## raw data (optional)
##                   raw.data.name='',
                   
                   ## options
                   is.final=FALSE,
                   pct.diff.criterion=20,
                   datestamp.output='n'
                   ) {

  ## if the params is not supplied, use the others to create a list named params
  if (!is.list(params)) {
    cat('[cappar] Creating params list using supplied (non-list) arguments\n')

    params <- list(run.code=run.code,
                   run.description=run.description,
                   cap.cons=cap.cons,
                   nsrcfit= nsrcfit,
                   strt=strt,
                   lb=lb,
                   ub=ub,
                   psearch=psearch,
                   avgs.name=avgs.name,
                   avgs.col.name=avgs.col.name,
                   loc.col.name=loc.col.name,
##                   unc.col.name=unc.col.name,
                   locs.to.use=locs.to.use,
##                   atres.df.name=raw.data.name,
                   is.final=is.final,
                   pct.diff.criterion=pct.diff.criterion,
                   datestamp.output=datestamp.output,
                   model.type='cap88')
  }
  
  ## but some can easily have defaults calculated if missing
  default.pars <- list(
                    run.code='no.run.code',
                    run.description='Run description not supplied',
                    datestamp.output='n',
                    lb=.01,
                    ub=15,
                    strt=1,
                    psearch=TRUE,
                    locs.to.use='all',
                    pct.diff.criterion=20,
                    is.final=FALSE,
                    model.type='cap88')

  
  ## identify the require elemens of params
  req.params <- c('run.code',
                  'run.description',
                  'model.type',
                  'datestamp.output',
                  'cap.cons',
                  'nsrcfit',
                  'nsources',
                  'nfixed',
                  'run.srcs',
                  'lb',
                  'ub',
                  'strt',
                  'psearch',
                  'avgs.name',
                  'avgs.col.name',
                  'loc.col.name',
##                  'unc.loc.name',
                  'locs.to.use',
                  'pct.diff.criterion',
                  'is.final'
                  )

  
  ## at this point, params will be a list, either because it was supplied as a list,
  ## or because a list was created from the individual args
  
  ## fill in missing elements (if any) using default values (if one is defined)
  pars.to.add <- setdiff(names( default.pars), names(params) )
  
  if (length(pars.to.add) > 0) {
    ## found at least one missing param to added
    cat('[cappar] Adding default params to supplied params:',pars.to.add,'\n')
    params <- c(params, default.pars[pars.to.add])
  }
  
  ## validity checks

  ## check that cap.cons is a list of 'capcon' objects
  ##   names of the list elements will be ignored (should be)
  if (!is.list(params$cap.cons)) {
    stop('[cappar] cap.cons must be a list or a "caplist" object',call.=FALSE)
  } else {
    ccls <- unlist(lapply(params$cap.cons,class))
    if (!all(ccls=='capcon')) {
      stop('[cappar] All elements in arg cap.cons must be "capcon" objects',call.=FALSE)
    }
  }

  ## required with no default
  if (is.na(params$nsrcfit)) {
    stop('[cappar] nsrcfit is required',call.=FALSE)
  }

  if (params$avgs.name=='') {
    stop('[cappar] avgs.name is required',call.=FALSE)
  }

  ## will check later that the named columns exist in the data frame identified
  ## by params$avgs.name
  ##  avgs.col.name='avg',
  ##  loc.col.name='loc',

  
  ## fill in some params that are calculated from others
  ## expand inputs of length 1 to length nsrcfit if needed
  params$nsources <- length(params$cap.cons)
  params$nfixed <- params$nsources-params$nsrcfit

  ## if lengths are too short, silently extend to correct length
  if (length(params$strt) < params$nsrcfit) params$strt <- rep(params$strt, len=params$nsrcfit)
  if (length(params$lb) < params$nsrcfit) params$lb <- rep(params$lb, len=params$nsrcfit)
  if (length(params$ub) < params$nsrcfit) params$ub <- rep(params$ub, len=params$nsrcfit)


  ## source names (requires that "capcon" objects have a site name
  params$run.srcs <- unlist(lapply(params$cap.cons, function(x) x$site))
  
  ## check for missing params
  pars.missing <- setdiff(req.params, names(params))
  if (length(pars.missing) > 0) {
    cat('[cappar] Missing parameters:\n  ',pars.missing,'\n')
  }


  class(params) <- 'cappar'

  params
}
