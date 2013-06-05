## 11/19/12
## the function version of fitavgs.r

## LLNL-CODE-637312


## I have a coordinate system issue
##   I have been doing CAP88 in a state plane system
##   I have been doing AERMOD in a UTM system
## Now I have to generalize.
## Or better, insist on dealing with only one coordinate system at a time

## 9/26/12
## this block of code is common to afitit.r and cfitit.r
## therefore, source it from them, rather than have it in both

## params is a parameters object from capfit() or aerfit()

get.cavgs <- function(params) {

  avgs.name <- params$avgs.name
  loc.col.name <- params$loc.col.name
  avgs.col.name <- params$avgs.col.name
  locs.to.use <- params$locs.to.use
  
  ## 9/17/12 now assume object named by avgs.name is a spatial object
  ## afitfun() does not need coordinate of samplers
  ##     avgs.to.fit.sptl <- get(avgs.name,'.GlobalEnv')

  ## 1/10/13 converting to non-Spatial (see new comments in capfit.r)
  avgs.to.fit <- get(avgs.name,'.GlobalEnv')

  ## check for Spatial class
  ##   expected to be SpatialPointsDataFrame, will be returned unchanged if not
  ##   Rnes.sp.to.df returns a list with a data frame, the proj4string, and the colnames of the coords
  if (grepl('Spatial',class(avgs.to.fit)))  {
    cat('\n')
    cat('[get.cavgs] Converting averages from',class(avgs.to.fit),'to simple data frame\n')
    cat('   copying from: ',avgs.name,'\n')
    cat('     copying to:  avgs.to.fit\n')
    avgs.cnvrt <- Rnes.sp.to.df(avgs.to.fit)
    avgs.to.fit <- avgs.cnvrt$df
  } else avgs.cnvrt <- list(prj=NULL, coln=NULL, Spatial.input=FALSE)
  
  if (locs.to.use=='all') locs.to.use <- avgs.to.fit[[loc.col.name]]

  ## 9/17/12 check whether samplers to fit are all available in the data
  if (!all(  locs.to.use %in% avgs.to.fit[[loc.col.name]] )) {
    cat('[get.cavgs] at least one specifed sampling location does not have data\n')
    cat('     ',setdiff(locs.to.use, avgs.to.fit[[loc.col.name]]),'\n')
    cat('    Stopping in get.cavgs()',call.=FALSE)
  }

  ## subset averages to those that are specified in locs.to.use
  ## (then when fit is done and we build a dataframe that has sampler averages and final model averages, use avgs.to.fit)
  avgs.to.fit <- avgs.to.fit[ avgs.to.fit[[loc.col.name]] %in% locs.to.use ,]
  navgs.to.fit <- nrow(avgs.to.fit)

  ## 11/19/12
  ##   I suspect that some other part of the process expects to find these in the GlobalEnv
  ##   if so, I'll have to find a different way to pass them to that process
  ##   Probably by returning a list() here
  
  ## put them in the order specified in locs.to.use
  avgs.to.fit <- avgs.to.fit[ match(locs.to.use, avgs.to.fit[[loc.col.name]]),]

  cat('\n')
  cat('[get.cavgs] Making named numeric vector of sampler averages\n')
  cat('      copying from: ',loc.col.name,'in avgs.to.fit\n')
  cat('        copying to:  avgs.with.names\n')
  
  ## pull the averages out in to a vector with names
  ## subset to specified locations and to specified order (i.e., per locs.to.use)
  ## (this syntax keeps them in the order they are in avgs.to.fit)
  avgs.with.names <- avgs.to.fit[[avgs.col.name]]
  names(avgs.with.names) <- avgs.to.fit[[loc.col.name]]
  
  cat('[get.cavgs] Done\n\n')

  ## MUST change this so that don't need versions in different projections
  c(list(locs.to.use=locs.to.use,
         avgs.with.names=avgs.with.names,
         avgs.to.fit=avgs.to.fit,
         navgs.to.fit=navgs.to.fit),
    avgs.cnvrt[c('prj','coln','Spatial.input')]
    )
}
