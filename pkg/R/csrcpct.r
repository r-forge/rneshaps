## 1/11/13
## renamed to csrcpct() from cfitinfo()
## the new name better represents what the function does

## LLNL-CODE-637312

## 5/11/09
## get info from output of cfitfun(  ,returnall=TRUE)

## cfitfun( , returnall=FALSE) is called repeatedly by the optimizer routine in capfit(),
## after a fit is complete, capfit() calls cfitfun(  ,returnall=TRUE)
## to get additional information about the fit
##
## this function extracts such information and returns it in data frame format


## note that this will interpolate to and from a different grid than is used
## in cfitfun (which uses the "reference grid")
## so interpolated values at samplers will not be identical

csrcpct <- function( cfit , type='pct') {
  ##  > names(tmp)
  ## [1] "BXX1 diffuse" "BXX3 diffuse" "BXX5 diffuse" "BXX4 diffuse" "DWTF diffuse" "BXX1 stack"   "BXX2 stack"
  ##     "cap.added"    "aterp.at.xy"         "model.avgs"   "ss"           "rms"          "scls"        

  fitmat <- matrix(NA, nrow=cfit$ndata, ncol=cfit$nmodels)
  colnames(fitmat) <- names(cfit$cap.fitted)
  
  for (i in seq(cfit$nmodels)) {
    tmpi <- cap.i( cfit$cap.fitted[[i]], ngrid=400)
    aterp.at.xy <- aterp( tmpi, cfit$fit.at.xy, nfill=0)
    fitmat[,i] <- 10^aterp.at.xy$xyz$z
  }

  fitmat <- cbind(round(fitmat,5),
                  'sum'=round(rowSums(fitmat),3),
                  'model.avgs'=round(cfit$model.avgs,3),
                  'field'=cfit$sampler.avgs )

  ## cfit$sampler.avgs is renamed in aermod output as $fit.to.avgs
  
  ## percentage contributions
  pctmat <- round(100*fitmat[,1:cfit$nmodels] / fitmat[,cfit$nmodels+1],1)
  
  if (is.character(names(cfit$sampler.avgs))) locs <- names(cfit$sampler.avgs) else locs <- rep('',cfit$ndata)

  if (type=='pct') {
    fitdf <- cbind( loc=locs , data.frame(pctmat), stringsAsFactors=FALSE)
  } else {
    fitdf <- cbind( loc=locs , data.frame(fitmat), stringsAsFactors=FALSE)
  }

  rownames(fitdf) <-  NULL
  
  fitdf
}

