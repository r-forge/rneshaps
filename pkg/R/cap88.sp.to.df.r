## 1/10/13

## LLNL-CODE-637312

## helper function for get.cavgs()

Rnes.sp.to.df <- function(spdf) {
  
  require(sp)

  icl <- class(spdf)
  if (icl != 'SpatialPointsDataFrame') {
    cat('[Rnes.sp.to.df] Input is class ',icl,', not SpatialPointsDataFrame, returning it unchanged\n',sep='')
    return(spdf)
  }
  ## assumes the input is a SpatialPointsDataFrame
  prj <- proj4string(spdf)
  coln <- colnames(spdf@coords)
  colnames(spdf@coords) <- c('x','y')
  df <- as(spdf,'data.frame')

  ## fix the fact that as() converts character to factor
  cclasses <- lapply(spdf@data,class)
  for (nm in names(cclasses)) if (cclasses[[nm]] == 'character') df[[nm]] <- format(df[[nm]])

  list(df=df,prj=prj,coln=coln, Spatial.input=TRUE)
}
