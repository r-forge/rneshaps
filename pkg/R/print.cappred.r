## 5/22/13
## taken out of predict.caplist.r and put it its own file

## LLNL-CODE-637312

print.cappred <- function(x,...) {
  cat('Predicted values at',x$ndata,'locations\n')
  cat(x$predicted,'\n')
  cat('\n')
  if (class(x$predict.at.xy) == 'SpatialPointsDataFrame') {
    tmp <- x$predict.at.xy
    tmp@data <- cbind(tmp@data,predicted=signif(x$predicted,3))
  } else {
    tmp <- cbind(data.frame(x$predict.at),predicted=signif(x$predicted,3))
  }
  
  if (x$ndata <= 30) print(tmp) else {
    cat('First 30 rows\n')
    print(head(tmp,30))
  }
  cat('\n')
}
