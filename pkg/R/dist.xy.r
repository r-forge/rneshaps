## cumulative distances from one point to the next
## 12/6/12 return 0 when only one point

## LLNL-CODE-637312

## example
##   tmp <- list(x=1:3, y=c(1,4,5))
##   foo <- fill.xy(tmp,3)
##   dist.xy(tmp)
##   [1]  0.000000 3.162278 4.576491
##  
##   dist.xy(foo)
##   [1] 0.000000 1.054093 2.108185 3.162278 3.633682 4.105087 4.576491
dist.xy <- function(xy) {
  n <- min(length(xy$x),length(xy$y))
  if (n==1) return(0)
  d <- numeric(n)
  d[1] <- 0
  for (i in 2:n) d[i] <-
    sqrt( (xy$x[i]-xy$x[i-1])^2 + (xy$y[i]-xy$y[i-1])^2 )
  cumsum(d)
}
