
## LLNL-CODE-637312

## given a list of form list(x=,y=)
## fill in the spaces between consecutive x,y pairs with additional
## equally spaced points (including the input pairs)
## example
##   tmp <- list(x=1:3, y=c(1,4,5))
##   foo <- fill.xy(tmp,3)
##   points(foo,col=2)

fill.xy <- function(xy,nfill=20) {
  n <- min(length(xy$x),length(xy$y))
  xy <- list(x=xy$x[1:n],
             y=xy$y[1:n])
  
  list( x=
       approx(seq(1, by = nfill, len = length(xy$x)),
              xy$x,
              1:(nfill * (length(xy$x) - 1) + 1))$y,
       y=
       approx(seq(1, by = nfill, len = length(xy$y)),
              xy$y,
              1:(nfill * (length(xy$y) - 1) + 1))$y
       )
}

