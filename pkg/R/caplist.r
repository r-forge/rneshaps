## 1/10/13
## like
##   list(bxx1d, bxx2d, bxx3d, bxx4d, bxx5d,   bxx1p, bxx2p, bxx6p)
## but gives it a class name of 'caplist'

## LLNL-CODE-637312

caplist <- function(...) {
  cl <- list(...)
  class(cl) <- 'caplist'
  cl
}
