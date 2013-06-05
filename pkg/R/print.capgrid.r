## 12/6/12
## nice printing for capgrid obect

## LLNL-CODE-637312

print.capgrid <- function(x,...) {
  cat('------ capgrid object ------\n')
  cat('x range: ',x$xr,'\n')
  cat('y range: ',x$yr,'\n')
  cat('dim: ',length(x$x),'by',length(x$y),'\n')
  cat('plim: ',x$plim,'\n')
  invisible()
}
