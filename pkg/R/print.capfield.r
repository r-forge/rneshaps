## 12/6/12
## nice printing for 'capfield' objects
## created by one of cap.add() cap.i() cap.ii()

## LLNL-CODE-637312

print.capfield <- function(x,...) {
  cat('------ capfield object ------\n')
  cat('site: ',x$site,'\n')
  cat('center coords (offset)\n')
  print(x$offset)
  cat('\n')
  cat('plim: ',x$plim,'\n')
  cat('  zr: ',x$zr,' (log10 units)\n')
  cat('\n')
  
  invisible()
}
  
