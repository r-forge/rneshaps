## 1/10/13
## nicely print a caplist object; see caplist()

## LLNL-CODE-637312

print.caplist <- function(x,...) {
  nobj <- length(x)
  cat('\n')
  cat('A list of',nobj,'capcon objects\n\n')

  for (i in seq(nobj)) {
    cat('-----',x[[i]]$site,'-----\n')
    cat('center coords (offset)\n')
    print(x[[i]]$offset)
    cat('range of concentrations\n')
    cat(range(x[[i]]$cap),'\n')
    if (x[[i]]$to.bq) cat('(conentrations converted to Bq assuming input in pCi)\n')
    cat('\n')
  }
  invisible()
}
