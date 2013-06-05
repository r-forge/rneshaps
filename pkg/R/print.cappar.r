## 11/21/12
## nicely formatted print of "cappar" object from function cappar()

## LLNL-CODE-637312

print.cappar <- function(x,...) {

  ## relatively nicely formatted version of params, to print for inspection of params
  infofun <- function(le) {
    if (is.list(le)) return(paste('list of length',length(le),' names (if any) are:',paste(names(le),collapse=', ')))
    if (length(le)>1) return(paste(le,collapse=', '))
    return(as.character(le))
  }

  tmpvl <- unlist(lapply(x,infofun))
  tmppar <- cbind( Param=names(tmpvl), Value=tmpvl)
  rownames(tmppar) <- NULL
  cat('\n')
  print(tmppar,quote=FALSE)
  cat('\n')

}
