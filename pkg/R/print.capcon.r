## 12/6/12
## nice printing of 'capcon' objects created by getcon()

## LLNL-CODE-637312

## mostly echos information from the header of the CAP88 PC concentration file

print.capcon <- function(x,...) {
  cat('------ capcon object ------\n')
  cat('site: ',x$site,'\n')
  cat('name: ',x$name,'\n')
  cat('date: ',x$date,'\n')
  cat('desc: ',x$desc,'\n')
  cat('windfile: ',x$windfile,'\n')
  cat('center coords (offset)\n')
  print(x$offset)
  cat('\n')
  cat('distances:\n')
##  print( matrix(x$dists,ncol=10, byrow=TRUE) )
  ndist <- x$ndist
  ilo <- seq(from=1, by=10, to=ndist-1)
  ihi <- ilo+9
  ihi <- pmin(ihi,ndist)
  for (ii in seq(ilo)) cat( x$dists[ilo[ii]:ihi[ii]],'\n')
  cat('\n')

  cat('directions:\n')
  tmp <- x$angle ; names(tmp) <- x$dirs
  print(sort(tmp))
  cat('\n')
  cat('head( $capdf):\n')
  print(head(x$capdf))
  cat('\n')
  cat('range of concentrations\n')
  cat(range(x$cap),'\n')
  if (x$to.bq) cat('(conentrations converted to Bq assuming input in pCi)\n')
  cat('\n')
  invisible()
}

## > lapply(bxx1p,class)
## $dirs  "character"
## $dists "integer"
## $ndir "integer"
## $ndist "integer"
## $capdf "data.frame"
## $dmat "matrix"
## $ddf "data.frame"
## $info "character"
## $convert.to.dose "logical"
## $site "character"
## $name "character"
## $date "character"
## $windfile "character"
## $desc "character"
## $x "matrix"
## $y "matrix"
## $compass "character"
## $angle "numeric"
## $dist "integer"
## $cap "matrix"
## $measure "character"
## $offset "numeric"


## > names(bxx1p)
##  [1] "dirs"  "dists" "ndir"  "ndist" "capdf" "dmat"  "ddf"   "info" 
##  [9] "convert.to.dose" "site"  "name"  "date"  "windfile"   "desc"  "x" "y"    
## [17] "compass"    "angle" "dist"  "cap"   "measure"    "offset"    
