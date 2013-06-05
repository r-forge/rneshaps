## 1/10/13
## nice printing for results returned by cfitfun(  , returnall=TRUE)

## LLNL-CODE-637312

print.cfitobj <- function(x,...) {
  cat('\n---- cfitfun results ----\n')
  cat('run code:',x$run.code,'\n')
  cat('sources:\n')
  cat(paste(names(x$cap.fitted),collapse=', '),'\n')

  cat('\n')
  fmat <- rbind(x$sampler.avgs, round(x$model.avgs,3))
  rownames(fmat) <- c('Sampled','Modeled')
  print(fmat)

  cat('\nCoef:\n')
  print(round(x$scls,3))

  cat('\nrms:',signif(x$rms,3),'  ss:',signif(x$ss,3),'\n')
}
