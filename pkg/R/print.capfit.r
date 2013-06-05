## 11/21/12
## print a "capfit" object returned by the capfit() function

## LLNL-CODE-637312

## change plotit option to integers for 2 or more types of plots,
## i.e., image plot and barplot

print.capfit <- function(x,... , print.fitted=TRUE, plotit=FALSE) {
  cat('\n')
  cat('------------ results of capfit ------------\n')
  cat('run code:',x$fbase,'\n')
  cat(x$runtime,'\n')
  cat('input params from:',x$param.arg.name,'\n')
  cat('description:',x$run.description,'\n')
  cat('\n')
  cat('Estimated source terms (scls)\n')
  print(x$scls)
  cat('\n')
  cat('rms: ',signif(x$rms,3),'\n')
  cat('\n')
  cat('Sources:\n')
  print(x$run.srcs)
  cat('\n')

  if (print.fitted) {
    print(x$fitpred)
    cat('\n')
  }
  
  if (plotit) print.capfit(x)

}
