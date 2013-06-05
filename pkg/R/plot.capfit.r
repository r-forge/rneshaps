## 1/10/13
## adapted from print.capfit

## LLNL-CODE-637312

## 11/21/12
## print a "capfit" object returned by the capfit() function

## change plotit option to integers for 2 or more types of plots,
## i.e., image plot and barplot

plot.capfit <- function(x,which=c(1L,2L), anno=TRUE) {

  npl <- length(which)

  if (any(which==1L)) {
    ## field plot
    if (require(fields)) {
      image.plot(x$fitfield,asp=1,main=x$run.code)
      pusr <- par()$usr
      if (anno) text(pusr[1],pusr[4],paste('rms =',signif(x$rms,3)),adj=c(-0.5, 1.5))
    } else {
      image(x$fitfield,asp=1)
      mtext(x$run.code)
      if (anno)  mtext(paste('rms =',signif(x$rms,3)),adj=0)
    }

    if (npl > 1) readline('CR to continue')
  }
  if (any(which==2L)) {
    ## barplot
#    tmpbar <- t(as.matrix(x$fitpred[,c('model.avgs',x$avgs.col.name)]))
    tmpbar <- t(as.matrix(cbind(Model= x$fitpred$model.avgs,
                                Data = x$fitpred[[x$avgs.col.name]])
                          ))
    colnames(tmpbar) <- x$fitpred[[x$loc.col.name]]
    yrng <- c(0,max(tmpbar)*1.1)
    
    barplot(tmpbar , beside=TRUE, legend.text=TRUE,
            ylab=if (x$to.bq) 'Bq/m3' else 'pC/m3',
            cex.axis=.75, cex.names=.75, las=2,
            args.legend=list(x='topleft'),
            ylim=yrng)
    if (anno) {
      mtext(paste('rms =',signif(x$rms,3)),adj=0)
    }
  }
  
  invisible()
}
