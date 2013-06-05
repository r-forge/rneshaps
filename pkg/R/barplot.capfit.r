## 4/12/13
## function to plot barplots of model vs data in appropriate spatial relationship
## or add such to an existing spatial plot

## LLNL-CODE-637312

## 5/22/13
## making arg names conform to the generic barplot()
## as a result, the 'height' arg to this function is not analogous to 'height'
## in the R-supplied barplot(). Here, height is a capfit object
## Don't have complete conformity yet.
##  generic:  barplot(height,...)
##     mine:  barplot(height, add, {and so on}

## assume/require that if the fitted object (first arg) has
##    Spatial.input TRUE
## then all relevant data objects are Spatial

## needs more flexibility in what elements are present

##plot.capbar <- function(obj,
barplot.capfit <- function(height,
                           add=FALSE,
                           add.markers=TRUE,
                           add.locnames=TRUE,
                           locs.to.show='all',
                           add.legend=TRUE,
                           image=FALSE,
                           hadj=0.5,
                           vadj= -0.1,
                           size=c(0.2,0.75),
                           pars=NULL,
                           fence.obj=fenceline.ex) {

  if (class(height) != 'capfit') {
    cat('[barplot.capfit] The first arg ("height") must be a "capfit" object. Stopping\n')
    stop('',call.=FALSE)
  }
  
  avgs.obj <- get(height$avgs.name)

  if (height$Spatial.input) {
    ## avgs.obj should be a SpatialPointsDataFrame
    locs.xy <- coordinates(avgs.obj)
  } else {
    ## avgs.obj should be a data frame with coords in x,y
    locs.xy <- as.matrix( avgs.obj[,c('x','y')])
  }
  rownames(locs.xy) <- avgs.obj[[height$loc.col.name]]
  
  ## if locs.to.show is not 'all', it should be a character vector identifying a subset of locations
  if (locs.to.show[1] != 'all') {
    locs.xy <- locs.xy[ rownames(locs.xy) %in% locs.to.show ,]

    if (nrow(locs.xy) < 1) {
      cat('[barplot.capfit] No locations remaining after subsetting; locs.to.show =',
          paste(locs.to.show,collapse=' '),
          '. Stopping.\n')
      stop('',call.=FALSE)
    }

  } else locs.to.show <- rownames(locs.xy)
  
  ## what if I don't have a fence line -- have to start with data locations
  if (height$Spatial.input) {
    plot(fence.obj, usePolypath=FALSE, add=add)
    if (add.markers) plot(avgs.obj,add=TRUE, cex=0.8)
  } else {
    ## need an initial plot when add=FALSE
    plot(range(fence.height$x), range(fence.height$y), type='n', bty='n',xlab='',ylab='')
    polygon(fence.obj)
    ## non-spatial, use data.frame x,y
    if (add.markers) points(locs.xy,pch=3, cex=0.8)
  }


  if (add.locnames) text(locs.xy, rownames(locs.xy), pos=2, cex=0.65)

  gcol <- gray(0.8)

  if (add.legend) legend('topleft',c('Modeled','Measured'),fill=c(1,gcol), cex=0.75)

  ## bar plot
  tmpbar <- t(as.matrix(cbind(Model = height$fitpred$model.avgs, 
                              Data = height$fitpred[[height$avgs.col.name]])))
  colnames(tmpbar) <- height$fitpred[[height$loc.col.name]]
  ## have trailing space characters in names, not sure why
  colnames(tmpbar) <- sub("[[:space:]]+$", "", colnames(tmpbar))

  tmpbar <- tmpbar[, colnames(tmpbar) %in% locs.to.show]
  
  yrng <- c(0, max(tmpbar) * 1.1)

  ## use drop = FALSE in subset of tmpbar, to keep it as a matrix, which in turn
  ## causes it to use the same colors as in plot.capfit

  require(Hmisc)
  
  ## column in tmpbar
  for ( ic in seq(ncol(tmpbar))) {
    lnm <- colnames(tmpbar)[ic]
    xyi <- locs.xy[lnm,]

    subplot(
      {
        barplot(tmpbar[,ic,drop=FALSE],
                beside = TRUE,
                ##           ylab = if (height$to.bq) "Bq/m3" else "pC/m3",
                cex.axis = 0.75,
                cex.names = 0.75,
                las = 1, 
                ylim = yrng,
                xaxt='n',
                yaxt='n',
                col=c(1,gcol)
                )
      },
      x=xyi[1],
      y=xyi[2],
      size=size,
      vadj=vadj,
      hadj=hadj,
      pars=pars
      )
  }
}

