## 4/4/14
## calculate contour lines for a capfit object

## LLNL-CODE-637312


#obj <- fitb

contour.capfit <- function(obj,
                           clevs = pretty(obj$fitfield$zr),
                           add = TRUE,
                           
                           lines.shpf.out=FALSE,
                           shpf.dir='.',
                           shpf.name=make.names(obj$run.code),
                           
                           lines.kml.out=FALSE,
                           image.kml.out=FALSE,
                           kml.dir='.',
                           kml.cut = -3
                           ) {

  ## initial version
  ##   always makes contour lines
  ##   always returns an object that can be added to an existing plot
  ##      optionally adds that object to an existing plot
  ##   optionally writes the contour lines to a KML file
  ##   optionally creates an image-like KML object (many small squares)
  ##      optionally writes it to a kml file
  ##
  ##      BUT: I should probably separate the KML version into another function
  ##           or perhaps rename the function to be more general?
  ##           but for now, keep all of the pieces

  ## obj$fitfield is a list with (among others)
  ##   x,y,z    (suitable for input to image, contour)
  ##   offset
  ##   site

  cat('\n[contour.capfit] Starting\n')

  ## during development
  ## add <- TRUE
  ## shpf.dir <- kml.dir <- 'tmp'
  ## lines.kml.out <- FALSE
  ## image.kml.out <- FALSE
  ## lines.shpf.out <- TRUE
  ## kml.cut <- -3


  ## contour levels that exceed the range of the data (log 10 units)

  crs.obj <- CRS(obj$pr)

  ## may want to give more flexibility?
  nms <- obj$run.code
  nmr <- make.names(nms)       ## shouldn't be needed, but in case I make a weird run.code
  nmc <- paste(nmr,'.contour',sep='')
  nmf <- paste(nmr,'.field',sep='')

  if (lines.kml.out | image.kml.out) {
    ## for KML output
    ## all of this requires Spatial data
    if (!obj$Spatial.input) {
      cat('[contur.capfit] input data must be of Spatial class for KML output. Stopping.\n')
      stop('',call.=FALSE)
    }
    
    ## extract current fit to spatial data frame
    tmpsp <- xyzimg.to.xyzdf( obj$fitfield )
    coordinates(tmpsp) <- c('x','y')
    proj4string(tmpsp) <- obj$prj

    ## need to define colors (this should be in a separate file, or where it can be commonly available
    ## tmpsp$z is in log10 units
    nbrks <- 64
    require(fields)  ## for tim.colors
    ccols <- tim.colors(nbrks)
    cbrks <- seq(min(tmpsp$z,na.rm=TRUE),max(tmpsp$z,na.rm=TRUE),len=nbrks+1)
    cbrks[1] <- -Inf
    cbrks[nbrks+1] <- Inf

    ind <- cut(tmpsp$z, cbrks, include.lowes=TRUE)
    icol <- ccols[match(as.character(ind),levels(ind))]

    tmpsp$color <- icol

    cat('[contour.capfit] Preparing for KML version of model field:',nms,'\n')
    tmppx <- as(tmpsp, "SpatialPixelsDataFrame")

    ##  cat('[aerforge.r] Convert to spatial grid\n')
    tmpgrdf <- as(tmppx, "SpatialGridDataFrame")

    ## work-around  (conversion to spatial grid converted character column to factor column
    tmpgrdf$color <- format(tmpgrdf$color)

    ##  find a way to create KML without converting to polygons (maybe plotKML?)
    cat('[contour.capfit] Converting SpatialGridDataFRame to SpatialPolygonsDataFrame (slow)\n')
    sgrdp <- as(tmpgrdf,'SpatialPolygonsDataFrame')

    ## only send to kml grid locs where conc is "large"
    kml.cut <- -3    ## log units
    is.large <- tmpgrdf$z > kml.cut
    is.large[is.na(is.large)] <- FALSE
    cat('[contour.capfit] count of "large" concentrations >',kml.cut,'\n')
    print(table( ifelse(is.large,'large','small')))
    cat('\n')
  }

  cat('[contour.capfit] Contour lines\n')
  tmp.cl <- contourLines(obj$fitfield,levels=clevs)

  if (obj$Spatial.input) {
    tmp.cl <- ContourLines2SLDF(tmp.cl)
    proj4string(tmp.cl) <- obj$prj

    ## option to plot on-screen
    if (add)  {
      ##  image.plot(obj$fitfield)
      plot(tmp.cl,add=TRUE)
    }
  
    if (image.kml.out) {
      cat('[contour.capfit] Writing spatial polygons data frame to KML (may take a long time)\n')
      spdf.to.kml( spTransform( sgrdp[is.large,], crs.ll), file.path(kml.dir,nmf),
                  Docname=nmf,layer.name=nmf,outline=FALSE, alpha=0.4)
    }

    if (lines.kml.out) {
      cat('[contour.capfit] contour lines\n')
      spln.to.kml(spTransform(tmp.cl,crs.ll),
                  file.path(kml.dir,nmc),nmc,description=paste('Contours of',nmr))
    }


    if (lines.shpf.out) {
      ## same coord system as the input fit object
      writeOGR(tmp.cl,shpf.dir,nmr,'ESRI Shapefile', overwrite_layer=TRUE)
    }

  } else {
    ## not yet developed as of 4/5/13
    ## might use lines(), will have to research

    ## just need to fill in
    if (add) {
      cat('[contour.capfit] add option for non-Spatial not implemented yet\n')
    }
  }

  ## make invisible for use when add=TRUE
  invisible(tmp.cl)
}
