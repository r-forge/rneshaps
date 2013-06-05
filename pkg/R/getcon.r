## function to read CAP88 PC version 1 concentration output files
## input files named like  'AAAAD.CON' (cap88 convention)

## LLNL-CODE-637312

## 4/18/12
## assign a class to the output

## 10/25/12
## make input file naming less idiosyncratic
## make it so the directory name and the file name are not both required,
## i.e., can supply entire path as infile

## remove requirement for informational output to infodir
## (but allow backward compatibility)

## option for source.coords to be data.frame or SpatialPointsDataFrame

## add some validity checks

## remove obolete commented-out code


## 5/7/10
## get rid of the suffix argument

## 4/18/08
##   rename this file to getcon.r (from capcon.r)

## 4/17/08
## complete re-write, to read CAPP88 .CON output files

## 5/6/09
## updating for coordinate systems based on my new Spatial object data

## 12/20/12
## add option to convert concentration output from pCi to Bq
## (assumes CAP88 PC v1 always outputs in pCi; this assumption has NOT been checked

getcon <- function(infile='path.to.file',
                   site='',
                   source.coords=srcs.lm,
                   site.var='Location',
                   convert.to.dose=FALSE,
                   dose.to.conc=134.7*.37,
                   to.bq=FALSE,
                   datadir=NULL,
                   infodir=NULL) {
  
  
  ## (note, no test for whether datadir is character, which it must be
  if (is.null(datadir)) input.file <- infile else input.file <- file.path(datadir,infile)

  if (length(input.file) > 1) {
    ## not sure how this can happen, but maybe it can; revealed during R CMD check --as-cran
    cat('[getcon] input.file somehow has length > 1.\n',paste(input.file,collapse=' '),'\n')
    cat('  Using only the first element\n')
    input.file <- input.file[1]
  }
  
  if (!file.exists(input.file)) {
    cat('[getcon] input file ',infput.file,' not found\n  Stopping\n')
    stop(call.=FALSE)
  }

  cat('[getcon()] Input data file:\n')
  cat('           ',input.file,'\n')

  if (is.character(infodir)) {
    ## output files (though in R I won't need these)
    infilesp <- gsub(' ','_',infile)
    info.file <- file.path(infodir,paste(infilesp,'_info',sep=''))
    dist.file <- file.path(infodir,paste(infilesp,'_dist',sep=''))
    conc.file <- file.path(infodir,paste(infilesp,'_conc',sep=''))

    cat('[getcon] See informational output files:\n')
    cat('           ',info.file,'\n')
    cat('           ',dist.file,'\n')
    cat('           ',conc.file,'\n')
    cat('\n')
  }
  
  ##
  ## load the .con file into a character vector, one element per line
  ##
  inl <- scan(input.file, what='', sep='\n', strip.white=TRUE)
  dsnm <- inl[grep('Dataset Name',inl)]
  dsdt <- inl[grep('Dataset Date',inl)]
  dswf <- inl[grep('Wind File',inl)]
  
  if (is.character(infodir)) {
    sink(info.file)
    cat(dsnm,'\n')
    cat(dsdt,'\n')
    cat(dswf,'\n')
    sink()
  }
  
  dsnm <- gsub('Dataset Name:','',dsnm)
  dsdt <- gsub('Dataset Date:','',dsdt)
  dswf <- gsub('Wind File:','',dswf)
  ## now get rid of leading spaces
  dsnm <- gsub('^[[:blank:]]*','',dsnm)
  dsdt <- gsub('^[[:blank:]]*','',dsdt)
  dswf <- gsub('^[[:blank:]]*','',dswf)
  
  ## we're going to assume a stable CAP88 PC output file structure
  ## good assumption for CAP88 PC v1
  
  dirs <- c('N','NNW','NW','WNW','W','WSW','SW','SSW','S','SSE','SE','ESE','E','ENE','NE','NNE')
  ndir <- length(dirs)

  ## an example input file is in the "inst" package directory
  ## find all the lines that begin with a direction specification (N, NNW, etc.)
  ## these are the lines that have the data we want
  ## the string to grep for (gstr) will be of the form:
  ##   ^NNE[[:blank:]]
  ## that is, one of the directions at the beginning of the line, followed by at least one blank

  indd <- numeric()
  for (dir in dirs) {
    gstr <- paste('[',dir,'[:blank:]]',sep='')
    gstr <- paste('[',dir,'][[:blank:]]',sep='')
    gstr <- paste('^',dir,sep='')
    gstr <- paste('^',dir,'[[:blank:]]',sep='')
##    cat(' grep string is:  ',gstr,'\n')
    indd <- c(indd, grep(gstr,inl))
  }
  indd <- sort(indd)

  ## line numbers of header lines, and logical vector to match
  ## the only possible use of this would be to verify that columns are
  ## in the expected order (see names(ddf) below)
  indh <- grep('Rate',inl)
  indh <- sort(c(indh, indh+1, indh-1, indh-2))
  useh <- seq(inl)[indh]

  ## this appears to be obsolete
  ##  doses <- list() ; length(doses) <- ndir ; names(doses) <- dirs

  zz <- textConnection(inl[indd])
  capdf <- read.table(zz,as.is=TRUE,header=FALSE)
  close(zz)

  ## note that these are hard-coded, which assumes the CON file structure is always the same
  capvars <- c('dir','dist','nuclide','conc','dry.dep.rate','wet.dep.rate','ground.dep.rate')
  names(capdf) <- capvars
  capdf$indx <- indd
  rownames(capdf) <- seq(nrow(capdf))

  ## sort in compass order, not alphabetical
  capdf <- capdf[order( ordered(capdf$dir, levels=dirs) , capdf$dist),]

  ## discovered a case where a distance was present twice
  capdf <- unique(capdf[,capvars])

  ## 12/20/12 unit conversion option, assumes CAP88 PC concentration output
  ## is pCi (per cubic meter), convert to Bq (per cubic meter)
  if (to.bq) capdf$conc <- capdf$conc * 0.037
  
  dists <- unique(capdf$dist) ## already sorted
  ndists <- length(dists)
  
  ## now need to convert to matrix
  ##   rows are directions (N, NNW, ... from top down)
  ##   columns are distances (shortest to longest, left to right)
  cap <- matrix(capdf$conc, nrow=16, byrow=TRUE)
  dimnames(cap) <- list( dirs, dists )

  ## other forms to have same structure as the function that read summary files
  dmat <- t(cap)
  ddf <- as.data.frame(dmat)

  ## these files are similar to what the old capmac SAS macro did
  ## they aren't needed anymore, but leave them in so that I can inspect them as
  ## a way to check that the above code did the right thing
  if (is.character(infodir)) {
    sink(conc.file)
    print(dmat)      ## change to write.table if needed to re-read
    sink()

    sink(dist.file)
    cat('[getcon] ',dists,'\n')
    sink()
  }
  
  ##
  ## gather info so far
  clist <- list(dirs=dirs,
                dists=dists,
                ndir=ndir,
                ndist=ndists,
                capdf=capdf,
                dmat=dmat,      ## not sure if this needs to be kept
                ddf=ddf,        ## not sure if this needs to be kept
                info=c(dsnm,dsdt,dswf),
                convert.to.dose=convert.to.dose,
                to.bq=to.bq,
                units={if (to.bq) 'Bq/m3' else 'pCi/m3'}
                )

  ## optional conversion to dose
  cap <- cap / ifelse(convert.to.dose,dose.to.conc,1)

  ## create cartesian coordinates
  ## cap data is ordered N, NNW, NW, etc., which is CCW around the compass, starting in north
  ## define angles as going CCW from east = 0, therefore north = 90
  ## thus x and y should be vectors of lenght 16, and the sequence of (x,y) coordinates should
  ## represent N, NNW, NW, ...
  ##   using %o% function, equivalent to outer( arg1, arg2 ), to multiply by distance vector
  angle <- seq(from=90,by=22.5,length=16)
  angle[angle>=360] <- angle[angle>=360]-360
  x <- cos(angle*pi/180) %o% dists  ; x <- round(x,2)
  y <- sin(angle*pi/180) %o% dists  ; y <- round(y,2)

  ## up to this point, (x,y) is relative to (0,0)
  ## now look up the site name in the source.coords object
  ## if input site is found in source.coords[[site.var]], center on its coordinates
  ## must find only one row, otherwise results are unpredictable and probably wrong
  ##   (not checking)
  if (site != '' && site %in% source.coords[[site.var]]) {

    if (class(source.coords)=='SpatialPointsDataFrame') {
      cat('[getcon] Transforming location coordinates to be in local coordinate system (meters)\n')
      ## if only one row is found, structure drops from matrix to vector
      d.from.c <- coordinates(source.coords)[source.coords[[site.var]]==site,]
      names(d.from.c) <- c('x','y')

    } else if (class(source.coords)=='data.frame') {
      ## source.coords can be a data frame with elements x, y, [[site.var]]
      if (!all(c('x','y') %in% names(source.coords))) {
        cat('[getcon] source.coords must contain variables names x, y\n  Stopping\n')
        stop(call.=FALSE)
      }
      cat('[getcon] Transforming location coordinates to be in local coordinate system (meters)\n')
      ## want a named vector; without the unlist() we get a data frame
      d.from.c <- unlist(source.coords[ source.coords[[site.var]]==site , c('x','y') ])
      
    } else {
      cat('[getcon] source.coords object must be a data.frame or a SpatialPointsDataFrame\n  Stopping\n')
      stop(call.=FALSE)
    }
          
  } else d.from.c <- c(x=0,y=0)

#### need to make d.from.c have the same structure whether from SPDF or data.frame
  x <- x+d.from.c['x']
  y <- y+d.from.c['y']
  
  ## construct full output object
  cap.out <- list(site=site,
                  name=dsnm,
                  date=dsdt,
                  windfile=dswf,
                  desc=NULL,
                  x=x,
                  y=y,
                  compass=dirs,
                  angle=angle,
                  dist=dists,
                  cap=cap,
                  measure=ifelse(convert.to.dose,'Dose from conc','Conc'),
		  offset=d.from.c
                 )

  tmp <- c(clist, cap.out)
  class(tmp) <- 'capcon'

  tmp
}


