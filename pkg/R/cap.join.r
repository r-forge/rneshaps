##
## cap.join
##

## LLNL-CODE-637312

## join two capcon objects together into a single one
## use whent he same model is run more than once, each time with
## a different set of distances
## for example one run from 100 to 2000 meters, a second run from 2100m to 4000m
## (because CAP88 PC has a max number of distances it can model in a single run)

## The assumption is that the several objects have different sets of distances
## and that they are supplied in order from small to large
## Duplicated distances are removed

## 10/25/12 not true anymore. Now check for and correct if supplied out of order.
## NOTE:  objects must be supplied in order of distances, small to large

## 4/18/12
## assign a class to the output

## function is designed for input of form  (form 1)
##   cap.join(obj1, obj2)
## where each object is of class capcon, created by getcon()
## but there is also a test for this form (form 2)
##   cap.join( list(obj1, obj2) )
## and an adjustment if found

## not that class 'capcon' objects are also lists

cap.join <- function(...) {
  ## ... should be provided in the form of
  ##   tmp1,tmp2
  ## where tmp1 and tmp2 are the output of getcon()  [lists with a getcon structure]

  ## this much is adequate for input form 1
  caps <- list(...)

  ## test and adjust for form 2
  if (length(caps) == 1 && is.list(caps) && class(caps) != 'capcon') {
    caps <- caps[[1]]
  }

  ncaps <- length(caps)
  cat('[cap.join] joining',ncaps,'objects\n')

  if (ncaps==1) {
    cat('[cap.join] Need to supply 2 or more capcon objects.\n  Stopping\n')
    stop('',call.=FALSE)
  }

  ## joining is supposed to be done for objects where the only model difference is the
  ## set of distances. Date and other annotative information may be different, and of
  ## course the concentrations will be differnt.
  ## Any element that is not updated in the loop below will have the value supplied in
  ## the first input object.

  ## Start with the first element
  cout <- caps[[1]]

  if (class(cout) != 'capcon') {
    cat('[cap.join] ERROR: first element in input is not a "capcon" object. Stopping,\n')
    stop('',call.=FALSE)
  }

  ## append information from additional objects
  ## assumes there are always at least two objects supplied in ...
  
  for (i in 2:ncaps) {

    if (class(caps[[i]]) != 'capcon') {
      cat('[cap.join] ERROR: element',i,'in input is not a "capcon" object. Stopping.\n')
      stop('',call.=FALSE)
    }
 
    ## assume distances are in order, and user supplied in order from smallest to largest
    ## but the last of one may be the same as the first of the next
    cout$dists <- c(cout$dists, caps[[i]]$dists)
    cout$ndist <- cout$ndist +  caps[[i]]$ndist

    cout$dmat <- rbind(cout$dmat, caps[[i]]$dmat)
    cout$ddf <- rbind(cout$ddf,  caps[[i]]$ddf)

    cout$name <- paste(cout$name, caps[[i]]$name )
    cout$desc <- paste(cout$desc, caps[[i]]$desc )

    cout$x <- cbind( cout$x, caps[[i]]$x )
    cout$y <- cbind( cout$y, caps[[i]]$y )
    cout$cap <- cbind( cout$cap, caps[[i]]$cap )

    cout$capdf <- rbind(cout$capdf, caps[[i]]$capdf)

    cout$dist <- c(cout$dist, caps[[i]]$dist)
    
  }

  ## 10/25/12
  ## add sort by distance
  dord <- order(cout$dists)
  if (any(diff(dord) < 0)) {
    ## vectors; rows correspond to differences
    cout$dists <- cout$dists[dord]
    cout$dist <- cout$dist[dord]
    cout$dmat <- cout$dmat[dord,]
    cout$ddf <- cout$ddf[dord,]
    ## columns correspond to distances
    cout$x <- cout$x[, dord]
    cout$y <- cout$y[, dord]
    cout$cap <- cout$cap[, dord]
  }

  ## currently should never be more than one nuclide, but just in case ...
  cout$capdf <- cout$capdf[ order(cout$capdf$nuclide,
                                  factor(cout$capdf$dir, levels=cout$dirs) ,
                                  cout$capdf$dist) ,
                           ]
                                  
  
  ## 10/25/12 this used to be inside the loop, but just as good outside
  ## now check for duplicate distances, drop rows/columns as appropriate
  dup.dists <- duplicated(cout$dists)
  cout$dmat <- cout$dmat[ !dup.dists ,]
  cout$ddf <-   cout$ddf[ !dup.dists ,]

  cout$x   <-   cout$x[ , !dup.dists ]
  cout$y   <-   cout$y[ , !dup.dists ]
  cout$cap <- cout$cap[ , !dup.dists ]

  cout$dist  <- cout$dist[ !dup.dists ]
  cout$dists <- cout$dists[ !dup.dists ]
  cout$ndist <- length(cout$dists)

  
  cout$info <- c(cout$name, cout$date, cout$windfile)

  ## same class as the input -- same structure, just joined two or more capcon objects
  class(cout) <- 'capcon'
  
  cout
}
