## 6/19/10
## convert x,y,z in dataframe form,
##   i.e., columns x,y,z
## into a list(x,y,z) suitable for image()

## LLNL-CODE-637312

## really want to have this in package rmacq

## 10/29/12 change to prefering columns named x and y in the input data frame

xyzdf.to.xyzimg <- function( df ) {
  ## df should be dataframe or matrix with columns x, y, z
  ## number of row = unique(x) * unique(y)
  ## first version assumes column order is x, y, z

  if (all( c('x','y','z') %in% names(df))) {
    df <- df[order(df$x, df$y) ,]
    xs <- df$x
    ys <- df$y
    zs <- df$z
  } else if (all( c('long','lat','z') %in% names(df))) {
    df <- df[order(df$long, df$lat) ,]
    xs <- df$long
    ys <- df$lat
    zs <- df$z
  } else {
    cat('[xyzdf.to.xyzimg] WARNING: elements x,y not found in input data frame\n')
    cat('    Assuming that the first column is x and the second is y\n')
    df <- df[order(df[,1], df[,2]) ,]
    xs <- df[,1]
    ys <- df[,2]
    zs <- df[,3]
  }
  
  xyz <- list(x=0,y=0,z=0)
  xyz$x <- unique(xs)
  xyz$y <- unique(ys)
  
  xyz$z <- matrix(zs, nrow=length(xyz$x), byrow=TRUE)
  xyz
}

df.to.img <- xyzdf.to.xyzimg


## test with:
#   tmp <- data.frame(x=rep(1:4,each=3) , y=rep(1:3,4) , z=1:12 )
#   tmpl <- as.list(tmp)
#   foo <- xyzdf.to.xyzimg(tmp)

#   par(mfrow=c(2,1))
#   plot(1:12,1:12, col = heat.colors(12), pch=17,cex=5)
#   image(foo)
#   text(tmpl, lab=tmpl$z)
#   par(mfrow=c(1,1))
##

## opposite conversion
xyzimg.to.xyzdf <- function(xyz) {
  dfo <- expand.grid(x=xyz$x, y=xyz$y)
  dfo$z <- as.vector(xyz$z)
  dfo
}

img.to.df <- xyzimg.to.xyzdf
