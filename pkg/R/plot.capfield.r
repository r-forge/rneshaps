## 1/12/13
## nice plotting for 'capfield' objects
## created by one of cap.add() cap.i() cap.ii()

## LLNL-CODE-637312

plot.capfield <- function(x,...) {

  if (require(fields)) {
    image.plot(x,asp=1,main=x$site)
  } else {
    image(x,asp=1)
    mtext(x$site)
  }
  
  invisible()
}
