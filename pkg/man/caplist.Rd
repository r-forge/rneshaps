\name{caplist}
\alias{caplist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a "caplist" object, a list of "capcon"  objects.
}
\description{
Create a list of capcon objects, giving it a class of "caplist".
}
\usage{
caplist(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
One or (usually) more objects created by getcon()
}
}
\details{
The purpose of this function is to create a list of capcon objects, but
give it a class of "caplist". The purpose of this is so that there can
be a print.caplist() function that will print reasonable information
about the list.
}
\value{
A list of capcon objects, suitable for input to capfit().
}
\author{
Don MacQueen
}
\note{

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{getcon}}
}
\examples{
 data(capsrcs.ex)
     
 ## get output from two models centered at different locations
 cp1n <-  getcon( file.path(.libPaths(),'Rneshaps','AAAAAM.CON'), site='Pnt 1',  source.coords=capsrcs.ex)
 cp2n <-  getcon( file.path(.libPaths(),'Rneshaps','AAAAAK.CON'), site='Pnt 2',  source.coords=capsrcs.ex)

 cap.cons <- caplist(cp1n,cp2n)
}