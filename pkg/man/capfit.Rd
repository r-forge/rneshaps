\name{capfit}
\alias{capfit}
\title{
Fit multiple source terms to ambient air monitoring data
}
\description{
  This function takes CAP88 PC model output from several source
  locations and scales a subset of them so as to provide a best fit of
  modeled to measured concentrations.
}
\usage{
capfit(params = NULL, ngrid = 400, optimizer.to.use = 2, assign.out = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{params}{
See the cappar() function for details
}
\item{ngrid}{
  Passed to cap.grid()
}
\item{optimizer.to.use}{
  Specify which of four methods available in the optim() function to use
}
  \item{assign.out}{
    If TRUE, the function will use assign() to write some information to
    .GlobalEnv: current.fit.results, current.src.pcts, current.results, and
    possibly tmp.obj.bth.
  }
}
\details{
  This function takes a CAP88 model information in the 'cap.cons' element
  of the input params, sample averages from the data frame (or
  SpatialPointsDataFrame) named in the 'avgs.name' param, and uses the
  optim() function to re-scale one or more of the input models to find a
  best fit of model averages to sample averages.

  Using capfit requires the following:
  \enumerate{
    \item{Store sampling data in a data frame}
    \item{Using getcon() to load CAP88 model concentration output into R}
    \item{Preparing input parameters using getpar()}
    \item{Passing the parameters to capfit()}
  }
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
  U.S. Environmental Protection Agency.
  User's guide for CAP88-PC Version 1.
  EPA 402-B-92-001; 1992.
  Available at
  http://www.epa.gov/rpdweb00/docs/cap88/402-b-92-001.pdf.
  (Accessed 20 December 2012). 
}
\author{
Don MacQueen
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{cappar}},\code{\link{getcon}},\code{\link{cap.add}},
}
\examples{
## example sampling data (step 1 in details)
data(capsrcs.ex)

## source location names and coordinates
data(caplocs.ex)

## prepare CAP88-PC 'CON' output (step 2 of details)
## two point sources, three diffuse sources
##   'cp' is 'c' for CAP88 and 'p' for point source
##   'cd' is 'c' for CAP88 and 'd' for diffuse source
## diffuse sources are assumed to be unit source terms
## point sources are assumed to have been modeled using measured source
## terms
## the site argument must be a site name found in capsrcs.ex$Location
cp1n <-  getcon( file.path(.libPaths(),'Rneshaps','AAAAAM.CON'), site='Pnt 1',  source.coords=capsrcs.ex)
cp1f <-  getcon( file.path(.libPaths(),'Rneshaps','AAAAAN.CON'), site='Pnt 1',  source.coords=capsrcs.ex)
cp2n <-  getcon( file.path(.libPaths(),'Rneshaps','AAAAAM.CON'), site='Pnt 2',  source.coords=capsrcs.ex)
cp2f <-  getcon( file.path(.libPaths(),'Rneshaps','AAAAAN.CON'), site='Pnt 2',  source.coords=capsrcs.ex)

## generic diffuse
cd1n <-  getcon( file.path(.libPaths(),'Rneshaps','AAAAAK.CON'), site='Diff 1',  source.coords=capsrcs.ex)
cd1f <-  getcon( file.path(.libPaths(),'Rneshaps','AAAAAL.CON'), site='Diff 1',  source.coords=capsrcs.ex)
cd2n <-  getcon( file.path(.libPaths(),'Rneshaps','AAAAAK.CON'), site='Diff 2',  source.coords=capsrcs.ex)
cd2f <-  getcon( file.path(.libPaths(),'Rneshaps','AAAAAL.CON'), site='Diff 2',  source.coords=capsrcs.ex)
cd3n <-  getcon( file.path(.libPaths(),'Rneshaps','AAAAAK.CON'), site='Diff 3',  source.coords=capsrcs.ex)
cd3f <-  getcon( file.path(.libPaths(),'Rneshaps','AAAAAL.CON'), site='Diff 3',  source.coords=capsrcs.ex)

## 'near' refers to distances of 100m to 2000m from the source, as
##  specified in the CAP88 run
## 'far' refers to 2100m to 4000m

## join the near and far concentrations
cp1 <- cap.join(cp1n, cp1f)
cp2 <- cap.join(cp2n, cp2f)

cd1 <- cap.join(cd1n, cd1f)
cd2 <- cap.join(cd2n, cd2f)
cd3 <- cap.join(cd3n, cd3f)

## normally, the second point source would have different CAP88 CON files
cp2$cap <- cp2$cap * adj.conc ; cp2$capdf$conc <- cp2$capdf$conc * adj.conc

## put the model concentration info into a 'caplist' object for use by capfit()
capcons <- caplist(cd1, cd2, cd3, cp1, cp2)

## inspect the combined models (unit source terms for diffuse sources)
refg <- cap.grid(capcons, ngrid=600)
cpi <- cap.add(cd1, cd2, cd3, cp1, cp2, grid=refg)
plot(cpi)
points(caplocs.ex$x, caplocs.ex$y, pch=15)

## CAP88-PC 'CON' output is now ready for use

## prepare fitting parameters (step 3 of details)
pfk <-  cappar(
          run.code='pfk',
          run.description='Test run using clean example data',
          cap.cons=capcons,
          nsrcfit=3,
          psearch=TRUE,
          strt=1,
          avgs.name='caplocs.ex',
          avgs.col.name='tavg',
          loc.col.name='locs'
          )

## find best fit (step 4 of details)
ffk <- capfit(pfk)

## of particular interest are the RMS and the best fit source terms
print(ffk)

plot(ffk)

}
