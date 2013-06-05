\name{get.cavgs}
\alias{get.cavgs}
\title{
Prepare ambient air monitoring data for use by the capfit() function.
}
\description{
This function is used by capfit() to get ambient air monitoring data and
prepare it for use in the fitting process by capfit().
}
\usage{
get.cavgs(params)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{params}{
    A list of fitting process input parameters, normally prepared by the
    cappar() function. See details for the required elements of params.
  }
}
\details{
The user is expected to separately prepare a data frame or
SpatialPointsDataFrame of ambient air monitoring results. It must
include a variable identifying monitoring location names, a variable
with ambient air average concentrations, and coordinates. If a data
frame, the coordinates must be in variables named 'x' and 'y'. If a
SpatialPointsDataFrame (see the sp package), the coords slot will be
used.

The params argument must contain elements
\itemize{
  \item{avgs.name }{The name of the data frame or SpatialPointsDataFrame}
  \item{loc.col.name }{The name of the variable (column) that identifes the
  monitoring locations}
  \item{avgs.col.name }{The name of variable that contains the average concentrations.}
  \item{locs.to.use }{"all" or a character vector of location names that will be used}
}

The locs.to.use element is used to identify a subset of locations that
will be used. "all" causes all locations to be used.

If the input is a SpatialPointsDataFrame it will be converted to a
simple data frame for use by capfit, and its spatial information will be
saved (in this case, capfit will convert its output element "fitpred" to
a SpatialPointsDataFrame with the same spatial information).
  
}
\value{
  A list with elements
  \itemize{
    \item{locs.to.use }{The locs.to.use supplied in the input}
    \item{avgs.with.names }{Numeric vector of named elements, each element
      is an average; the names are the location names.}
    \item{avgs.to.fit }{A data frame of averages, subsetted to locs.to.use
      if necessary, and converted from a
      SpatialPointsDataFrame if necessary.}
    \item{navgs.to.fit }{The number of averages being fit.}
    \item{prj }{The proj4string of the input if it was Spatial}
    \item{coln }{The names of the coordinate columns in the coords slot,
      if input was Spatial}
    \item{Spatial.input }{Logical indicating whether the input was Spatial
      or not.}
    }
}
\author{
Done MacQueen
}

\seealso{
\code{\link{capfit}}, \code{\link{Rnes.sp.to.df}}
}
\examples{
##Will create examples after example input data has been created
}
