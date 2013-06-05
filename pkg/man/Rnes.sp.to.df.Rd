\name{Rnes.sp.to.df}
\alias{Rnes.sp.to.df}
\title{
Convert a SpatialPointsDataFrame to a data.frame.
}
\description{
Converts a SpatialPointsDataFrame to a data.frame and retains its
projection information.

}
\usage{
Rnes.sp.to.df(spdf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spdf}{
A SpatialPointsDataFrame (see packge sp).
}
}
\details{
  This function is designed for use inside capfit. If the user supplies
  location and source information as a SpatialPointsDataFrame,this
  function converts it to a data frame for internal work in the capfit()
  function. The data frame
  of fitted concentrations is then converted to a SpatialPointsDataFrame
  with the same projection before being returned by capfit().
}
\value{
  If the input is a SpatialPointsDataframe, returns a list:
  \item{df }{The input SpatialPointsDataFrame converted to a data
    frame.}
  \item{prj }{The proj4string of the input}
  \item{coln }{The column names of the coordinates in the input}
  \item{Spatial.input }{Logical, TRUE if the input was a
    SpatialPointsDataFrame}
  Otherwise returns the input unchanged.
}
\author{
Don MacQueen
}
\note{
  A convenience function whose purpose is to allow the user to supply location
  and source information as either a data frame or a
  SpatialPointsDataFrame.
}
\seealso{
  \code{\link{capfit}}
}
