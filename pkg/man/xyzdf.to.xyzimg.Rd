\name{xyzdf.to.xyzimg}
\alias{xyzdf.to.xyzimg}
\alias{df.to.img}
\alias{xyzimg.to.xyzdf}
\alias{img.to.df}
\title{
Convert to and from data frame and list(x,y,z)
}
\description{
  Convenience functions used to transform a data frame with
  x,y,z elements to a list with elements x,y,z, or vice versa. The list
  structure is suitable for use in image() or fields::image.plot().
}
\usage{
xyzdf.to.xyzimg(df)
df.to.img(df)
xyzimg.to.xyzdf(xyz)
img.to.df(xyz)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  For converting from a data frame to an "image" structure (xyzdf.to.img
  and df.to.img):
  \item{df}{A data frame with columns \code{x,y,z},  or
    \code{long,lat,z}. If neither of thise sets is found,
    the first three columns will be used as x,y,z.
  }

  For converting from "image" structure to a data frame (xyzimg.to.df
  and img.to.df):
  \item{xyz}{A list with at least three elements, x,y, and z,
    structured suitably for image() or fields::image.plot(). Additional
    elements, if any, will be ignored.
  }
  }
  \details{

    On input, df (a data frame) must include either x,y,z, or long,lat,z. If neither of
    these sets of three is found, the function will assume the first
    three columns are x,y, and z, in that order. All should be numeric,
    and the function does not check.

    On output, df will be a data frame with elements x,y, and z.
    
    On input, xyz must include at least the elements x,y,z. Any other
    elements will be ignored.

    On output, xyz will be a list of exactly those three elements.
}
\value{
  For converting from "image" structure to a data frame (xyzimg.to.df):
  \item{df }{A data frame with three columns, x, y, and z}
  
  For converting from a data frame to an "image" structure (xyzdf.to.img
  and df.to.img), a list with elements:
  \item{x }{A vector of unique x coordinates}
  \item{y }{A vector of unique y coordinates}
  \item{z }{A matrix of z coordinates for each unique x,y pair}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Don MacQueen
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{image}},
  \code{\link[fields]{image.plot}}
}
\examples{
tmpdf <- cbind( expand.grid( x=1:5, y=11:15), z=rnorm(25))
tmpls <- xyzdf.to.xyzimg(tmpdf)
image(tmpls)

tmpdf2 <- xyzimg.to.xyzdf(tmpls)
all(tmpdf$z == tmpdf2$z)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
