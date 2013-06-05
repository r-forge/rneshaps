\name{cfitfun}
\alias{cfitfun}
\title{
Calculate the sum of squares for a set of scaling factors.
}
\description{
  Primarily designd for use internally by capfit(). Supplied to the
  optim() function, as the function that returns the sum of squares to
  be minimized.
}
\usage{
cfitfun(scls, cap.crnt = cap.cons, fit.at.xy = at.xy, sampler.avgs = avgs.with.names, refgrid = ref.grid, runcd = run.code, runds = run.description, returnall = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{scls}{
Numeric vector, length=length(cap.crnt). Concentrations in each model will be multiplied by these.
}
  \item{cap.crnt}{
A list of "capcon" objects.
}
  \item{fit.at.xy}{
    Coordinates at which to calculate the sum of squares. See
    ?predict.caplist and ?aterp for more information
}
  \item{sampler.avgs}{
    A data frame containing average concentrations at sampler locations.
    See ?predict.caplist and ?aterp for more information
}
  \item{refgrid}{
    An object created by cap.grid().
}
  \item{runcd}{
    A run code. See cappar().
}
  \item{runds}{
    A run description. See cappar()
  }
  \item{returnall}{
    FALSE to return only the sum of squares. TRUE to return a list that
    includes additional information.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  Either the sum of squares for the current fit, or that plus additional information.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
