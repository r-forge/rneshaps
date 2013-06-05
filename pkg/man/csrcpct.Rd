\name{csrcpct}
\alias{csrcpct}
\title{
Calculate the relative or absolute contribution of several sources to several locations.
}
\description{
This function takes the output of capfit() and calculates either the relative
contribution, or the absolute contribution, of each source to each location.
}
\usage{
csrcpct(cfit, type = "pct")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cfit}{
    Output from capfit()
}
  \item{type}{
    type='pct' for relative contribution. Any other value will result in
    absolute contribution
}
}
\details{
The total prediction concentration at any location is the sum of
concentrations from each of several sources. This function reports those
individual source contributions, either in concentration units, or as a
percent of total predicted concentration.

Results are presented in a matrix. Percentages should sum to 100\% at
each location.
}
\value{
%%  ~Describe the value returned
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
  \code{\link{capfit}}
}
\examples{
## First run the example in ?capfit
## then after
##    ffk <- capfit(pfk)
## finishes:

csrcpct(ffk)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
