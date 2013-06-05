\name{cappar}
\alias{cappar}
\title{
Prepare input parameters for capfit()
}
\description{
This function helps the user prepare fitting paramters for the capfit() function.
}
\usage{
cappar(run.code = "no.run.code", run.description = "no run description", params = NULL, cap.cons = NA, nsrcfit = NA, strt = 1, lb = 0.01, ub = 15, psearch = TRUE, avgs.name = "", avgs.col.name = "avg", loc.col.name = "loc", locs.to.use = "all", is.final = FALSE, pct.diff.criterion = 20, datestamp.output = "n")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{run.code}{
    An arbitrary character string (length 1) representing this set of
    input parameters. May be used to construct names of output objects
    and/or files.
  }
  \item{run.description}{
    An arbitrary character string that describes key features of the
    run. Length should be 1, but the description it self is intended to
    be as long as desired.
  }
  \item{params}{
    An optional list() of named parameters.
  }
  \item{cap.cons}{
    A "capcons" object created by caplist()
  }
  \item{nsrcfit}{
    The number of sources to fit. Must be <= length(capcons).
  }
  \item{strt}{
    A vector of initial guesses for scaling terms for first nsrcfit models
    in caplist. Length should = 1 or = nsrcfit.
  }
  \item{lb}{
    Lower bounds for estimated scaling terms. Vector of length = 1 or = nsrcfit.
  }
  \item{ub}{
    Upper bounds for estimated scaling terms. Vector of length = 1 or = nsrcfit.
  }
  \item{psearch}{
    TRUE to estimate scaling terms, FALSE to apply the supplied scaling
    terms (strt) to the input caplist and return.
  }
  \item{avgs.name}{
    The name of the data frame or SpatialPointsDataFrame that contains
    the sampler averages.
  }
  \item{avgs.col.name}{
    The name of the column (variable) in avgs.name that contains the
    sampler averages.
  }
  \item{loc.col.name}{
    The name of the column in avgs.name that contains the sampler
    location name.
  }
  % \item{unc.col.name}{
  %   Deprecated
  % }
  \item{locs.to.use}{
    A vector of location names to fit.
  }
  % \item{raw.data.name}{
  %   Deprecated
  % }
  \item{is.final}{
    Optional; flag a fit as "final".
  }
  \item{pct.diff.criterion}{
    Numeric, represents a \%difference value which is considered "good".
  }
  \item{datestamp.output}{
    Put at date-time string in the name of selected output objects.
  }
}
\details{
  Takes the user-supplied parameters, checks whether all of the required
  parameters are present, and supplies a default value as needed. 
}
\value{
  A list of fitting parameters suitable for input to capfit()
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
## See the example for capfit().
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
