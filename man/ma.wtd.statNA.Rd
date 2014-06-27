\name{ma.wtd.statNA}
\alias{ma.wtd.statNA}

\alias{ma.wtd.covNA}
\alias{ma.wtd.corNA}
\alias{ma.wtd.meanNA}
\alias{ma.wtd.sdNA}
%- Also NEED an '\alias' for EACH other topic documented here.


\title{
Some Multivariate Descriptive Statistics for Weighted Data in \pkg{miceadds}
}
\description{
Some multivariate descriptive statistics for weighted data in \pkg{miceadds}.
}

\usage{
ma.wtd.meanNA(data, weights = rep(1, nrow(data)) )
ma.wtd.sdNA(data, weights = rep(1, nrow(data)) )
ma.wtd.covNA(data, weights = rep(1, nrow(data)) )
ma.wtd.corNA(data, weights = rep(1, nrow(data)) )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Numeric data frame
}
  \item{weights}{
Optional vector of sampling weights
}
}
\details{
Contrary to ordinary \R practice, missing values are ignored in the
calculation of descriptive statistics.

\tabular{ll}{
\code{ma.wtd.meanNA} \tab weighted means \cr
\code{ma.wtd.sdNA} \tab weighted standard deviations \cr
\code{ma.wtd.covNA} \tab weighted covariance matrix \cr
\code{ma.wtd.corNA} \tab weighted correlation matrix
}

}
\value{
A matrix or a vector depending on the requested statistic.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Alexander Robitzsch
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%See \code{\link{ma.wtd.stat}} for descriptive statistics of multiply
%imputed datasets.
%}
%% include links to other packages

\examples{
#############################################################################
# EXAMPLE 1: Weighted statistics data.ma01
#############################################################################

data(data.ma01)
dat <- as.matrix(data.ma01[,-c(1:3)])

# weighted mean
ma.wtd.meanNA( dat , weights=data.ma01$studwgt  ) 

# weighted SD
ma.wtd.sdNA( dat , weights=data.ma01$studwgt  ) 

# weighted covariance
ma.wtd.covNA( dat , weights=data.ma01$studwgt )

# weighted correlation
ma.wtd.corNA( dat , weights=data.ma01$studwgt )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{Utility function}
\keyword{Descriptives}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
