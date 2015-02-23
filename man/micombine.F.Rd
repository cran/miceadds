\name{micombine.F}
\alias{micombine.F}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Combination of F Statistics for Multiply Imputed Datasets Using a
Chi Square Approximation
}

\description{
Several \eqn{F} statistics from multiply imputed datasets are combined using
an approximation based on \eqn{\chi^2} statistics 
(see \code{\link{micombine.chisquare}}).
}

\usage{
micombine.F(Fvalues, df1, display = TRUE)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
\item{Fvalues}{
Vector containing \eqn{F} values.
}
  \item{df1}{
Degrees of freedom of the denominator. Degrees of freedom of the
numerator are approximated by \eqn{\infty} (large number of degrees
of freedom).
}
  \item{display}{
A logical indicating whether results should be displayed at the console 
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
The same output as in \code{\link{micombine.chisquare}}
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

\seealso{
\code{\link{micombine.chisquare}}
}

\examples{
#############################################################################
# EXAMPLE 1: F statistics for 5 imputed datasets
#############################################################################

Fvalues <- c( 6.76 , 4.54 , 4.23 , 5.45 , 4.78 )
micombine.F(Fvalues, df1=4 )
  ##   Combination of Chi Square Statistics for Multiply Imputed Data
  ##   Using 5 Imputed Data Sets
  ##   F(4,67.11)=4.097     p=0.00497 
  ##   Chi Square Approximation Chi2(4)=16.387     p=0.00254 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{F statistic}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
