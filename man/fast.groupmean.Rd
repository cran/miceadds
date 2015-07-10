\name{fast.groupmean}
\alias{fast.groupmean}
\alias{fast.groupsum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculation of Groupwise Descriptive Statistics for Matrices
}
\description{
Calculates some groupwise descriptive statistics
}
\usage{
fast.groupmean(data, group, weights=NULL, extend=FALSE)

fast.groupsum(data, group, weights=NULL, extend=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A numeric data frame
}
  \item{group}{
A vector of group identifiers
}
\item{weights}{An optional vector of sample weights}
\item{extend}{Optional logical indicating whether the group means (or sums)
should be extended to the original dimensions of the dataset.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A data frame with groupwise calculated statistics
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
#############################################################################
# EXAMPLE 1: Group means data.ma02
#############################################################################

data( data.ma02 )
dat <- data.ma02[[1]] # select first dataset

# group means for read and math
fast.groupmean( dat[ , c("read","math") ] , group=dat$idschool )


# extend group means to original dataset
fast.groupmean( dat[ , c("read","math") ] , group=dat$idschool , extend =TRUE )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Utility function}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
