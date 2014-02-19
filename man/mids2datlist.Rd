\name{mids2datlist}
\alias{mids2datlist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converting a \code{mids} or \code{mids.1chain} Object in a Dataset List
}
\description{
Converts a \code{mids} or \code{mids.1chain} object in a dataset list.
}
\usage{
mids2datlist(midsobj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{midsobj}{
Object of class \code{mids} or \code{mids.1chain}
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
List of multiply imputed datasets
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
\dontrun{
#############################################################################
# EXAMPLE 1: Imputing nhanes data and convert result into a dataset list
#############################################################################

data(nhanes,package="mice")

#**** imputation using mice
imp1 <- mice( nhanes , m=3 , maxit=5 )
# convert mids object into list
datlist1 <- mids2datlist( imp1 )

#**** imputation using mice.1chain
imp2 <- mice.1chain( nhanes , burnin=4 , iter=20 , Nimp=5 )
# convert mids.1chain object into list
datlist2 <- mids2datlist( imp2 )
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{mids}

