\name{pool.mids.nmi}
\alias{pool.mids.nmi}
\alias{summary.mipo.nmi}
\alias{coef.mipo.nmi}
\alias{vcov.mipo.nmi}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pooling for Nested Multiple Imputation
}

\description{
Statistical inference for scalar parameters for nested multiply imputed 
datasets (Rubin, 2003; Harel & Schafer, 2003; Reiter & Raghanuthan, 2007).
}


\usage{
pool.mids.nmi(object, method = "largesample")

\method{summary}{mipo.nmi}(object, digits=3, ...)

\method{coef}{mipo.nmi}(object, ...)

\method{vcov}{mipo.nmi}(object, ...)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object of class \code{mids.nmi}. For \code{summary} it must be an object
of class \code{mipo.nmi}.
}
  \item{method}{
Method for calculating degrees of freedom. Until now, only the
method \code{"largesample"} is available.
}
\item{digits}{Number of digits after decimal for printing results in
\code{summary}.}
\item{\dots}{Further arguments to be passed.}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
Object of class \code{mipo.nmi} with following entries

\item{qhat}{Estimated parameters in all imputed datasets}
\item{u}{Estimated covariance matrices of parameters in all imputed datasets}
\item{qbar}{Estimated parameter}
\item{ubar}{Average estimated variance within imputations}
\item{Tm}{Total variance of parameters}
\item{df}{Degrees of freedom}
\item{lambda}{Total fraction of missing information}
\item{lambda_Between}{Fraction of missing information of between imputed
 datasets (first stage imputation)}
\item{lambda_Within}{Fraction of missing information of within imputed
 datasets (second stage imputation)}
}




\references{
Harel, O., & Schafer, J. (2003). \emph{Multiple imputation in two stages}. 
In Proceedings of Federal Committee on Statistical Methodology 2003 Conference.

Reiter, J. P., & Raghunathan, T. E. (2007). The multiple adaptations of 
multiple imputation. \emph{Journal of the American Statistical Association}, 
\bold{102(480)}, 1462-1471.

Rubin, D. B. (2003). Nested multiple imputation of NMES via partially 
incompatible MCMC. \emph{Statistica Neerlandica}, \bold{57(1)}, 3-18.
}

\author{
Alexander Robitzsch
}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{mice.nmi}}

\code{\link[mice:pool]{pool}} (\pkg{mice}) 
}


\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Nested multiple imputation and statistical inference
#############################################################################

library(BIFIEsurvey)
data(data.timss2 , package="BIFIEsurvey" )
datlist <- data.timss2         
# remove first four variables
M <- length(datlist)
for (ll in 1:M){
    datlist[[ll]] <- datlist[[ll]][ , -c(1:4) ]
               }
                
#***************
# (1) nested multiple imputation using mice
imp1 <- mice.nmi( datlist ,  m=3 , maxit=2 )
summary(imp1)

#***************
# (2) first linear regression: ASMMAT ~ migrant + female
res1 <- with( imp1 , lm( ASMMAT ~ migrant + female ) ) # fit
pres1 <- pool.mids.nmi( res1 )  # pooling
summary(pres1)  # summary
coef(pres1) 
vcov(pres1)

#***************
# (3) second linear regression: likesc ~ migrant + books
res2 <- with( imp1 , lm( likesc ~ migrant + books  ) )
pres2 <- pool.mids.nmi( res2 ) 
summary(pres2)
}	
}



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Nested multiple imputation}
\keyword{summary}
\keyword{coef}
\keyword{vcov}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
