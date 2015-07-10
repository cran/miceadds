\name{pool.mids.nmi}
\alias{pool.mids.nmi}
\alias{summary.mipo.nmi}
\alias{coef.mipo.nmi}
\alias{vcov.mipo.nmi}
\alias{NMIcombine}
\alias{NMIextract}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pooling for Nested Multiple Imputation
}

\description{
Statistical inference for scalar parameters for nested multiply imputed 
datasets (Rubin, 2003; Harel & Schafer, 2003; Reiter & Raghanuthan, 2007).

The \code{NMIcombine} and \code{NMIextract} functions are extensions of
\code{\link[mitools:MIcombine]{mitools::MIcombine}} and
\code{\link[mitools:MIextract]{mitools::MIextract}}.
}


\usage{
pool.mids.nmi(object, method = "largesample")

NMIcombine( qhat , u=NULL , NMI=TRUE, comp_cov=TRUE, is_list=TRUE )

NMIextract(results, expr, fun)

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
\item{qhat}{List of lists of parameter estimates. In case of an ordinary
imputation it can only be a list.}
\item{u}{Optional list of lists of covariance matrices of parameter estimates}
\item{NMI}{Optional logical indicating whether the \code{NMIcombine}
	function should be applied for results of 
	nested multiply imputed datasets.
	It is set to \code{FALSE} if only a list results of multiply imputed 
datasets is available.} 
\item{comp_cov}{Optional logical indicating whether covariances between
parameter estimates should be estimated.}
\item{is_list}{Optional logical indicating whether \code{qhat} and \code{u}
	are provided as lists as an input. If \code{is_list=FALSE},
appropriate arrays can be used as inpur.}
\item{results}{A list of objects}
\item{expr}{An expression}
\item{fun}{A function of one argument}
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

\code{\link[mice:pool]{mice::pool}}, 
\code{\link[mitools:MIcombine]{mitools::MIcombine}},
\code{\link[mitools:MIextract]{mitools::MIextract}}

\code{\link{MIcombine.NestedImputationResultList}}
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

#***************
# (4) some descriptive statistics using the mids.nmi object
res3 <- with( imp1 , c( "M_lsc"=mean(likesc) , "SD_lsc"=sd(likesc) ) )
pres3 <- NMIcombine( qhat = res3$analyses )
summary(pres3)

#*************
# (5) apply linear regression based on imputation list

# convert mids object to datlist
datlist2 <- mids2datlist( imp1 )
str(datlist2, max.level=1)

# double application of lapply to the list of list of nested imputed datasets
res4 <- lapply( datlist2 , FUN = function(dl){
    lapply( dl , FUN = function(data){ 
            lm( ASMMAT ~ migrant + books  , data = data )
                                } )
                }  )
                
# extract coefficients
qhat <- lapply( res4 , FUN = function(bb){
            lapply( bb , FUN = function(ww){
                    coef(ww)
                        } )
                } )
# shorter function
NMIextract( results=res4 , fun=coef )                

# extract covariance matrices
u <- lapply( res4 , FUN = function(bb){
            lapply( bb , FUN = function(ww){
                    vcov(ww)
                        } )
                } )        
# shorter function
NMIextract( results=res4 , fun=vcov )                               

# apply statistical inference using the NMIcombine function
pres4 <- NMIcombine( qhat=qhat , u=u )                
summary(pres4)       
}	
}



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Nested multiple imputation}
\keyword{summary}
\keyword{coef}
\keyword{vcov}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
