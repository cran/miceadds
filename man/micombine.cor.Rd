\name{micombine.cor}
\alias{micombine.cor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Combination of Correlations for Multiply Imputed Data Sets
}
\description{
Statistical inference for correlation coefficients for multiply
imputed datasets
}
\usage{
micombine.cor(mi.res, variables = 1:(ncol(mi.list[[1]])), conf.level = 0.95,
     method="pearson")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mi.res}{
Object of class \code{mids} or \code{mids.1chain}
}
  \item{variables}{
Indices of variables for selection
}
  \item{conf.level}{
Confidence level
}
\item{method}{Method for calculating correlations. Must be one
of \code{"pearson"} or \code{"spearman"}. The default is the
calculation of the Pearson correlation.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A data frame containing the correlation coefficient (\code{r}) and its
corresponding standard error (\code{rse}), fraction of missing information
(\code{fmi}) and a \eqn{t} value (\code{t}).
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
# EXAMPLE 1: nhanes data | combination of correlation coefficients
#############################################################################

library(mice)
data(nhanes, package="mice")
set.seed(9090)

# nhanes data in one chain
imp.mi <- mice.1chain( nhanes , burnin=5 , iter=20 , Nimp=4 ,
        imputationMethod=rep("norm" , 4 ) )
# correlation coefficients of variables 4,2 and 3 (indexed in nhanes data)
res <- micombine.cor(mi.res=imp.mi, variables = c(4,2,3) )
  ##     variable1 variable2       r    rse fisher_r fisher_rse    fmi       t      p
  ##   1       chl       bmi  0.2458 0.2236   0.2510     0.2540 0.3246  0.9879 0.3232
  ##   2       chl       hyp  0.2286 0.2152   0.2327     0.2413 0.2377  0.9643 0.3349
  ##   3       bmi       hyp -0.0084 0.2198  -0.0084     0.2351 0.1904 -0.0358 0.9714
  ##     lower95 upper95
  ##   1 -0.2421  0.6345
  ##   2 -0.2358  0.6080
  ##   3 -0.4376  0.4239
  
#############################################################################
# EXAMPLE 2: nhanes data | comparing different correlation coefficients
#############################################################################  
  
library(psych)
library(mitools)

# imputing data
imp1 <- mice( nhanes ,  imputationMethod=rep("norm" , 4 ) )
summary(imp1)

#*** Pearson correlation
res1 <- micombine.cor(mi.res=imp1, variables = c(4,2) )

#*** Spearman rank correlation
res2 <- micombine.cor(mi.res=imp1, variables = c(4,2) ,  method="spearman")

#*** Kendalls tau
# test of computation of tau for first imputed dataset
dat1 <- complete(imp1, action=1)
tau1 <- psych::corr.test(x=dat1[,c(4,2)], method = "kendall")
tau1$r[1,2]    # estimate
tau1$se     # standard error

# results of Kendalls tau for all imputed datasets
res3 <- with( data=imp1 , 
        expr = psych::corr.test( x = cbind( chl , bmi ) , method="kendall") )
# extract estimates
betas <- lapply( res3$analyses , FUN = function(ll){ ll$r[1,2] } )
# extract variances
vars <- lapply( res3$analyses , FUN = function(ll){ ll$se^2 } )
# Rubin inference
tau_comb <- mitools::MIcombine( betas , vars )
summary(tau_comb)  
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Decriptives}
\keyword{mids}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
