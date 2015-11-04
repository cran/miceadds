\name{mice.impute.2l.pls2}
\alias{mice.impute.2l.pls}
\alias{mice.impute.2l.pls2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Imputation using Partial Least Squares for Dimension Reduction
}
\description{
This function imputes a variable with missing values using 
PLS regression (Mevik & Wehrens, 2007) for a dimension
reduction of the predictor space.
}
\usage{
mice.impute.2l.pls2(y, ry, x, type, pls.facs = NULL, 
   pls.impMethod = "pmm", pls.print.progress = TRUE, 
   imputationWeights = rep(1, length(y)), pcamaxcols = 1E+09, 
   tricube.pmm.scale = NULL, min.int.cor = 0, min.all.cor=0, 
   N.largest = 0, pls.title = NULL, print.dims = TRUE,
   pls.maxcols=5000 , ...)

mice.impute.2l.pls(y, ry, x, type, pls.facs = NULL, 
   pls.impMethod = "tricube.pmm2", pls.method = NULL, 
   pls.print.progress = TRUE, imputationWeights = rep(1, length(y)), 
   pcamaxcols = 1E+09, tricube.pmm.scale = NULL, min.int.cor = 0, min.all.cor=0, 
   N.largest = 0, pls.title = NULL, print.dims = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{y}{Incomplete data vector of length \code{n}}
    
  \item{ry}{
Vector of missing data pattern (\code{FALSE} -- missing,
\code{TRUE} -- observed)
}

  \item{x}{Matrix (\code{n} x \code{p}) of complete
  covariates.}
  
  \item{type}{
\code{type=1} -- variable is used as a predictor,

\code{type=4} -- create interactions with the specified
variable with all other predictors,

\code{type=5} -- create a quadratic term of the specified variable 

\code{type=6} -- if some interactions are specified, ignore
the variables with entry \code{6} when creating interactions 

\code{type=-2} -- specification of a cluster variable. The cluster mean
of the outcome \code{y} (when eliminating the subject under study)
is included as a further predictor in the imputation.

}
  \item{pls.facs}{
Number of factors used in PLS regression. This argument can also
be specified as a list defining different numbers of factors
for all variables to be imputed.
}
  \item{pls.impMethod}{
Imputation method based in the PLS regression model: \cr

\code{norm} -- normal linear regression

\code{pmm} -- predictive mean matching (\code{pmm} method from \pkg{mice}) 

\code{pmm5} -- predictive mean matching (\code{pmm5} method from \pkg{miceadds}) 

\code{tricube.pmm}/\code{tricube.pmm2} -- predictive mean 
matching with tricube kernel 

\code{xplsfacs} -- create only PLS factors of the regression
model 
}
\item{pls.method}{
Calculation method of PLS regression. See \code{\link[pls:plsr]{pls::plsr}}
(\pkg{pls}) for more details.
}
  \item{pls.print.progress}{
Print progress during PLS regression.
}
  \item{imputationWeights}{
Vector of sample weights to be used in imputation models.
}
  \item{pcamaxcols}{
Maximum number of principal components.
}
  \item{tricube.pmm.scale}{
Scale factor for tricube predictive mean matching.
}
  \item{min.int.cor}{
Minimum absolute correlation for an interaction of two predictors
to be included in the PLS regression model
}
  \item{min.all.cor}{
Minimum absolute correlation for inclusion in the PLS regression
model.
}
  \item{N.largest}{
Number of variable to be included which do have the largest
absolute correlations.
}
  \item{pls.title}{
Title for progress print in console output.
}
  \item{print.dims}{
An optional logical indicating whether dimensions of 
inputs should be printed.
}
\item{pls.maxcols}{
Maximum number of interactions to be created.
}
  \item{\dots}{
Further arguments to be passed.
}
}
\details{
The function \code{mice.impute.2l.pls2} uses \code{\link{kernelpls.fit2}}
instead of \code{kernelpls.fit} from the \pkg{pls} package and is
a bit faster.
}

\value{
A vector of length \code{nmis=sum(!ry)} with imputations
if \code{pls.impMethod != "xplsfacs"}. In case of
\code{pls.impMethod == "xplsfacs"} a matrix with PLS factors
is computed.
}

\references{
Mevik, B. H., & Wehrens, R. (2007). The \pkg{pls} package: 
Principal component and partial least squares regression in \R. 
\emph{Journal of Statistical Software}, \bold{18}, 1-24.
}

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
# EXAMPLE 1: PLS imputation method for internet data
#############################################################################	

data(data.internet)
dat <- data.internet

# specify predictor matrix
predictorMatrix <- matrix( 1 , ncol(dat) , ncol(dat) )
rownames(predictorMatrix) <- colnames(predictorMatrix) <- colnames(dat)
diag( predictorMatrix) <- 0

# use PLS imputation method for all variables
impMethod <- rep( "2l.pls2" , ncol(dat) )
names(impMethod) <- colnames(dat)

# define predictors for interactions (entries with type 4 in predictorMatrix)
predictorMatrix[c("IN1","IN15","IN16"),c("IN1","IN3","IN10","IN13")] <- 4
# define predictors which should appear as linear and quadratic terms (type 5)
predictorMatrix[c("IN1","IN8","IN9","IN10","IN11"),c("IN1","IN2","IN7","IN5")] <- 5

# use 9 PLS factors for all variables
pls.facs <- as.list( rep( 9 , length(impMethod) ) )
names(pls.facs) <- names(impMethod)
pls.facs$IN1 <- 15   # use 15 PLS factors for variable IN1

# choose norm or pmm imputation method
pls.impMethod <- as.list( rep("norm" , length(impMethod) ) )
names(pls.impMethod) <- names(impMethod)
pls.impMethod[ c("IN1","IN6")] <- "pmm5"   

# Model 1: Three parallel chains
imp1 <- mice(data = dat , imputationMethod = impMethod ,  
     m=3 , maxit=5 , predictorMatrix = predictorMatrix ,
     pls.facs = pls.facs , # number of PLS factors
     pls.impMethod = pls.impMethod ,  # Imputation Method in PLS imputation
     pls.print.progress = TRUE )
summary(imp1)

# Model 2: One long chain
imp2 <- mice.1chain(data = dat , imputationMethod = impMethod ,  
     burnin=10 , iter=21 , Nimp=3 , predictorMatrix = predictorMatrix ,
     pls.facs = pls.facs , pls.impMethod = pls.impMethod )
summary(imp2)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Partial least squares regression (PLS)}
\keyword{Dimension reduction}
\keyword{mice imputation method}
