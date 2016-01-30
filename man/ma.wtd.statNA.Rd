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
Some multivariate descriptive statistics for weighted datasets in \pkg{miceadds}.
A list of multiply imputed data is also allowed as input.
}

\usage{
ma.wtd.meanNA(data, weights = NULL , vars = NULL )
ma.wtd.sdNA(data, weights = NULL , vars = NULL , method = "unbiased" )
ma.wtd.covNA(data, weights = NULL , vars = NULL , method = "unbiased" )
ma.wtd.corNA(data, weights = NULL , vars = NULL , method = "unbiased" )
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Numeric data frame or objects of one of the classes \code{datlist}, 
\code{imputationList}, \code{mids}, \code{mids.1chain},
\code{nested.datlist} or 
\code{NestedImputationList}.
}
  \item{weights}{
Optional vector of sampling weights
}
\item{vars}{Optional vector of variable names}
\item{method}{Computation method for covariances. These amount to
choosing the divisor \eqn{(n-1)} (\code{method="unbiased"})
instead of \eqn{n} (\code{method="ML"}). See
\code{\link[stats:cov.wt]{stats::cov.wt}} for further details.
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
A vector or a matrix depending on the requested statistic.
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
\code{\link[stats:weighted.mean]{stats::weighted.mean}}
\code{\link[stats:cov.wt]{stats::cov.wt}},
\code{\link[Hmisc:wtd.var]{Hmisc::wtd.var}}
	
See \code{\link{micombine.cor}} for statistical inference of correlation 
coefficients.
}
%% include links to other packages

\examples{
#############################################################################
# EXAMPLE 1: Weighted statistics for a single dataset data.ma01
#############################################################################

data(data.ma01)
dat <- as.matrix(data.ma01[,-c(1:3)])

# weighted mean
ma.wtd.meanNA( dat , weights=data.ma01$studwgt  ) 

# weighted SD
ma.wtd.sdNA( dat , weights=data.ma01$studwgt  ) 

# weighted covariance for selected variables
ma.wtd.covNA( dat , weights=data.ma01$studwgt , vars = c("books","hisei") )

# weighted correlation
ma.wtd.corNA( dat , weights=data.ma01$studwgt )

\dontrun{
#############################################################################
# EXAMPLE 2: Weighted statistics multiply imputed dataset
#############################################################################

library(mitools)
data(data.ma05)
dat <- data.ma05

# do imputations
resp <- dat[ , - c(1:2) ]
# object of class mids
imp <- mice::mice( resp , imputationMethod="norm" , maxit=3 , m=5 )
# object of class datlist
datlist <- mids2datlist( imp )
# object of class imputationList
implist <- mitools::imputationList(datlist)

# weighted means
ma.wtd.meanNA(datlist)
ma.wtd.meanNA(implist)
ma.wtd.meanNA(imp)	

#############################################################################
# EXAMPLE 3: Weighted statistics nested multiply imputed dataset
#############################################################################

library(BIFIEsurvey)
data(data.timss2 , package="BIFIEsurvey" )
datlist <- data.timss2   # list of 5 datasets containing 5 plausible values

#** define imputation method and predictor matrix
data <- datlist[[1]]
V <- ncol(data)
# variables
vars <- colnames(data)
# variables not used for imputation
vars_unused <- scan.vec("IDSTUD TOTWGT  JKZONE  JKREP" )
#- define imputation method
impMethod <- rep("norm" , V )
names(impMethod) <- vars
impMethod[ vars_unused ] <- ""
#- define predictor matrix
predM <- matrix( 1 , V , V )
colnames(predM) <- rownames(predM) <- vars
diag(predM) <- 0
predM[ , vars_unused ] <- 0

# object of class mids.nmi
imp1 <- mice.nmi( datlist , imputationMethod=impMethod , predictorMatrix=predM, 
                m=4 , maxit=3 )
# object of class nested.datlist
datlist <- mids2datlist(imp1)
# object of class NestedImputationList
imp2 <- NestedImputationList(datlist)

# weighted correlations
vars <- c("books","ASMMAT","likesc")
ma.wtd.corNA( datlist ,  vars = vars )
ma.wtd.corNA( imp2 ,  vars = vars )
ma.wtd.corNA( imp1 ,  vars = vars )
}	
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{Utility function}
\keyword{Descriptives}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
