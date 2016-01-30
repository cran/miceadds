\name{datlist_create}
\alias{datlist_create}
\alias{nested.datlist_create}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Creates Objects of Class \code{datlist} or \code{nested.datlist}
}
\description{
Creates objects of class \code{datlist} or \code{nested.datlist}.
}
\usage{
datlist_create(datasets)

nested.datlist_create(datasets)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{datasets}{
For \code{datlist_create}: List of datasets, objects of class
\code{imputationList}, \code{mids}, \code{mids.1chain},

For \code{nested.datlist_create}: nested list of datasets,
\code{NestedImputationList}, \code{mids.nmi}
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
Object of class \code{datlist} or \code{nested.datlist}
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
## The function datlist_create is currently defined as
function (datasets) 
{
    class(datasets) <- "datlist"
    return(datasets)
  }
  
#############################################################################
# EXAMPLE 1: Create object of class datlist
#############################################################################

library(BIFIEsurvey)
data(data.timss2 , package="BIFIEsurvey" )
datlist <- data.timss2      

# class datlist
obj1 <- datlist_create(data.timss2)

#############################################################################
# EXAMPLE 2: Multiply imputed datasets: Different object classes
#############################################################################

library(mice)
data(nhanes2, package="mice")
set.seed(990)

# nhanes2 data imputation
imp1 <- mice.1chain( nhanes2 , burnin=5 , iter=25 , Nimp=5 )
# object of class datlist
imp2 <- mids2datlist(imp1)
# alternatively, one can use datlist_create
imp2b <- datlist_create(imp1)
# object of class imputationList
imp3 <- mitools::imputationList(imp2)
# retransform object in class datlist
imp2c <- datlist_create(imp3)
str(imp2c)

#############################################################################
# EXAMPLE 3: Nested multiply imputed datasets
#############################################################################

library(BIFIEsurvey)
data(data.timss2 , package="BIFIEsurvey" )
datlist <- data.timss2      
   # list of 5 datasets containing 5 plausible values

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

# object of class nmi
imp1 <- mice.nmi( datlist , imputationMethod=impMethod , predictorMatrix=predM, 
                m=4 , maxit=3 )
# object of class nested.datlist
imp2 <- mids2datlist(imp1)
# object of class NestedImputationList
imp3 <- NestedImputationList(imp2)
# redefine class nested.datlist
imp4 <- nested.datlist_create(imp3)
}  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
