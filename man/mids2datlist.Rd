\name{mids2datlist}
\alias{mids2datlist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converting a \code{mids}, \code{mids.1chain} or \code{mids.nmi} 
Object in a Dataset List
}
\description{
Converts a \code{mids} \code{mids.1chain} or \code{mids.nmi} 
object in a dataset list.
}
\usage{
mids2datlist(midsobj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{midsobj}{
Object of class \code{mids}, \code{mids.1chain} or \code{mids.nmi} 
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

#############################################################################
# EXAMPLE 2: Nested multiple imputation and datalist conversion
#############################################################################

library(BIFIEsurvey)
data(data.timss2 , package="BIFIEsurvey" )
datlist <- data.timss2		
   # list of 5 datasets containing 5 plausible values

# remove first four variables
M <- length(datlist)
for (ll in 1:M){
    datlist[[ll]] <- datlist[[ll]][ , -c(1:4) ]
                }
                
#***************
# (1) nested multiple imputation using mice
imp1 <- mice.nmi( datlist ,  m=4 , maxit=3 )
summary(imp1)

#***************
# (2) nested multiple imputation using mice.1chain
imp2 <- mice.nmi( datlist , Nimp=4 , burnin=10 ,iter=22, type="mice.1chain")
summary(imp2)

#**************
# conversion into a datalist
datlist.i1 <- mids2datlist( imp1 )
datlist.i2 <- mids2datlist( imp2 )
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{mids}

