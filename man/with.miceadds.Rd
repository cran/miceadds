\name{with.miceadds}
\alias{with.mids.1chain}
\alias{with.mids.nmi}
\alias{summary.mira.nmi}
\alias{within.imputationList}
\alias{with.NestedImputationList}
\alias{within.NestedImputationList}

%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Evaluates an Expression for (Nested) Multiply Imputed Datasets
}

\description{
Evaluates an expression for (nested) multiply imputed datasets. These
functions extend the following functions:
\code{\link[mice:with.mids]{with.mids}} (\pkg{mice}),
\code{\link[base:within.data.frame]{within.data.frame}} (\pkg{base}),
\code{\link[mitools:with.imputationList]{with.imputationList}} (\pkg{mitools})
}

\usage{
\method{with}{mids.1chain}(data, expr, ...)

\method{with}{mids.nmi}(data, expr, ...)

\method{within}{imputationList}(data, expr, ...)

\method{with}{NestedImputationList}(data, expr, fun , ...)

\method{within}{NestedImputationList}(data, expr, ...)

\method{summary}{mira.nmi}(object , ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Object of class \code{mids.1chain}, \code{mids.nmi}, \code{imputationList}
or \code{NestedImputationList}
}
  \item{expr}{
Expression with a formula object.
}
\item{fun}{A function taking a data frame argument}
  \item{\dots}{
Additional parameters to be passed to \code{expr}.
}
\item{object}{Object of class \code{mira.nmi}.}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
\code{with.mids.1chain}: List of class \code{mira}.

\code{with.mids.nmi}: List of class \code{mira.nmi}.

\code{within.imputationList}: List of class \code{imputationList}.

\code{with.NestedImputationList}: List of class 
\code{NestedImputationResultList}.

\code{within.NestedImputationList}: List of class 
\code{NestedImputationList}.
}

%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Slightly modified code of \code{\link[mice:with.mids]{with.mids}} (\pkg{mice}), 
\code{\link[mice:summary.mira]{summary.mira}} (\pkg{mice}),
\code{\link[base:within.data.frame]{within.data.frame}} (\pkg{base})
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[mice:with.mids]{with.mids}} (\pkg{mice}),
\code{\link[mice:summary.mira]{summary.mira}} (\pkg{mice}),
\code{\link[base:within.data.frame]{within.data.frame}} (\pkg{base}),
\code{\link[mitools:with.imputationList]{with.imputationList}} (\pkg{mitools})

Imputation functions in \pkg{miceadds}: 
\code{\link{mice.1chain}}, \code{\link{mice.nmi}}
}


\examples{	
#############################################################################
# EXAMPLE 1: One chain nhanes data | application of 'with' and 'within'
#############################################################################

library(mice)
data(nhanes, package="mice")
set.seed(9090)

# nhanes data in one chain
imp <- mice.1chain( nhanes , burnin=5 , iter=40 , Nimp=4 )
# apply linear regression
res <- with( imp , expr = lm( hyp ~ age + bmi  ) )
summary(res)
# pool results
summary(pool(res))

# calculate some descriptive statistics
res2 <- with( imp , expr = c( mean(hyp) , sd(age) ) )
res2

# convert mids object into an object of class imputationList
datlist <- mids2datlist( imp )
datlist <- mitools::imputationList(datlist)

# define formulas for modification of the data frames in imputationList object
datlist2 <- within.imputationList( datlist , {
                     age.D3 <- 1*(age == 3)
                     hyp_chl <- hyp * chl
                        } )
# look at modified dataset
head( datlist2$imputations[[1]] )

\dontrun{
#############################################################################
# EXAMPLE 2: Nested multiple imputation and application of with/within methods
#############################################################################

library(BIFIEsurvey)
data(data.timss2 , package="BIFIEsurvey" )
datlist <- data.timss2        

# remove first four variables
M <- length(datlist)
for (ll in 1:M){
    datlist[[ll]] <- datlist[[ll]][ , -c(1:4) ]
                }
                
# nested multiple imputation using mice
imp1 <- mice.nmi( datlist ,  m=4 , maxit=3 )
summary(imp1)
# apply linear model and use summary method for all analyses of imputed datasets
res1 <- with( imp1 , lm( ASMMAT ~ migrant + female ) )	
summary(res1)

# convert mids.nmi object into an object of class NestedImputationList
datlist1 <- mids2datlist( imp1 )
datlist1 <- NestedImputationList( datlist1 )

# use with function
res1b <- with( datlist1 , glm( ASMMAT ~ migrant + female ) )

# use within function for data transformations
datlist2 <- within( datlist1 , {
            highsc <- 1*(ASSSCI > 600)
            books_dum <- 1*(books>=3)
            rm(scsci)   # remove variable scsci
                    } )
}
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{with}
\keyword{summary}
\keyword{within}
