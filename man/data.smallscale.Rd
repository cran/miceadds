\name{data.smallscale}
\alias{data.smallscale}
\docType{data}
\title{
Small-Scale Dataset for Testing Purposes (Moderate Number of Cases, 
Many Variables)
}
\description{
Small-scale dataset for testing purposes (moderate number of cases, 
many variables)
}

\usage{
data(data.smallscale)
}

\format{
  A data frame with 675 observations on the following 164 variables.
  The format is
  
\code{'data.frame':   675 obs. of  164 variables:} \cr
\code{ $ v1  : num  3 3 2 3 3 0 1 0 3 NA ...} \cr
\code{ $ v2  : num  3 0 1 3 0 0 0 3 2 NA ...} \cr
\code{ $ v3  : num  0 0 2 3 2 0 1 0 0 NA ...} \cr
\code{ $ v4  : num  1 3 3 3 NA 0 0 0 3 NA ...} \cr
\code{ $ v5  : num  0 0 3 3 0 0 3 1 3 3 ...} \cr
\code{ $ v6  : num  8 8 9 8 9 9 9 8 9 9 ...} \cr
\code{[...]} \cr

}

%%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%%}

%%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%%}
%%\references{
%%  ~~ possibly secondary sources and usages ~~
%%}
\examples{
\dontrun{
data(data.smallscale)
data <- as.matrix( data.smallscale )

#****
# Model 1: Predictive mean matching

# define imputation methods
impmeth1 <- rep( "pmm" , ncol(data) )
names(impmeth1) <- colnames(data)

impmeth3 <- gsub( "pmm" , "pmm3" , impmeth1 )
impmeth4 <- gsub( "pmm" , "pmm4" , impmeth1 )
impmeth5 <- gsub( "pmm" , "pmm5" , impmeth1 )
impmeth6 <- gsub( "pmm" , "pmm6" , impmeth1 )
impmeth7 <- gsub( "pmm" , "fastpmm" , impmeth1 )

# compare timing using the rbenchmark package
library(rbenchmark)
# define imputation functions
fct_pmm <- function(){ mice( data , m = 1 , maxit=1 ,  imputationMethod = impmeth1 ) } 
fct_pmm3 <- function(){ mice( data , m = 1 , maxit=1 ,  imputationMethod = impmeth3 ) } 
fct_pmm4 <- function(){ mice( data , m = 1 , maxit=1 ,  imputationMethod = impmeth4 ) } 
fct_pmm5 <- function(){ mice( data , m = 1 , maxit=1 ,  imputationMethod = impmeth5 ) }
fct_pmm6 <- function(){ mice( data , m = 1 , maxit=1 ,  imputationMethod = impmeth6 ) }
fct_fastpmm <- function(){ mice( data , m = 1 , maxit=1 ,  imputationMethod = impmeth7 ) }

res <- rbenchmark::benchmark(fct_pmm(),fct_pmm4(),fct_pmm3(),fct_pmm5(),fct_pmm6(),
                             fct_fastpmm() ,
                             columns=c("test", "replications", "elapsed",
                                       "relative", "user.self", "sys.self"),
                             order="relative" , replications = 1)
##              test replications elapsed relative user.self sys.self
##   5    fct_pmm6()            1   14.90    1.000     14.84     0.08
##   3    fct_pmm3()            1   15.69    1.053     15.70     0.01
##   6 fct_fastpmm()            1   15.92    1.068     15.90     0.06
##   4    fct_pmm5()            1   16.07    1.079     15.97     0.03
##   1     fct_pmm()            1   17.53    1.177     17.38     0.03
##   2    fct_pmm4()            1   20.35    1.366     20.25     0.02
              
}
}
\keyword{datasets}
