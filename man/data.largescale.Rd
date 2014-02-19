\name{data.largescale}
\alias{data.largescale}
\docType{data}

\title{
Large-scale Dataset for Testing Purposes (Many Cases, Few Variables)
}

\description{
Large-scale dataset with many cases and few variables 
included for testing purposes.
}

\usage{
data(data.largescale)
}

\format{
  A data frame with 14000 observations on the following 13 variables.
  The format is
  
\code{'data.frame':   14000 obs. of  13 variables:} \cr
\code{ $ id: num  1e+07 1e+07 1e+07 1e+07 1e+07 ...} \cr
\code{ $ D1: num  0 0 0 0 1 0 0 0 0 0 ...} \cr
\code{ $ D2: num  0 0 0 1 0 1 0 1 0 0 ...} \cr
\code{ $ D3: num  0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{ $ D4: num  0 0 0 1 0 0 0 1 0 0 ...} \cr
\code{ $ D5: num  0 0 0 0 0 1 0 0 0 0 ...} \cr
\code{ $ v1: num  118 117 94 106 86 117 96 96 82 95 ...} \cr
\code{ $ v2: num  101 101 86 101 65 94 72 75 70 99 ...} \cr
\code{ $ v3: num  0 0 0 0 0 1 0 0 0 0 ...} \cr
\code{ $ v4: num  3 NA 3 5 2 5 5 5 4 2 ...} \cr
\code{ $ v5: num  0 NA 0 0 0 1 0 0 0 0 ...} \cr
\code{ $ v6: num  3 3 3 4 NA 1 3 3 2 3 ...} \cr
\code{ $ v7: num  51 36 14 47 22 17 13 37 47 38 ...} \cr
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
\dontrun{
data(data.largescale)
    
# missing proportions
round( colMeans( is.na( data.largescale[,-1] )) , 3)
##   > round( colMeans( is.na( data.largescale[,-1] )) , 3)
##      D1    D2    D3    D4    D5    v1    v2    v3    v4    v5    v6    v7 
##   0.000 0.000 0.000 0.000 0.000 0.001 0.000 0.008 0.015 0.020 0.112 0.048

data <- data.largescale[,-1]

#*****
# Model 1: Multiple imputation with predictive mean matching
#     select imputation methods 'pmm', 'pmm3', 'pmm4' and 'pmm5'
impmeth1 <- rep( "pmm" , ncol(data) )
names(impmeth1) <- colnames(data)

impmeth3 <- gsub( "pmm" , "pmm3" , impmeth1 )
impmeth4 <- gsub( "pmm" , "pmm4" , impmeth1 )
impmeth5 <- gsub( "pmm" , "pmm5" , impmeth1 )
impmeth6 <- gsub( "pmm" , "pmm6" , impmeth1 )

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
##   > res
##              test replications elapsed relative user.self sys.self
##   5    fct_pmm6()            1    0.60    1.000      0.60     0.00
##   4    fct_pmm5()            1    0.74    1.233      0.75     0.00
##   2    fct_pmm4()            1    1.20    2.000      1.20     0.00
##   6 fct_fastpmm()            1    2.17    3.617      2.15     0.00
##   3    fct_pmm3()            1    4.76    7.933      4.38     0.37
##   1     fct_pmm()            1    6.04   10.067      6.02     0.00

## -> The methods 'pmm5'/'pmm6' are the fastest imputation methods, but they are
##    probably the crudest ones.
}
}

\keyword{datasets}
