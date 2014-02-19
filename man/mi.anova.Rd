\name{mi.anova}
\alias{mi.anova}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Analysis of Variance for Multiply Imputed Data Sets (Using the \eqn{D_2} Statistic)
}
\description{
This function combines \eqn{F} values from analysis of variance using
the \eqn{D_2} statistic which is based on combining \eqn{\chi^2} statistics
(see Allison, 2001; \code{\link{micombine.F}}, \code{\link{micombine.chisquare}}).
}
\usage{
mi.anova(mi.res, formula)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mi.res}{
Object of class \code{mids} or \code{mids.1chain}
}
  \item{formula}{
Formula for \code{lm} function. Note that this can be also a string.
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
A list with the following entries:
\item{r.squared}{Explained variance \eqn{R^2}}
\item{anova.table}{ANOVA table}
}

\references{
Allison, P. D. (2002). \emph{Missing data}. Newbury Park, CA: Sage.
}


\author{
Alexander Robitzsch
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{micombine.F}}, \code{\link{micombine.chisquare}}
}

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: nhanes2 data | two-way ANOVA
#############################################################################

library(mice)
data(nhanes2, package="mice")
set.seed(9090)

# nhanes data in one chain and 8 imputed datasets
mi.res <- mice.1chain( nhanes2 , burnin=4 , iter=20 , Nimp=8 )
# 2-way analysis of variance
an2 <- mi.anova(mi.res=mi.res, formula="bmi ~ age * hyp")
##   Univariate ANOVA for Multiply Imputed Data 
##   
##   lm Formula:  bmi ~ age * hyp
##   R^2=0.201354 
##   ..........................................................................
##   ANOVA Table 
##                      SSQ df1       df2 F value  Pr(>F)    eta2 partial.eta2
##   age          61.45624   2  42.19467  1.2055 0.30964 0.15483      0.16238
##   hyp          16.17124   1 638.27517  0.8599 0.35412 0.04074      0.04854
##   age:hyp       2.29723   1 669.47653  0.0782 0.77981 0.00579      0.00719
##   Residual    317.01177  NA        NA      NA      NA      NA           NA
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ANOVA}
\keyword{mids}
