\name{miceadds-package}
\alias{miceadds-package}
\alias{miceadds}
\docType{package}
\title{
Some Additional Multiple Imputation Functions, Especially for \pkg{mice} 
}
\description{
    Contains some auxiliary functions for multiple imputation which 
    complements existing functionality in \R.
    In addition to some utility functions, main features include 
    plausible value imputation, multilevel imputation functions,
    imputation using partial least squares (PLS) for high dimensional 
    predictors and two-way imputation.
}

\details{
\code{DESCRIPTION} \pkg{miceadds} package

\tabular{ll}{
Package: \tab \pkg{miceadds} \cr
Type: \tab Package\cr
Version: \tab 1.1 \cr
Publication Year: \tab 2015\cr
License: \tab GPL (>= 2)\cr
URL: \tab \url{https://sites.google.com/site/alexanderrobitzsch/software} \cr
}

\itemize{

\item In addition to the usual \code{mice} imputation function which employs
parallel chains, the function \code{\link{mice.1chain}} does multiple 
imputation from a single chain. 

\item Imputation based on partial least squares regression is implemented
in \code{\link{mice.impute.2l.pls}}.

\item Unidimensional plausible value imputation for latent variables (or
variables with measurement error) in the \pkg{mice} sequential imputation
framework can be applied by using the method 
\code{\link{mice.impute.2l.plausible.values}}.

\item Imputations for questionnaire items can be
accomplished by two-way imputation (\code{\link{tw.imputation}}).

\item The \pkg{miceadds} package also includes some functions \R utility functions 
(e.g. \code{\link{write.pspp}}, \code{\link{ma.scale2}}).
}



}
\author{
Alexander Robitzsch \cr 
Federal Institute for Education Research, Innovation and Development of the 
Austrian School System (BIFIE Salzburg), Austria
\cr \cr
Maintainer: Alexander Robitzsch <a.robitzsch@bifie.at> \cr \cr
URL: \url{https://sites.google.com/site/alexanderrobitzsch/}
}


%\references{
%}



\seealso{
See other \R packages for conducting multiple imputation: 
\pkg{mice}, \pkg{Amelia}, \pkg{pan}, \pkg{mi}, \pkg{norm},
\pkg{BaBooN}, \pkg{VIM}, ... \cr

Some links to internet sites related to missing data:

\url{http://missingdata.lshtm.ac.uk/} \cr
\url{http://www.stefvanbuuren.nl/mi/} \cr
\url{http://www.bristol.ac.uk/cmm/software/realcom/} \cr
}

\examples{
##   
##   ::'''''''''''''''''''''''''''''''''::
##   :: miceadds 0.11-69 (2013-12-01)   ::
##   ::'''''''''''''''''''''''''''''''''::
##
##  ----------------------- mice at work ---------------------------------
##
##                         (\-.
##                         / _`> .---------.
##                 _)     / _)=  |'-------'|
##                (      / _/    |O   O   o|
##                 `-.__(___)_   | o O . o |
##                               `---------'
##   
##                                          oo__
##                                         <;___)------
##                                    oo__   " "
##                                   <;___)------     oo__
##                                     " "           <;___)------
##                                                     " "
}

\keyword{package}
