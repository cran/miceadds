\name{cxxfunction.copy}
\alias{cxxfunction.copy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
\R Utilities: Copy of an \code{Rcpp} File
}
\description{
Copy the \code{Rcpp} function into the working directory.
}
\usage{
cxxfunction.copy(cppfct , name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cppfct}{
The Rcpp function
}
  \item{name}{
Name of the output Rcpp function to be generated
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
\references{
Eddelbuettel, D. & Francois, R. (2011). Rcpp: Seamless R and C++ integration.
\emph{Journal of Statistical Software}, \bold{40(8)}, 1-18.
}
\author{
Alexander Robitzsch
}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also the \code{cxxfunction} in the \pkg{inline} package
}
\examples{
\dontrun{
# define Rcpp file
code1 <- "
    // array A
    Rcpp::NumericMatrix AA(A);
    // Rcpp::IntegerVector dimAA(dimA);
    int nrows = AA.nrow();
    int ncolumns = AA.ncol();
    NumericMatrix Alogis(nrows,ncolumns)  ; 
    // compute logistic distribution
    for (int ii=0; ii<nrows; ++ii){
        NumericVector h1=AA.row(ii) ;
        NumericVector res = plogis( h1 ) ;
        for (int jj=0;jj<ncolumns;++jj){
            Alogis(ii,jj) = res[jj] ;
                        }
                    }
                
    return( wrap(Alogis) );
    "
# compile Rcpp code
calc1 <- cxxfunction( signature( A= "matrix"), code1, plugin = "Rcpp", verbose=TRUE )
cxxfunction.copy( cppfct=calc1, name="calclogis" )
    }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{R utilities}
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
