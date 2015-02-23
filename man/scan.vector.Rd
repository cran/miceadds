\name{scan.vec}
\alias{scan.vec}
\alias{scan.vector}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
\R Utilities: Scan a Character Vector
}
\description{
This function splits a string into a character vector.
}
\usage{
scan.vec(vec)	
scan.vector(vec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vec}{
A string which should be splitted according to blanks
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
vars <- scan.vector( "female urbgrad  groesse  Nausg    grpgroesse   privat  ")
  ## Read 6 items
vars
  ## [1] "female"     "urbgrad"    "groesse"    "Nausg"      "grpgroesse"
  ## [6] "privat"    
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{R utilities}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
