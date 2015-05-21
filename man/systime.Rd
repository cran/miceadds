\name{systime}
\alias{systime}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
\R Utilities: Various Strings Representing System Time 
}
\description{
This function generates system time strings in several formats.
}
\usage{
systime()
}
%- maybe also 'usage' for other objects documented here.
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A vector with entries of system time (see Examples).
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
systime()
  ##  > systime()
  ##  [1] "2015-05-14 17:38:02" "2015-05-14"          "20150514"           
  ##  [4] "2015-05-14_1738"     "2015-05-14_1700"     "20150514_173802"    
  ##  [7] "20150514173802"      "SD70_20150514173802"
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{R utilities}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
