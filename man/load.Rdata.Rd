\name{load.Rdata}
\alias{load.Rdata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
\R Utilities: Loading \code{Rdata} Files in a Convenient Way
}
\description{
This function loads a \code{Rdata} object saved as a data frame or a matrix
in the current \R environment.
}
\usage{
load.Rdata(filename, objname)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{filename}{
Rdata file (matrix or data frame)
}
  \item{objname}{
Object name. This obejct will be a global variable in \R.
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

\seealso{
See also \code{\link{save.Rdata}} for saving data frames in
an \code{Rdata} format.
}
\examples{
\dontrun{
# load a data frame in the file "data_s3.Rdata" and save this
# as the object "dat.s3"
load.Rdata( filename = "data_s3.Rdata" , "dat.s3" )
head(dat.s3) }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{R utilities}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
