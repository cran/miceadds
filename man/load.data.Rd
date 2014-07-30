\name{load.data}
\alias{load.data}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
\R Utilities: Loading/Reading Data Files using \pkg{miceadds}
}

\description{
This function is a wrapper function for loading or reading data frames
or matrices.
}

\usage{
load.data( filename , type="Rdata" , path=getwd() , ...)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
\item{filename}{
Data file (matrix or data frame)
}
  \item{type}{
The type of file in which the data frame or matrix should be loaded.
This can be \code{Rdata} (for \R binary format, using \code{\link{load.Rdata2}}), 
\code{csv} (using \code{\link{read.csv2}}), \code{csv} (using \code{\link{read.csv}}),
\code{table} (using \code{\link{read.table}}) or 
\code{sav} (using \code{\link[foreign:read.spss]{read.spss}} (\pkg{foreign})).	  
  	  }
\item{path}{Directory from which the dataset should be loaded}
\item{\dots}{Further arguments to be passed to \code{load.Rdata2},
	\code{read.csv2}, \code{read.csv}, \code{read.table}
	or \code{read.spss} (\pkg{foreign}).
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
See also \code{\link{load.Rdata}} for loading \R data frames.

See \code{\link{save.Rdata}} and \code{\link{save.data}} 
for saving/writing \R data frames. 
}


\examples{
\dontrun{
# load a data frame in the file "data_s3.Rdata" and save this
# as the object "dat.s3"
dat.s3 <- load.data( filename = "data_s3.Rdata" , type = "Rdata" )
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{R utilities}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
