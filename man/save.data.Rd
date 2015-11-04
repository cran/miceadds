\name{save.data}
\alias{save.data}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
\R Utilities: Saving/Writing Data Files using \pkg{miceadds}
}

\description{
This function is a wrapper function for saving or writing data frames
or matrices.
}

\usage{
save.data( data, filename, type="Rdata", path=getwd(), row.names=FALSE, na=NULL, 
      suffix = NULL , suffix_space = "__" , ...)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
\item{data}{Data frame or matrix to be saved}
\item{filename}{
Name of data file
}
  \item{type}{
The type of file in which the data frame or matrix should be loaded.
This can be \code{Rdata} (for \R binary format, using
\code{\link[base:save]{base::save}}, 
\code{csv} (using \code{\link[utils:write.csv2]{utils::write.csv2}}), 
\code{csv} (using \code{\link[utils:write.csv]{utils::write.csv}}),
\code{table} (using \code{\link[utils:write.table]{utils::write.table}}) or 
\code{sav} (using \code{\link[sjmisc:write_spss]{sjmisc::write_spss}}).	  
  	  }
\item{path}{Directory from which the dataset should be loaded}
\item{row.names}{Optional logical indicating whether row names
should be included in saved \code{csv} or \code{csv2} files.}
\item{na}{Missing value handling. The default is \code{""} for \code{type="csv"}
and \code{type="csv2"} and is \code{"."} for \code{type="table"}.}
\item{suffix}{Optional suffix in file name.}
\item{suffix_space}{Optional place holder if a suffix is used.}
\item{\dots}{Further arguments to be passed to \code{save},
	\code{write.csv2}, \code{write.csv}, \code{write.table}
	or \code{sjmisc::write_spss}.
}
}

%\details{
%For writing \code{sav} files, an installation of PSPP is necessary.
%See \code{\link{write.pspp}}.
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
See \code{\link{load.Rdata}} and \code{\link{load.data}} 
for saving/writing \R data frames. 
}


\examples{
\dontrun{
#*** use data.ma01 as an example for writing data files using save.data
data(data.ma01)
dat <- data.ma01

# set a working directory
pf2 <- "P:/ARb/temp_miceadds"

# save data in Rdata format
save.data( dat , filename="ma01data"  , type="Rdata" , path=pf2)

# save data in table format
save.data( dat , filename="ma01data"  , type="table" , path=pf2 ,
            row.names=FALSE , na =".")

# save data in csv2 format
save.data( dat , filename="ma01data"  , type="csv2" , path=pf2 ,
            row.names=FALSE , na ="")

# save data in sav format 
save.data( dat , filename="ma02data"  , type="sav" ,  path=pf2 )
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{R utilities}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
