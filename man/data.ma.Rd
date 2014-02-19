\name{data.ma}
\alias{data.ma}

\alias{data.ma01}
\alias{data.ma02}
\alias{data.ma03}
\alias{data.ma04}
\alias{data.ma05}

\docType{data}

\title{
Example Datasets for \pkg{miceadds} Package
}

\description{
Example datasets for \pkg{miceadds} package.
}


\usage{
data(data.ma01)
data(data.ma02)
data(data.ma03)
data(data.ma04)
data(data.ma05)
}


\format{
\itemize{
\item Dataset \code{data.ma01}: 

Dataset with students nested within school and
student weights (\code{studwgt}). The format is

\code{'data.frame':   4073 obs. of  11 variables:} \cr
\code{ $ idstud  : num  1e+07 1e+07 1e+07 1e+07 1e+07 ...} \cr
\code{ $ idschool: num  1001 1001 1001 1001 1001 ...} \cr
\code{ $ studwgt : num  6.05 6.05 5.27 5.27 6.05 ...} \cr
\code{ $ math    : int  594 605 616 524 685 387 536 594 387 562 ...} \cr
\code{ $ read    : int  647 651 539 551 689 502 503 597 580 576 ...} \cr
\code{ $ migrant : int  0 0 0 1 0 0 1 0 0 0 ...} \cr
\code{ $ books   : int  6 6 5 2 6 3 4 6 6 5 ...} \cr
\code{ $ hisei   : int  NA 77 69 45 66 53 43 NA 64 50 ...} \cr
\code{ $ paredu  : int  3 7 7 2 7 3 4 NA 7 3 ...} \cr
\code{ $ female  : int  1 1 0 0 1 1 0 0 1 1 ...} \cr
\code{ $ urban   : num  1 1 1 1 1 1 1 1 1 1 ...} \cr

\item Dataset \code{data.ma02}: 

10 multiply imputed datasets of incomplete data \code{data.ma01}.
The format is

\code{List of 10} \cr
\code{ $ :'data.frame':       4073 obs. of  11 variables:} \cr
\code{ $ :'data.frame':       4073 obs. of  11 variables:} \cr
\code{ $ :'data.frame':       4073 obs. of  11 variables:} \cr
\code{ $ :'data.frame':       4073 obs. of  11 variables:} \cr
\code{ $ :'data.frame':       4073 obs. of  11 variables:} \cr
\code{ $ :'data.frame':       4073 obs. of  11 variables:} \cr
\code{ $ :'data.frame':       4073 obs. of  11 variables:} \cr
\code{ $ :'data.frame':       4073 obs. of  11 variables:} \cr
\code{ $ :'data.frame':       4073 obs. of  11 variables:} \cr
\code{ $ :'data.frame':       4073 obs. of  11 variables:} \cr

\item Dataset \code{data.ma03}: 

This dataset contains one variable
\code{math_EAP} for which a conditional posterior distribution with EAP
and its associated standard deviation is available.

\code{'data.frame':   120 obs. of  8 variables:} \cr
\code{ $ idstud    : int  1001 1002 1003 1004 1005 1006 1007 1008 1009 1010 ...} \cr
\code{ $ female    : int  0 1 1 1 1 0 1 1 1 1 ...} \cr
\code{ $ migrant   : int  1 1 0 1 1 0 0 0 1 0 ...} \cr
\code{ $ hisei     : int  44 NA 26 NA 32 60 31 NA 34 26 ...} \cr
\code{ $ educ      : int  NA 2 NA 1 4 NA 2 NA 2 NA ...} \cr
\code{ $ read_wle  : num  74.8 78.1 103.2 81.2 119.2 ...} \cr
\code{ $ math_EAP  : num  337 342 264 285 420 ...} \cr
\code{ $ math_SEEAP: num  28 29.5 28.6 28.5 27.5 ...} \cr


\item Dataset \code{data.ma04}: 

This dataset contains two hypothetical
scales \code{A} and \code{B} and single variables \code{V5}, \code{V6} and
\code{V7}.

\code{'data.frame':   281 obs. of  13 variables:} \cr
\code{ $ group: int  1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{ $ A1   : int  2 2 2 1 1 3 3 NA 2 1 ...} \cr
\code{ $ A2   : int  2 2 2 3 1 2 4 4 4 4 ...} \cr
\code{ $ A3   : int  2 3 3 4 1 3 2 2 2 4 ...} \cr
\code{ $ A4   : int  3 4 6 4 7 5 3 5 5 1 ...} \cr
\code{ $ V5   : int  2 2 5 5 4 3 4 1 3 4 ...} \cr
\code{ $ V6   : int  2 5 5 1 1 3 2 2 2 4 ...} \cr
\code{ $ V7   : int  6 NA 4 5 6 2 5 5 6 7 ...} \cr
\code{ $ B1   : int  7 NA 6 4 5 2 5 7 3 7 ...} \cr
\code{ $ B2   : int  6 NA NA 6 3 3 4 6 6 7 ...} \cr
\code{ $ B3   : int  7 NA 7 4 3 4 3 7 5 NA ...} \cr
\code{ $ B4   : int  4 5 6 5 4 3 4 5 2 1 ...} \cr
\code{ $ B5   : int  7 NA 7 4 4 3 5 7 5 4 ...} \cr


\item Dataset \code{data.ma05}:

This is a two-level dataset with students nested within classes. Variables
at the student level are \code{Dscore}, \code{Mscore}, \code{denote},
\code{manote}, \code{misei} and \code{migrant}. Variables at the class
level are \code{sprengel} and \code{groesse}.

\code{'data.frame':   1673 obs. of  10 variables:} \cr
\code{ $ idstud  : int  100110001 100110002 100110003 100110004 100110005  ...} \cr
\code{ $ idclass : int  1001 1001 1001 1001 1001 1001 1001 1001 1001 1001 ...} \cr
\code{ $ Dscore  : int  NA 558 643 611 518 552 NA 534 409 543 ...} \cr
\code{ $ Mscore  : int  404 563 569 621 653 651 510 NA 517 566 ...} \cr
\code{ $ denote  : int  NA 1 1 1 3 2 3 2 3 2 ...} \cr
\code{ $ manote  : int  NA 1 1 1 1 1 2 2 2 1 ...} \cr
\code{ $ misei   : int  NA 51 NA 38 NA 50 53 53 38 NA ...} \cr
\code{ $ migrant : int  NA 0 0 NA 0 0 0 0 0 NA ...} \cr
\code{ $ sprengel: int  0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{ $ groesse : int  25 25 25 25 25 25 25 25 25 25 ...} \cr

}
}


\section{Example Index}{

%%%%%%% data.ma01
\emph{Dataset} \code{data.ma01} 	
	
\code{\link{mice.1chain}} (Example 3),
\code{\link{mice.impute.weighted.pmm}} (Example 1),

%%%%%%% data.ma02
\emph{Dataset} \code{data.ma02} 	
	
\code{\link{fast.groupmean}} (Example 1),


%%%%%%% data.ma03
\emph{Dataset} \code{data.ma03} 	
	
\code{\link{mice.impute.2l.eap}} (Example 1)



%%%%%%% data.ma04
\emph{Dataset} \code{data.ma04} 	
	
\code{\link{mice.impute.2l.plausible.values}} (Example 1)



%%%%%%% data.ma05
\emph{Dataset} \code{data.ma05} 	
	
\code{\link{mice.impute.2l.contextual.pmm}} (Example 1),
\code{\link{mice.impute.2l.latentgroupmean}} (Example 1)


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
%\examples{
%data(data.ma01)
%## maybe str(data.ma01) ; plot(data.ma01) ...
%}
\keyword{datasets}