\name{mice.impute.2lonly.pmm2}
\alias{2lonly.pmm2}
\alias{mice.impute.2lonly.pmm2}
\alias{2lonly.norm2}
\alias{mice.impute.2lonly.norm2}


\title{Imputation at Level 2 (in \pkg{miceadds})}


\description{
These functions are just bug fixes of 
\code{\link[mice:mice.impute.2lonly.pmm]{mice.impute.2lonly.pmm}} and
\code{\link[mice:mice.impute.2lonly.norm]{mice.impute.2lonly.norm}} in
\pkg{mice} (version 2.21).
}



\usage{
mice.impute.2lonly.pmm2(y, ry, x, type, ...)
mice.impute.2lonly.norm2(y, ry, x, type, ...)
}


\arguments{
  \item{y}{Incomplete data vector of length \code{n}}

  \item{ry}{Vector of missing data pattern
  (\code{FALSE}=missing, \code{TRUE}=observed)}

  \item{x}{Matrix (\code{n} x \code{p}) of complete
  covariates. Only numeric variables are permitted for
  usage of this function.}

  \item{type}{Group identifier must be specified by '-2'.
  Predictors must be specified by '1'.}

  \item{...}{Other named arguments.}
}

\value{
A vector of length \code{nmis} with imputations.
}



% Link to functions in other packages
%\code{\link[TAM:tam.fa]{tam.fa}}

% \details{
% This function allows in combination with
% }


% \examples{
% }


\author{
Alexander Robitzsch
}




\seealso{
\code{\link[mice:mice.impute.2lonly.pmm]{mice.impute.2lonly.pmm}} (\pkg{mice}),
\code{\link[mice:mice.impute.2lonly.norm]{mice.impute.2lonly.norm}} (\pkg{mice})
}

