\name{mice.impute.2l.binary}
\alias{mice.impute.2l.binary}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Imputation of a Binary Variable From a Two-Level Logistic Regression Model
}

\description{
This function imputes values of a binary variable 
from a two-level logistic regression model
fitted with the \code{\link[lme4:glmer]{lme4::glmer}} function. 
See Snijders and Bosker (2012) and Zinn (2013) for details.
}

\usage{
mice.impute.2l.binary(y, ry, x, type, intercept=TRUE, 
    groupcenter.slope=FALSE, draw.fixed=TRUE, random.effects.shrinkage=10^-6,
    glmer.warnings=TRUE, ... )
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
Incomplete data vector of length \code{n}
}
  \item{ry}{
Vector of missing data pattern (\code{FALSE} -- missing,
\code{TRUE} -- observed)
}
  \item{x}{
Matrix (\code{n} x \code{p}) of complete predictors.
}
\item{type}{Type of predictor variable. The cluster identifier 
has type \code{-2}, fixed effects predictors without a random slope
type \code{1} and predictors with fixed effects and random effects
have type \code{2}.
}
  \item{intercept}{
Optional logical indicating whether the intercept should be included.
}
\item{groupcenter.slope}{Optional logical indicating whether covariates 
should be centered around group means}
\item{draw.fixed}{Optional Logical indicating whether fixed effects 
parameter should be randomly drawn}
\item{random.effects.shrinkage}{Shrinkage parameter for stabilizing the 
covariance matrix of random effects}
\item{glmer.warnings}{Optional logical indicating whether warnings from
\code{glmer} should be displayed}
  \item{\dots}{
Further arguments to be passed
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
A vector of length \code{nmis=sum(!ry)} with imputed values.
}

\references{
Snijders, T. A. B., & Bosker, R. J. (2012). \emph{Multilevel analysis: 
An introduction to basic and advanced multilevel modeling}.
Thousand Oaks, CA: Sage.

Zinn, S. (2013). \emph{An imputation model for multilevel binary data}.
NEPS Working Paper No 31.
}

\author{
Simon Grund
}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: TO BE INCLUDED
#############################################################################	
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{mice imputation method}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
