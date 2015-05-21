\name{lm.cluster}
\alias{lm.cluster}
\alias{glm.cluster}
\alias{coef.lm.cluster}
\alias{coef.glm.cluster}
\alias{vcov.lm.cluster}
\alias{vcov.glm.cluster}
\alias{summary.lm.cluster}
\alias{summary.glm.cluster}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cluster Robust Standard Errors for Linear Models and General Linear Models
}

\description{
Computes cluster robust standard errors for linear model
(\code{\link[stats:lm]{lm}}) and general linear models 
(\code{\link[stats:glm]{glm}}) using the 
\code{\link[multiwayvcov:cluster.vcov]{cluster.vcov}} 
function in the \pkg{multiwayvcov} package.
}

% \code{\link[TAM:tam.fa]{tam.fa}} (\pkg{TAM})
% multiwayvcov::cluster.vcov( model = mod , cluster = dfr)

\usage{
lm.cluster(data, formula, cluster, ...)

glm.cluster(data, formula, cluster, ...)

\method{summary}{lm.cluster}(object,...)
\method{summary}{glm.cluster}(object,...)

\method{coef}{lm.cluster}(object,...)
\method{coef}{glm.cluster}(object,...)

\method{vcov}{lm.cluster}(object,...)
\method{vcov}{glm.cluster}(object,...)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Data frame
}
  \item{formula}{
An \R formula
}
  \item{cluster}{
Variable name for clustering contained in \code{data}
}
  \item{\dots}{
Further arguments to be passed to \code{\link[stats:lm]{lm}} and 
\code{\link[stats:glm]{glm}}
}
  \item{object}{
Object of class \code{lm.cluster} or \code{glm.cluster}
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
List with following entries
  \item{lm_res}{Value of \code{\link[stats:lm]{lm}}}
  \item{glm_res}{Value of \code{\link[stats:glm]{glm}}}
  \item{vcov}{Covariance matrix of parameter estimates}
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

\seealso{
\code{\link[stats:lm]{lm}}, \code{\link[stats:glm]{glm}},
\code{\link[multiwayvcov:cluster.vcov]{cluster.vcov}} (\pkg{multiwayvcov})
}


\examples{
#############################################################################
# EXAMPLE 1: Cluster robust standard errors data.ma01
#############################################################################

data(data.ma01)
dat <- data.ma01

#*** Model 1: Linear regression
mod1 <- lm.cluster( data = dat , formula = read ~ hisei + female , 
               cluster = "idschool" )
coef(mod1)
vcov(mod1)
summary(mod1)

#*** Model 2: Logistic regression
dat$highmath <- 1 * ( dat$math > 600 )   # create dummy variable
mod2 <- glm.cluster( data = dat , formula = highmath ~ hisei + female , 
                cluster = "idschool" , family="binomial")
coef(mod2)
vcov(mod2)
summary(mod2)		
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Cluster robust standard errors}
\keyword{coef}
\keyword{vcov}
