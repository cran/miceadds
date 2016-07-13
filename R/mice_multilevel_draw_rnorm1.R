
########################################################################
# draw a vector of random variables using a Cholesky decomposition
# input mean vector and covariance matrix
mice_multilevel_draw_rnorm1 <- function( mu  , Sigma){	
	#----	
	#b.star <- b.star + as.vector( t(base::chol(vcov(fit))) %*% rnorm(length(b.star)) )		
	NP <- base::length(mu)	
	res <- mu + base::as.vector( base::t( base::chol(Sigma) %*% stats::rnorm(NP) ) )	
	base::return(res)
}