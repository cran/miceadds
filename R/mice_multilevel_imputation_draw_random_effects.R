
################################################
# draw of random effects
# INPUT:
# means mu (NG , NR )
# covariance matrices Sigma (NR,NR,NG)

mice_multilevel_imputation_draw_random_effects <- function( mu , Sigma ,
		ridge = 1E-50 ){

	dim_Sigma <- base::dim(Sigma)
	ngr <- dim_Sigma[3]
	NR <- dim_Sigma[1]
	# draw random effects
	u <- base::matrix(0, nrow=ngr, ncol=NR)
	if (NR==1){
		u[,1] <- stats::rnorm(ngr, mean=mu[,1], sd= base::sqrt(Sigma[1,1,]) )
	} else {
		for(i in 1:ngr){
			#-- compute covariance matrix with ridge		
			Sigma1 <- Sigma[,,i] + base::diag(ridge,NR)
			# Cholesky matrix of Sigma1
			Sigma_chol <- base::chol(Sigma1)
			# simulation
			rn <- stats::rnorm(NR, mean=0 , sd=1)
			u[i,] <- mu[i,] + base::as.vector( base::t( Sigma_chol ) %*% rn )
		}
	}
	base::return(u)
}	