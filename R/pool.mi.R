##############################################################
# Inference for multiply imputed datasets
# The following code is copied from the mice package
# and slightly modified.
pool_mi <- function( qhat , u=NULL , se=NULL ,
				dfcom = 1E7 , method="smallsample" ){
	#****
	# qhat	... List of parameter vectors
	# u		... List of covariance matrices
	# se	... List of parameter vectors of standard errors
	#****
	CALL <- base::match.call()
	
	eps <- 1E-100
	m <- base::length(qhat)
	k <- base::length(qhat[[1]] )
	
	if ( ! is.null(se) ){
		u <- base::list(1:m)
		for (ii in 1:m){
			u[[ii]] <- base::diag( se[[ii]]^2 )						
		}
	}
					
	q1 <- qhat[[1]]
	names1 <- base::names(q1)
	qhat <- base::unlist(qhat)
	qhat <- base::matrix( qhat , nrow=m , ncol=k , byrow=TRUE )
    qbar <- base::colMeans(qhat)
	u0 <- u
	u <- base::array( 0 , dim = c(m,k,k) )
	for (ii in 1:m){
		u[ii,,] <- u0[[ii]]
	}
    ubar <- base::apply( u, base::c(2, 3), base::mean )
    e <- qhat - base::matrix(qbar, nrow = m, ncol = k, byrow = TRUE)
    b <- ( base::t(e) %*% e )/(m - 1 + eps )
    t <- ubar + (1 + 1/m) * b
    r <- (1 + 1/m) * base::diag(b/ubar)
    lambda <- (1 + 1/m) * base::diag(b/t)
    df <- mice_df(m, lambda, dfcom, method)
    fmi <- (r + 2/(df + 3))/(r + 1)
	base::names(lambda) <- names1
	base::names(fmi) <- names1
	base::names(df) <- names1	
	base::names(r) <- names1
	base::names(qbar) <- names1
	base::rownames(t) <- base::colnames(t) <- names1	
	
	#----
	# include t values and standard errors
	tval <- qbar / base::sqrt( base::diag(t) )
	pval <- 2 * stats::pt( - base::abs(tval) , df = df )
	base::names(tval) <- base::names(pval) <- names1
	
	#********************************
	# class mipo
	res <- base::list(
		nmis = NA ,
		m = m, qhat = qhat, u = u, qbar = qbar, 
		ubar = ubar, b = b, t = t, r = r, dfcom = dfcom, df = df, 
		fmi = fmi, lambda = lambda, tval = tval , pval = pval ,
		qhat_names = names1 , call = CALL)
	base::class(res) <- "pool_mi"
	base::return(res)
}
###########################################################
# Calculation of degrees of freedom
mice_df <- function (m, lambda, dfcom, method){
	eps <- 1E-4
    lambda[lambda < eps ] <- eps
    dfold <- (m - 1 + eps )/lambda^2
    dfobs <- (dfcom + 1)/(dfcom + 3) * dfcom * (1 - lambda)
    df <- dfold * dfobs/(dfold + dfobs)
    if (method != "smallsample"){ 
        df <- dfold
	}
    base::return(df)
}
###########################################################
# This function is a modification of mitools::summary.MIresult
summary.pool_mi <-function(object,alpha=0.05, ...){
  cat("Multiple imputation results:\nCall: ")
#   lapply(object$call, function(a) {cat("      ");print(a)})
  print(object$call)
  out <- base::data.frame( results= object$qbar , 
				   se= sqrt(diag( object$t)) 
				         )						 
  crit <- stats::qt(alpha/2,object$df, lower.tail=FALSE)  
  out$t <- object$tval
  out$p <- object$pval  
  out$"(lower"<- out$results-crit*out$se
  out$"upper)"<- out$results+crit*out$se
  out$"missInfo" <- paste0(round(100*object$fmi,1), " %")
  print(out,...)
}
############################################################
coef.pool_mi <- function(object, ...){
	base::return(object$qbar)
}
vcov.pool_mi <- function(object, ...){
    base::return(object$t)
}		