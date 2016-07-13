#############################################
# Rcpp version of kernel PLS regression
kernelpls.fit2 <- function(X, Y, ncomp ){
	e1 <- base::environment()
	tsqs <- NULL
    ## Save dimnames:
    dnX <- base::dimnames(X)
    dnY <- base::dimnames(Y)
    nobj <- base::dim(X)[1]
    npred <- base::dim(X)[2]
    nresp <- base::dim(Y)[2]
	if (nresp>1){
	    base::stop("PLS regression is only provided for one-dimensional responses.")
	}
    ## Center variables:
    Xmeans <- base::colMeans(X)
    X <- X - base::rep(Xmeans, each = nobj)
    Ymeans <- base::colMeans(Y)
    Y <- Y - base::rep(Ymeans, each = nobj)
	# apply Rcpp function
	res <- kernelpls_1dim(Y,X , comp=ncomp)
	.attach.environment( res=res , envir=e1 )	
	#****
	# output management copied from kernelpls.fit function
	# from the pls package
	residuals <- - fitted + base::c(Y)
    fitted <- fitted + base::rep(Ymeans, each = nobj) # Add mean
    ## Add dimnames:
    objnames <- dnX[[1]]
    if ( base::is.null(objnames) ){
		objnames <- dnY[[1]]
	}
    prednames <- dnX[[2]]
    respnames <- dnY[[2]]
    compnames <- base::paste("Comp", 1:ncomp)
    nCompnames <- base::paste(1:ncomp, "comps")
    base::dimnames(TT) <- base::dimnames(U) <- base::list(objnames, compnames)
    base::dimnames(R) <- base::dimnames(W) <- base::dimnames(P) <-
							base::list(prednames, compnames)
    base::dimnames(tQ) <- base::list(compnames, respnames)
    base::dimnames(B) <- base::list(prednames , nCompnames)
    base::dimnames(fitted) <- base::dimnames(residuals) <-
							base::list(objnames,  nCompnames)
    base::class(TT) <- base::class(U) <- "scores"
    base::class(P) <- base::class(W) <- base::class(tQ) <- "loadings"

    res <- base::list(coefficients = B,
				scores = TT, loadings = P,
				loading.weights = W,
				Yscores = U, Yloadings = base::t(tQ),
				projection = R,
				Xmeans = Xmeans, Ymeans = Ymeans,
				fitted.values = fitted, residuals = residuals,
				Xvar = base::colSums(P * P) * tsqs,
				Xtotvar = base::sum(X * X) )
	# R^2 measures
	R2 <- base::cumsum(res$Xvar) / res$Xtotvar
	R21 <- base::sapply( 1:ncomp , FUN = function(cc){
             1 - stats::var( Y[,1] -  res$fitted.values[,cc] ) / stats::var( Y[,1] )
          } )
	R2 <- base::rbind( R2 , R21)
	base::rownames(R2) <- base::c("R2(X)" , "R2(Y)")
	base::colnames(R2) <- compnames
	res$R2 <- R2
	base::class(res) <- "kernelpls.fit2"
	base::return(res)
}
#######################################################
# attach all elements in a list in a local environment
.attach.environment <- function( res , envir ){
	CC <- base::length(res)
	for (cc in 1:CC){
		base::assign( base::names(res)[cc] , res[[cc]] , envir=envir )		
	}
}
########################################################
# Call to Rcpp function
kernelpls_1dim <- function (Y,X,comp){ 
	res <- base::.Call("kernelpls_1dim_C", 
				Y,X,comp, 
				PACKAGE = "miceadds")
	return(res)
}
##########################################################