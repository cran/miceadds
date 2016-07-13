pca.covridge <- function( x , ridge = 1E-10 ){
    cx <- stats::cov(x)
    base::diag(cx) <- base::diag(cx) + ridge
    pcax <- stats::princomp( covmat=cx )
    L <- pcax$loadings
    sdev <- pcax$sdev
    D <- base::diag( pcax$sdev^2)
    # scores <- t( t(L) %*%  t( x ) )  # = x %*% L
	scores <- x %*% L
    res <- base::list( "loadings" = L , "scores" = scores , 
                "sdev" = sdev )
    base::return(res)            
}
