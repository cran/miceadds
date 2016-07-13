

mice_imputation_pls_correlation_criteria <- function( y , x , ry , use.ymat)
{
	if ( ! use.ymat ){ 
		c1 <- stats::cor( y[ry] , x[ry,] ) 
	} else {  
		# look for correlations of all the dummy variables
		c1 <- stats::cor( y[ry,] , x[ry,] ) 
		c1 <- base::apply( base::abs(c1) , 2 , base::mean , na.rm=TRUE )
	}
	base::return(c1)
}
	