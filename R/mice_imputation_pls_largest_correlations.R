
mice_imputation_pls_largest_correlations <- function( y , x , ry , type ,
	use.ymat , pls.print.progress , x10 , N.largest , min.all.cor )
{
	
	# compute correlations
	c1 <- mice_imputation_pls_correlation_criteria( y , x , ry , use.ymat)	
    elim.ind <- base::which( base::abs(c1) < min.all.cor )
	N11 <- base::ncol(x)		

	Nelim <- base::length(elim.ind)
	if ( ( N11 - Nelim <= 1 ) & (N11>2) ){
		elim.ind <- elim.ind[ -c(1:2) ]
	}
	if ( base::length(elim.ind) > 0){ 
		x <- x[ , - elim.ind , drop=FALSE] 
		}
	N12 <- base::ncol(x)
	if ( pls.print.progress){ 
		cat("\n" , paste( N11 , " Predictor Variables" , sep="") )
		cat("\n" , "Minimal Absolute Correlation of min.all.cor =" , min.all.cor , "\n")				
		cat( "  Kept" , paste( N12 , "Predictor Variables" , names(y) ) , "\n")  
		utils::flush.console()
	}
																
	# look for largest correlations
	c1 <- mice_imputation_pls_correlation_criteria( y , x , ry , use.ymat)	

	#***---***---***---***---***---***---***---***---											
	if (N.largest>0){  # begin N.largest				 
		dfr1 <- data.frame( "index" = base::seq( 1 , base::ncol(x) ) , 
					"abs.cor" = base::abs(as.vector(c1)) )
		dfr1 <- dfr1[ base::order( dfr1$abs.cor , decreasing=TRUE) , ]
		x <- x[ , dfr1[ 1:N.largest , "index" ] ]
		# look if some columns do have complete missing entries or SD of zero
		cmna1 <- base::which( colMeans( is.na(x))  == 1 )
		cmna2 <- base::which( base::apply( x , 2, stats::sd ) == 0 )
		cmna <- base::unique( base::sort( base::c( cmna1 , cmna2 ) ) )
	
		if ( base::length(cmna) > 0 ){ 
			x <- x[ , - cmna ] 
			N.largest <- base::ncol(x)
		}
		if ( pls.print.progress){ 
			cat("\n" , paste( N12 , " Predictor Variables" , sep="") )
			cat("\n" , "Select Predictors with" , N.largest , "Largest Correlations\n")	
			flush.console()
		}
	} # end N.largest
									
	if ( base::is.vector(x) ){ 
		x <- base::cbind( x , x10[,1:2] ) 
	}
	if ( base::dim(x)[2] == 0 ){ 
		x <-  x10[,1:2]  
		n0 <- base::dim(x)[1]
		eps <- 1E-20
		x[,1] <- x[,1] + stats::rnorm( n0 , sd = eps )
		x[,2] <- x[,2] + stats::rnorm( n0 , sd = eps )
	}
	#-- output
	res <- base::list( x = x , NX = base::ncol(x) )
	base::return(res)					
}					