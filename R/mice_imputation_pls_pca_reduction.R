

mice_imputation_pls_pca_reduction <- function(x , pcamaxcols ,
	pls.print.progress)
{						
	if ( base::ncol(x) > pcamaxcols ){
	    a0 <- base::Sys.time()
		NX <- base::nrow(x)
		xdims <- base::min( pcamaxcols , NX-2 )
		if( pls.print.progress ){ 
			cat("\nDimension reduction with Principal Components Analysis\n")
			if (pcamaxcols > 1){
				cat("Dimension of X:" , NX , " -> Dimension reduction to " , 
						xdims , "dimensions\n")
			}
			if (pcamaxcols < 1){
				cat("Dimension of X:" , NX , " -> Dimension reduction to " , 
						100 * pcamaxcols , "% of total variance\n")
			}
		}				
		x <- base::as.matrix(x)
		xpca <- pca.covridge(x=x)
		varexpl <- xpca$sdev^2 
		varexpl <- base::cumsum( varexpl / base::sum( varexpl) * 100 )
		xdims <- base::which( varexpl > 100*pcamaxcols )[1]
		if (pls.print.progress){
			cat( " ->" , xdims , "extracted dimensions\n")
			cat("Explained variance:" , round( varexpl[ xdims] , 2 )  , " % " )
		}
		x <- xpca$scores[ , 1:xdims , drop=FALSE]
		a1 <- Sys.time()
		if (pls.print.progress){
			cat("\nTime needed:" , a1-a0 , "\n")
		}
	}			
	#--- output
	base::return(x)
}
