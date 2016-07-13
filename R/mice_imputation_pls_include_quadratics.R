

mice_imputation_pls_include_quadratics <- function( pls.quadratics , 
	pls.interactions , x0 , x , pls.print.progress, xs )
{
    pls.quadratics <- base::union( pls.quadratics , pls.interactions )
    use.quad <- base::unique( base::intersect( base::colnames(x0), pls.quadratics ) )
    # exclude variables from constructing quadratic terms if they only possess 2 values
    h1 <- base::apply( base::as.matrix(x0[ ,use.quad]) , 2 , FUN = function(tt){ 
						base::length( base::table(tt) ) } )
    pls.quadratics <- base::intersect( pls.quadratics , use.quad[ h1 > 2 ] )
	
    if ( base::length( pls.quadratics ) > 0 ){
        use.quad <- base::unique( base::intersect( base::colnames(x0), pls.quadratics))
        x <- base::cbind( x , xs[ , use.quad ] * xs[, use.quad ] )
        base::colnames(x) <- base::paste0("x" , 1:(ncol(x)) )
        if( pls.print.progress ){  
            cat("\n" , paste("Created" , length(use.quad) ,"Quadratic Terms" , 
					substring( Sys.time() ,1) ) , "\n") ; flush.console() 
            cat("Quadratic terms of " , paste(use.quad,collapse=" ") , "\n" , sep="") 
			utils::flush.console()
        }
    }	
	res <- base::list( x = x , use.quad = use.quad )
	base::return(res)
}