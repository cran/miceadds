mice.impute.2l.groupmean <- function (y, ry, x, type , grmeanwarning=TRUE, ...){  
    if ( ( base::ncol(x) > 2 ) & grmeanwarning ){
		base::warning("\nMore than one variable is requested to be aggregated.\n") 
		}
	clusterx <- base::paste( x[,type==-2] )
	a1 <- base::rowsum( x[ , type %in% base::c(1,2) ] , clusterx ,  na.rm= TRUE )	
	a2 <- base::rowsum( 1+0*x[ , type %in% base::c(1,2) ] , clusterx ,  na.rm= TRUE )
    i1 <- base::match( clusterx  , base::as.numeric( base::rownames(a1) ) )
	ximp <- a1[i1,,drop=FALSE]  / a2[i1,,drop=FALSE]
    # calculate aggregated values
	base::colnames(ximp) <- base::paste( base::names(type)[ type %in% c(1,2) ] , 
									base::names(type)[ type == -2 ] )
    base::return(ximp)
}
