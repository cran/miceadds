


mice_multilevel_impute_groupmean <- function (y, ry, x, type , grmeanwarning=TRUE,
			...){  
  # Written by Alexander Robitzsch, July 2012
	if ( ( base::ncol(x) > 2 ) & grmeanwarning ){
		base::warning("\nMore than one variable is requested to be aggregated.\n")   
	}
	# calculate aggregated values
	a1 <- stats::aggregate( x[, type %in% base::c(1,2) ] , 
							base::list( x[,type == -2] ) , base::mean , na.rm=TRUE)
	i1 <- base::match( x[,type == -2] , a1[,1] )
	ximp <- base::as.matrix(a1[i1,-1])
	base::colnames(ximp) <- base::paste( base::names(type)[ type %in% base::c(1,2) ] , 
								base::names(type)[ type == -2 ] , sep="." )
	base::return(ximp)
}
