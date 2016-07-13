mice.impute.2l.groupmean.elim <- function (y, ry, x, type ,  ...){  
	if ( base::ncol(x) > 2){
		base::warning("\nOnly one variable is allowed to be aggregated.\n")
		}
    # aggregated mean of x
    clusterx <- base::paste( x[,type==-2] )
	a1 <- base::rowsum( x[ , type %in% base::c(1,2) ] , clusterx ,  na.rm= TRUE )	
	a2 <- base::rowsum( 1+0*x[ , type %in% base::c(1,2) ] , clusterx ,  na.rm= TRUE )	
    i1 <- base::match( clusterx  , base::rownames(a1) )
    ximp <- ( a1[i1,] - x[, type %in% base::c(1,2) , drop=FALSE] ) / ( a2[i1,] - 1 )
	ximp <- ximp[,1]
	ximp[ base::is.na( ximp) ] <- base::mean( ximp , na.rm=TRUE)
    base::return(ximp)
}
