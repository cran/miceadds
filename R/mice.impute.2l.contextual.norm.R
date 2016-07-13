mice.impute.2l.contextual.norm <- function (y, ry, x, type , ridge = 10^(-5) , 
			imputationWeights = NULL , interactions=NULL , quadratics = NULL ,  ...){									
	res <- mice_imputation_get_states( pos = base::parent.frame(n=1) )	
	vname <- res$vname
	newstate <- res$newstate   
	# data preparation
	xcov <- .a2l.contextual.auxiliary( y = y  , ry=ry , x=x , type=type , ...)     
	#------
	# norm imputation at level 2
	ximp <- mice.impute.weighted.norm( y= y , ry=ry, x = xcov , ridge = ridge , 
				imputationWeights = imputationWeights , 
                interactions= interactions , quadratics = quadratics ,  ... ) 
	base::return(ximp)
}

	
	

#......................................
.a2l.contextual.auxiliary <- function( y , ry , x , type , ...){
	# extract cluster index
	clusterx <- x[,type == -2 ]
	x1 <-  base::as.matrix(x[,type %in% c(1,2) ])
	if ( base::sum( type==2)  > 0 ){   
        z <-  as.matrix(x[,type == 2 ]) 
        # calculate aggregated values
        a1 <- stats::aggregate( z , base::list( clusterx ) , base::mean , na.rm=FALSE)
        colnames(a1)[-1] <- base::paste0( "M." , base::colnames(z) )
    } 
	# calculate aggregated value for y
	a21 <- stats::aggregate( y , base::list( clusterx ) , base::sum , na.rm=FALSE)
	a22 <- stats::aggregate( 1+0*y , base::list( clusterx ) , base::sum , na.rm=FALSE)
	ic <- base::match( clusterx , a21[,1] )
	y2 <- ( a21[ ic , 2] - y ) / ( a22[ ic , 2 ] - 1 )
	y2[ base::is.na(y2) ] <- base::mean(y2,na.rm=TRUE)                   
	if ( base::sum( type==2)  > 0 ){   
		xcov <- base::as.matrix( base::cbind(  x1 , a1[ ic , -1 ] , y2 ) )
    } else {
		xcov <- base::as.matrix( base::cbind(  x1 ,  y2 ) )
    }
	vname <- base::get("vname", pos = base::parent.frame()) 
	base::colnames(xcov)[ base::ncol(xcov) ] <- base::paste0("M1." , vname )
	base::return(xcov)     
}
###########################################################################################


