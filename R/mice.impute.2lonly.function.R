


#******************************************
# general imputation function at level 2
mice.impute.2lonly.function <- function( y , ry , x , type , imputationFunction , ... ){
    if ( base::sum(type==-2 ) != 1 ){
        base::stop( "No cluster variable!\n")
    }
	
	#--- extract arguments
	res <- mice_imputation_get_states(pos = base::parent.frame() )	
	vname <- res$vname
	imputationFunction_vname <- mice_imputation_extract_list_arguments( 
				micearg = imputationFunction , 
                vname = vname , miceargdefault = "norm" )
    # extract cluster index
    clusterx <- x[,type == -2 ]
	# calculate aggregated values
    x <- base::cbind(1, base::as.matrix(x[,type %in% base::c(1,2)]))      
    a2 <- base::rowsum( base::cbind(x,y) , clusterx , na.rm=FALSE)
	#~~~~~
	clusterx0 <- base::as.numeric( base::paste0( base::rownames(a2)))
    a2 <- a2 / base::rowsum( 1+0*y , clusterx , na.rm=FALSE )[,1] 
    a1 <- base::cbind( clusterx0  , a2 )
	#~~~~~
    N1 <- base::ncol(a1)
    cly2 <- base::unique( clusterx[ ry ] )  # clusters without missings on y
    ry2 <- a1[,1] %in% cly2  
    x1 <- base::as.matrix(a1[, - base::c(1,N1)])
	#*** collect arguments and apply general imputation method
	args <- base::list( y= base::as.matrix(a1[,N1]), ry=ry2, x = x1[,-1] , ... )
	imp_function <- base::paste0("mice.impute." , imputationFunction_vname )
	ximp2 <- base::do.call( imp_function , args )
    #*** data postprocessing
    cly2 <- a1[ ! ry2 , 1] 
    i1 <- base::match( clusterx, cly2 )
    ximp <- ( ximp2[i1] )[ ! ry ]
    base::return(ximp)	
}