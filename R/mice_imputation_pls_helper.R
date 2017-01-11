

#------------------------------------------------------------------------
# auxiliary function for PLS imputation
mice_imputation_pls_helper <- function( newstate , vname , pls.impMethod , x , y , ry ,  
                imputationWeights = rep( 1 , length(y)) , 
                interactions , quadratics , pls.facs , 
				envir_pos = NULL , 
                ... ){	
				
    # interactions and quadratic terms
    interactions <- mice_imputation_extract_list_arguments( micearg = interactions , 
                           vname = vname , miceargdefault = NULL )
	names_x <- base::colnames(x)	
    interactions <- base::intersect( interactions , names_x )
    quadratics <- mice_imputation_extract_list_arguments( micearg = quadratics , 
                           vname = vname , miceargdefault = NULL )                
    quadratics <- base::setdiff( base::intersect( quadratics, names_x ), interactions)
	if ( base::is.vector(x) ){ 
		x <- base::matrix( x , ncol=1 ) 
	}
	# define variable type
    type <- base::rep(1, base::ncol(x)  )
    base::names(type) <- names_x
    pls.use <- FALSE
    if ( base::length( interactions)>0 ){ 
		type[ interactions ] <- 4
		pls.use <- TRUE  
	}
    if ( base::length( quadratics )>0 ){ 
		type[ quadratics ] <- 5
		pls.use <- TRUE  
	}
    if ( pls.use & base::is.null(pls.facs) ){ 
		pls.facs <- 10000 
	}  
	
    #.*.*.*..*.*.*..*.*.*.
    # PLS imputation if specified
    pls.facs <- mice_imputation_extract_list_arguments( micearg = pls.facs , 
                           vname = vname , miceargdefault = NULL )
	if ( base::is.null(envir_pos) ){
		envir_pos <- base::parent.frame(n=1)  
	}

	
	yimp <- NULL	
    if ( ! base::is.null(pls.facs) ){ 
			#@@@ included 2016-12-14
        yimp <- mice.impute.pls(y=y, ry=ry, x=x , type=type , pls.facs = pls.facs , 
                        pls.impMethod = pls.impMethod , 
                        imputationWeights = imputationWeights ,
						envir_pos = envir_pos , extract_data = FALSE , ... )           
    }
    res <- base::list( yimp = yimp , pls.facs = pls.facs )
    base::return(res)  
}
#------------------------------------------------------------------------

.aux.pls.imputation <- mice_imputation_pls_helper