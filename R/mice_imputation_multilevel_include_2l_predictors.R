



#-----------------------------------------------------------------------------
# function for inclusion of group level predictors
mice_imputation_multilevel_include_2l_predictors <- function( y, x , ry , type , ... ){
        X <- base::as.matrix( x[ , type %in% base::c(1,2)] )
        X <- base::cbind( 1 , X )        
        # group level predictors
        if ( base::sum( type == -2 ) > 0 ){
            cluster <- x[ , type == -2 ]
			if ( base::sum( type == 2 ) > 0 ){          
				x1a <- base::as.matrix( base::cbind( x[ , type== - 2 ] , 
								x[ , type== 2 ]  ) )
				base::colnames(x1a) <- base::c( base::colnames(x)[ type == -2 ] , 
								base::colnames(x)[ type == 2 ] )
				gm0 <- mice.impute.2l.groupmean(y = y , ry = FALSE * ry , x = x1a, 
							type = base::c( -2 , base::rep(1, base::ncol(x1a)-1 ) ),
							grmeanwarning=FALSE , ... )
				gm0 <- base::as.matrix(gm0)
				base::colnames(gm0) <- base::paste0("M." , base::colnames(x1a)[-1] )
				X <- base::as.matrix( base::cbind( X , gm0 ))
            }
        }  else { 
			cluster <- NULL 
		}
		# res <- mice_imputation_get_states()	
		# vname <- res$vname
        # base::return(res)
		base::return(X)
}
#-----------------------------------------------------------------------------
          
.include.2l.predictors <- mice_imputation_multilevel_include_2l_predictors		  