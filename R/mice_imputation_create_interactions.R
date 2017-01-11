


################################################
# create interactions
mice_imputation_create_interactions <- function (y_, xobs_, xall_, 
	index_int_, min_int_cor_, maxcols_ )
{ 
	res <- base::.Call("create_interactions_cpp", 
				y_, xobs_, xall_, index_int_, min_int_cor_, maxcols_ , 
				PACKAGE = "miceadds")
		# List of 5
		# $ index_int   
		# $ xint        
		# $ allcorrs    
		# $ min_int_cor 
		#  $ N_interactions				
	r1 <- res$allcorrs
	r1[ base::is.na( r1[,1] ) , 1] <- 0
	#---- remove some interactions with SD = 0
	g00 <- base::which( r1[,1] %in% base::c(-Inf,Inf) )
	if ( base::length(g00) > 0 ){
		r1[ g00 , 1 ] <- 0
		r1[ g00 , 2 ] <- 0
		xint <- res$xint
		xint <- xint[ , - g00 ]
		res$N_interactions <- base::ncol(xint)
		res$xint <- xint
		res$allcorrs <- r1
	}	
	base::return(res)
}