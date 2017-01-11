
##################################################################
# application of scale for a list of multiply imputed datasets,
# single datasets or nested multiply imputed datasets
scale_datlist <- function( datlist , orig_var , trafo_var , weights = NULL ,
                    M=0, SD=1 , digits = NULL){
	is_dfr <- FALSE
	is_iL <- FALSE
    is_NIL <- FALSE
	is_ndl <- FALSE
	is_dl <- FALSE
	
	if ( base::class(datlist) == "nestedImputationList"){
		datlist <- nested.datlist_create( datlist )
		is_iL <- TRUE
	}
	
	if ( base::class(datlist) == "nested.datlist" ){
		datlist <- nested.datlist2datlist(datlist)
		Nimp <- base::attr(datlist , "Nimp" )
		is_ndl <- TRUE		
	}
	
	if ( base::class(datlist) == "imputationList"){
        datlist0 <- datlist
		datlist <- datlist_create( datlist )
		is_iL <- TRUE
	}

	if ( base::class(datlist) == "datlist"){
        datlist0 <- datlist
		datlist <- datlist_create( datlist )
		is_dl <- TRUE
	}

	#**** processing if datlist is a data frame
	if ( ! ( base::class(datlist) %in% base::c("datlist") ) ){
	    is_dfr <- TRUE 
	    datlist0 <- datlist 
        datlist <- base::list( 1 )
		datlist[[1]] <- datlist0
		base::class(datlist) <- "datlist"
	}
					
    #*** create weights if needed					
    PP <- base::length(datlist)
    if ( base::is.null(weights) ){
	    N <- base::nrow(datlist[[1]])
        weights <- base::rep(1,N)
    }

	weights0 <- weights
	
	orig_var0 <- orig_var
	trafo_var0 <- trafo_var
	N0 <- base::length(orig_var0)
	for (nn in 1:N0){
		orig_var <- orig_var0[nn]
		trafo_var <- trafo_var0[nn]
		#---- compute means and standard deviations
		res <- base::lapply( datlist , FUN = function(dd){
			# dd <- datlist[[1]]
			if ( base::is.character(weights0) ){
				weights <- dd[ , weights0 ]
			}
			m1 <- stats::weighted.mean( dd[,orig_var] , w = weights )
			sd1 <- TAM::weighted_sd( x=dd[,orig_var] , w=weights )
			base::c(m1,sd1)
				} )
		#---- compute averaged mean and SD
		res <- base::matrix( base::unlist(res) ,ncol=2 , byrow=TRUE ) 
		a1 <- base::colMeans(res)
		#---- create derived variable
		for (pp in 1:PP){
			dd <- datlist[[pp]]
			dd[,trafo_var] <- M + SD * ( dd[,orig_var] - a1[1] ) / a1[2]
			if ( ! is.null(digits) ){
				dd[,trafo_var] <- base::round( dd[,trafo_var] , digits )
			}
			datlist[[pp]] <- dd
		}
	}	
	#---- output
	if ( is_dfr ){
		datlist <- datlist[[1]]
	}			
	if ( is_iL ){
        datlist0$imputations <- datlist
		datlist <- datlist0
	}				 
	if ( is_dl ){
        datlist <- datlist_create(datlist)
	}				 
	if ( is_ndl ){
		datlist <- datlist2nested.datlist(datlist=datlist, Nimp=Nimp)	
	}
	if ( is_NIL ){
		datlist <- NestedImputationList(datlist)
	}								
    base::return(datlist)
}
######################################################################