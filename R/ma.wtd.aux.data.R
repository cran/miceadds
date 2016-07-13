
########################################################
# auxiliary function
ma.wtd.aux.data <- function(data , weights , vars = NULL ){
    #*****
	
	is_dfr <- TRUE		# default is class data frame
	if ( base::is.vector(data) ){
		data <- base::data.frame( "Var" = data)
		is_dfr <- TRUE
	}
			
	#---- mids or mids.1chain
	if ( base::class(data) %in% base::c("mids","mids.1chain","mids.nmi") ){
		data <- mids2datlist( data )
	}		
	
	#----- NestedImputationList
	if ( base::class(data) == "NestedImputationList" ){
		is_dfr <- FALSE
		data <- data$imputations
		class(data) <- "nested.datlist"
	}						
		
	#--------------------
	# conversion in case of a nested datalist	
	if ( base::class(data) == "nested.datlist" ){
		is_dfr <- FALSE
		data <- nesteddatlist2datlist(data)
	}											
						
	#---- imputationList
	if ( base::class(data) == "imputationList" ){
		data <- data$imputations
		base::class(data) <- "datlist"
	}
	
	#--------------------
	# conversion in case of a datlist
	if ( base::class(data) == "datlist" ){
		is_dfr <- FALSE
		if ( ! base::is.null(vars) ){
			M <- base::length(data)
			for (ii in 1:M){
			    dat0 <- data[[ii]]
				data[[ii]] <- dat0[ , vars, drop=FALSE ]		
			}
		}
	}

	#--------------------
	# conversion in case of class BIFIEdata
	if ( base::class(data) == "BIFIEdata" ){
	    base::requireNamespace("BIFIEsurvey")
		if ( base::is.null(vars) ){
			vars <- data$variables
		}
		#*** use weights
		if ( base::is.null(weights) ){
			weights <- data$wgt
		}						
		if ( data$cdata){
			data <- BIFIEsurvey::BIFIE.BIFIEcdata2BIFIEdata(bifieobj=data, 
							varnames = vars )
		}
		data <- BIFIEsurvey::BIFIE.BIFIEdata2datalist(bifieobj=data, varnames = vars)			
		data <- datlist_create( data )
		M <- base::length(data)
		for (ii in 1:M){
			data[[ii]][ , "one"] <- NULL
		}
		base::attr(data,"nvars") <- base::ncol(data[[ii]])
		is_dfr <- FALSE		
	}
						
	#-------------------					
	# conversion in case of a data frame
	if ( is_dfr ){
	    data0 <- data
		data0 <- base::as.matrix(data0)
		if ( ! base::is.null(vars) ){
			data0 <- data0[ , vars , drop=FALSE ]
		}			
		data <- base::list(1)
		data[[1]] <- data0
	}
	#-------------------
	# creation of weights (if needed)
	if ( base::is.null(weights) ){
		weights <- base::rep(1 , base::nrow(data[[1]] ) )
	}
	res <- base::list( data = data , weights = weights )
	base::return(res)
}
#########################################################	



