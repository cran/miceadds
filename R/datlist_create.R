#################################################
# create datlist
datlist_create <- function(datasets){
	CALL <- match.call()
	# if ( class(datasets) %in% c("mids","mids.1chain") ){
	if ( base::inherits( datasets , "mids")  |  
			base::inherits( datasets , "mids.1chain")	){ 	
				datasets <- mids2datlist(datasets)
	}
	# if ( class(datasets) %in% "imputationList" ){
	if ( base::inherits(datasets , "imputationList" ) ) {
		datasets <- datasets$imputations								
	}
    base::class(datasets) <- "datlist"
	base::attr(datasets,"Nimp") <- base::length(datasets)
	base::attr(datasets,"call") <- CALL
	base::attr(datasets,"nobs") <- base::nrow(datasets[[1]])	
	base::attr(datasets,"nobs_datasets") <- 
			base::lapply( datasets , FUN = function(dd){ base::nrow(dd) } )	
	attr(datasets,"nobs") <- base::round( base::mean( base::unlist( 
				base::attr(datasets,"nobs_datasets") ) ) , 2 )			
	base::attr(datasets,"nvars") <- base::ncol(datasets[[1]])	
	base::attr(datasets,"variables") <- base::colnames(datasets[[1]])		
    base::return(datasets)
}
#**************** print method ***********************			
print.datlist <- function(x,...){
	cat("Object of class 'datlist'\nCall: ")
	print( attr(x,"call"))  
	cat("MI data with", attr(x,"Nimp") ,"datasets\n")
	v1 <- paste0( attr(x,"nobs") , " cases and " ,
	attr(x,"nvars") , " variables \n" )
	cat(v1)
}
########################################################
					
