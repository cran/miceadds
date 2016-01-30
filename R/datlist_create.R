datlist_create <- function(datasets){
	if ( class(datasets) %in% c("mids","mids.1chain") ){
		datasets <- mids2datlist(datasets)
						}
	if ( class(datasets) == "imputationList" ){
		datasets <- datasets$imputations								
						}
    class(datasets) <- "datlist"
    return(datasets)
                    }

nested.datlist_create <- function(datasets){
	if ( class(datasets) %in% c("mids.nmi") ){
		datasets <- mids2datlist(datasets)
						}
	if ( class(datasets) == "NestedImputationList" ){
		datasets <- datasets$imputations								
						}
	v1 <- c("between" = length(datasets) , "within" = length(datasets[[1]]) )				
    class(datasets) <- "nested.datlist"
	attr(datasets,"Nimp") <- v1 			
    return(datasets)
                    }
					