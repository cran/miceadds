GroupMean  <- function( data , group , weights=NULL , extend=FALSE){
	groups <- base::sort( base::unique( group ) )
	index.group <- base::match( group , groups )
	if ( base::is.null(weights) ){
		Ngroup <- base::rowsum( 1 - base::is.na(data) , index.group ) 
	    data1 <- base::rowsum( data , index.group , na.rm=TRUE)		
	} else {
		Ngroup <- base::rowsum( weights*(1 - base::is.na(data)) , index.group )
	    data1 <- base::rowsum( data*weights , index.group , na.rm=TRUE)		
	}
	base::colnames(data1) <- base::colnames(data)
	data1 <- data1 / Ngroup
	data1 <- base::data.frame( "group" = groups , data1 )
	if (extend){
	   data1 <- data1[ index.group , ]
	   base::rownames(data1) <- NULL
	}
	base::return(data1)
}
