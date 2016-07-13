

GroupSum <- function( data , group , weights=NULL , extend=FALSE){
	groups <- base::sort( base::unique( group ) )
	index.group <- base::match( group , groups )
	if ( base::is.null(weights) ){
	    data1 <- base::rowsum( data , index.group , na.rm=TRUE)		
	} else {
	    data1 <- base::rowsum( data*weights , index.group , na.rm=TRUE)		
	}
	base::colnames(data1) <- base::colnames(data)
	data1 <- base::data.frame( "group" = groups , data1 )
	if (extend){
	   data1 <- data1[ index.group , ]
	   base::rownames(data1) <- NULL
	}	
	base::return(data1)
}
