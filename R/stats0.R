


stats0 <- function(x, FUN , na.rm=TRUE,...){
	if ( ! base::is.vector(x) ){
		sd1 <- base::apply( x , 2 , FUN=FUN , na.rm=na.rm, ...)
	} else {
		sd1 <- FUN( x=x, na.rm=na.rm, ...)
	}
	base::return(sd1)
}