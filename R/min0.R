

min0 <- function(x, na.rm=TRUE){
	sd1 <- stats0( x = x , FUN = base::min , na.rm=na.rm )
	base::return(sd1)
}