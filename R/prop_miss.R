

prop_miss <- function(x){
	x <- base::is.na(x)
	sd1 <- stats0( x = x , FUN = base::mean )
	base::return(sd1)
}