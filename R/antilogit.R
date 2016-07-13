
antilogit <- function(p){
	base::return( 1 / ( 1 + base::exp(-p) ) )
}