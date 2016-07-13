
mice_imputation_include_cluster_effect <- function( x , y , ry , type )
{
	#***********************
	# include cluster effect: group mean (eliminating the subject under study)
	if ( base::sum(type==-2) > 0 ){
		x1 <- base::cbind( y , x[ , base::which( type==-2) ] )
		type1 <- base::c( 1,-2)
		ximp <- mice.impute.2l.groupmean.elim( y=y , ry=ry , x = x1 , type=type1 )
		x <- base::as.matrix( base::cbind( x , ximp ) )
		newname <- "y_aggr"
		base::colnames(x)[ base::ncol(x) ] <- newname
		x10 <- x0 <- x
		type <- base::c( type , 1 )
		base::names(type)[ base::length(type) ] <- newname
		# check whether x10 and x0 are really needed here
	}							
	res <- base::list( x = x , type = type )
	base::return(res)
}