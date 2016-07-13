

##################################################
# linear model for clustered data
glm.cluster <- function( data , formula , cluster , ... ){
	mod <- stats::glm( data=data , formula=formula ,  ... )
	if ( base::length(cluster) > 1 ){
		v1 <- cluster 
	} else {
		v1 <- data[,cluster]
	}	
	dfr <- base::data.frame( cluster = v1 ) 
	vcov2 <- multiwayvcov::cluster.vcov( model = mod , cluster = dfr)	
	res <- base::list( "glm_res" = mod , "vcov" = vcov2 )
	base::class(res) <- "glm.cluster"
	base::return(res)
}
###################################################			
coef.glm.cluster <- function( object , ... ){
	coef( object$glm_res)
}
####################################################			
vcov.glm.cluster <- function( object , ... ){
	object$vcov
}
####################################################
summary.glm.cluster <- function( object , ... ){
	smod <- summary( object$glm_res )
	csmod <- smod$coefficients
	csmod[,"Std. Error"] <- base::sqrt( base::diag( vcov(object) ))
	csmod[,"z value"] <-  csmod[,"Estimate"] / csmod[,"Std. Error"]
	csmod[,"Pr(>|z|)"] <- stats::pnorm( - base::abs( csmod[,"z value"] ) )*2
	# R2 <- smod$r.squared
	# cat("R^2 =" , round(R2 , 5),"\n\n" )
	base::print(csmod)
	base::invisible(csmod)
}
#######################################################			