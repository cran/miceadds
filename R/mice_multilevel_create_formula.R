

mice_multilevel_create_formula <- function( 
		variables , include_intercept ){
	#---
	intercept_code <- if ( include_intercept ){ 1 } else { 0 }
	fml <- paste0( base::c( intercept_code , variables ), 
			collapse="+" )
	base::return(fml)
}