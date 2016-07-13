
#######################################################
# calls a function and suppresses warnings if requested

mice_multilevel_doCall_suppressWarnings <- function( what , args , warnings = TRUE){
	if (warnings){
		res <- base::do.call( what = what , args = args)
	} else {
		base::suppressWarnings(
			res <- base::do.call( what = what , args = args)
			)
	}
	base::return(res)
}	