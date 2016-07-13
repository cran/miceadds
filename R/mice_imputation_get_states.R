
mice_imputation_get_states <- function( pos= base::parent.frame(n=1) ){
	if ( base::is.null(pos) ){
		pos <- base::parent.frame()
	}
    vname <- base::get("vname", pos = pos ) 
    newstate <- base::get( "newstate" , pos = pos )  
	res <- base::list("vname" = vname , "newstate" = newstate)
	base::return(res)
}
