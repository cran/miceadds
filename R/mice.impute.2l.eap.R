
mice.impute.2l.eap <- function (y, ry, x,  eap , ...){  
    # define imputation method
    vname <- get("vname", pos = parent.frame()) # get variable name
    newstate <- get( "newstate" , pos = parent.frame() )  
    M.scale <- eap[[ vname ]][[ "M" ]]
    SE.scale <- eap[[ vname ]][[ "SE" ]]
    ximp <- rnorm( length(M.scale) , mean= M.scale , sd = SE.scale )
	flush.console()
    # return imputed values	
    return(ximp)	
}
