
########################################################
# Call to Rcpp function
ma.scale2 <- function (x , missings =FALSE ){ 
    x_ <- base::as.matrix(x)
	if ( ! missings ){
		res <- base::.Call("scale2_C", x_, PACKAGE = "miceadds")
	} else {
		res <- base::.Call("scale2_NA_C", x_, PACKAGE = "miceadds")
	}
	base::colnames(res) <- base::colnames(x)
	base::return(res)
}
##########################################################