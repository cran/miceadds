
######################################
# transformation of correlation
cor2fisher <- function(r){
	f1 <- 1/2* base::log( ( 1 + r) / ( 1 - r ) )
	base::return(f1)
}
########################################		
  