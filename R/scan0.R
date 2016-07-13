
####################################################
scan.vector <- function( vec ){ 
    vec <- base::unlist( base::strsplit( vec , split="\n" , fixed=TRUE) )
    vec <- base::unlist( base::strsplit( vec , split=" " , fixed=TRUE) )
    vec <- vec[ vec != "" ] 
    base::return(vec)
}
scan.vec <- scan.vector
####################################################				
# scan function with default what = "character"
scan0 <- function( file="" , ...){
	base::scan( file=file , what="character" , ...)			
}
#########################################################		