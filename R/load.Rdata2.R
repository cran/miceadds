
#**********************************************
# load Rdata objects			
load.Rdata2 <- function( filename , path=getwd() ){
    d1 <- load( file=file.path(path,filename) )
    objname <- "ma01"
	eval(parse(text = paste(objname, "<- ", d1)))	
    eval(parse(text= paste0( "return( " , objname , ")" ) ) )			
}