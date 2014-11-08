

#########################################################################
# miceadds::load.data: load Rdata objects coveniently
load.data <- function( filename , type="Rdata" , path=getwd() , 
				spss.default=TRUE , ...){
	#*** the resulting object is dat4!	
	dir <- path
	file <- filename
	
	files <- list.files( dir , filename )	
	type1 <- type
	if ( type=="table" ){
		files <- grep.vec( c("dat","txt") , files , "OR" )$x
						}		
	
	files <- grep( gsub("csv2","csv" , type1) , files , value=TRUE)
	file <- max(files)
	cat( paste0( "*** Load " , file , "\n"))

    #*** Rdata objects	
	if (type == "Rdata" ){
		dat4 <- load.Rdata2( filename=file , path=dir )
				}
    #*** csv2 objects
	if (type == "csv2" ){
		dat4 <- read.csv2( file.path(dir,file) , ... )
				}
    #*** csv objects
	if (type == "csv" ){
		dat4 <- read.csv( file.path(dir,file) , ... )
				}
    #*** table objects
	if (type == "table" ){
		dat4 <- read.table( file.path(dir,file) , ... )
						}
    #*** sav objects (SPSS objects)
	if (type == "sav" ){
	  if ( ! spss.default){
			dat4 <- foreign::read.spss( file.path(dir,file) , ... )
							}
	  if (  spss.default){
			dat4 <- foreign::read.spss( file.path(dir,file) , 
				to.data.frame=TRUE , use.value.labels=FALSE , ... )
							}			
				}				
	return(dat4)
			}
#########################################################################			