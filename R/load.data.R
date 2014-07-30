

#########################################################################
# miceadds::load.data: load Rdata objects coveniently
load.data <- function( filename , type="Rdata" , path=getwd() , ...){
	#*** the resulting object is dat4!	
	dir <- path
	file <- filename
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
		dat4 <- foreign::read.spss( file.path(dir,file) , ... )
				}				
	return(dat4)
			}
#########################################################################			