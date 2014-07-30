

#########################################################################
# miceadds: saving data
save.data <- function( data , filename , type="Rdata" , path=getwd() , ...){
	#*** the resulting object is dat4!	
	dir <- path
	file <- filename	
    #*** Rdata objects	
	if (type == "Rdata" ){
		save( data , file= file.path( dir , file ) )
				}
    #*** csv2 objects
	if (type == "csv2" ){
		write.csv2( data , file.path(dir,file) , ... )
				}
    #*** csv objects
	if (type == "csv" ){
		write.csv( data , file.path(dir,file) , ... )
				}
    #*** table objects
	if (type == "table" ){
		write.table( data , file.path(dir,file) , ... )
				}
    #*** sav objects (SPSS objects)
	if (type == "sav" ){
	    dir2 <- getwd()
	    setwd( path)
		miceadds::write.pspp( data ,  datafile= file , ... )
		setwd(dir2)
				}				
			}
#########################################################################			