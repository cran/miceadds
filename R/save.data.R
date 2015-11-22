

#########################################################################
# miceadds: saving data
save.data <- function( data , filename , type="Rdata" , path=getwd() , 
          row.names=FALSE , na = NULL , suffix = NULL , suffix_space = "__" , ...){
	#*** the resulting object is dat4!	
	dir <- path
	file <- filename	
	if ( ! is.null(suffix ) ){
		file <- paste0( file , suffix_space , suffix )
							}
	
	#*** missing handling
	if ( is.null(na) ){
		na <- switch( type , 
					"csv" = "" , 
					"csv2" = "" ,
					"table" = "." )
						}
	
	type2 <- type
	if ( type == "csv2" ){ 
			type2 <- "csv" 
					}
	if ( type == "table" ){ 
			type2 <- "dat" 
				}
	i1 <- grep( type2 , file )
	if ( length(i1) == 0 ){
		file <- paste0( file , "." , type2 )
						}
	
    #*** Rdata objects	
	if (type == "Rdata" ){
		save( data , file= file.path( dir , file ) )
				}
    #*** csv2 objects
	if (type == "csv2" ){
		write.csv2( data , file.path(dir,file) , row.names=row.names , na=na ,... )
				}
    #*** csv objects
	if (type == "csv" ){
		write.csv( data , file.path(dir,file) , row.names=row.names , na=na,  ... )
				}
    #*** table objects
	if (type == "table" ){
		write.table( data , file.path(dir,file) , na=na , ... )
				}
    #*** sav objects (SPSS objects)
	if (type == "sav" ){
#	    dir2 <- getwd()
#	    setwd( path)
#		miceadds::write.pspp( data ,  datafile= file , ... )
#		setwd(dir2)
#		data <- sjmisc::set_var_labels( data, lab=attr(data, "variable.labels") )
		data <- sjmisc::set_label( data, lab=attr(data, "variable.labels") )
		sjmisc::write_spss( data , file.path( dir , file ) )		
				}				
			}
#########################################################################			