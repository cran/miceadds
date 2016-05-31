fast.groupmean <- function( data , group , weights=NULL , extend=FALSE){
	groups <- sort( unique( group ) )
	index.group <- match( group , groups )
	if ( is.null(weights) ){
		Ngroup <- rowsum( 1-is.na(data) , index.group ) 
	    data1 <- rowsum( data , index.group , na.rm=TRUE)		
						} else {
		Ngroup <- rowsum( weights*(1-is.na(data)) , index.group )
	    data1 <- rowsum( data*weights , index.group , na.rm=TRUE)		
							}
	colnames(data1) <- colnames(data)
	data1 <- data1 / Ngroup
	data1 <- data.frame( "group" = groups , data1 )
	if (extend){
	   data1 <- data1[ index.group , ]
	   rownames(data1) <- NULL
				}
	return(data1)
            }
GroupMean <- fast.groupmean			
#.....
# ARb 2013-10-25
# extend this function to include weights
# and calculation of standard deviation and skewness
#.....
fast.groupsum <- function( data , group , weights=NULL , extend=FALSE){
	groups <- sort( unique( group ) )
	index.group <- match( group , groups )
	if ( is.null(weights) ){
#		Ngroup <- rowsum( 1-is.na(data) , index.group ) 
	    data1 <- rowsum( data , index.group , na.rm=TRUE)		
						} else {
#		Ngroup <- rowsum( weights*(1-is.na(data)) , index.group )
	    data1 <- rowsum( data*weights , index.group , na.rm=TRUE)		
							}
	colnames(data1) <- colnames(data)
#	data1 <- data1 / Ngroup
	data1 <- data.frame( "group" = groups , data1 )
	if (extend){
	   data1 <- data1[ index.group , ]
	   rownames(data1) <- NULL
				}	
	return(data1)
            }
GroupSum <- fast.groupsum			

#############################################################
# group SD
GroupSD <- function( data , group , weights=NULL , extend=FALSE){	
	m1 <- GroupSum( data=data^2 , group = group , weights=weights , extend=extend )
	m2 <- GroupSum( data=1+0*data , group = group , weights=weights , extend=extend )
	m3 <- GroupMean( data=data , group = group , weights=weights , extend=extend )
	res <- ( m1[,-1] - m2[,-1] * m3[,-1]^2 ) / ( m2[,-1] - 1 + 1E-10)
	res <- sqrt( res )
	res <- data.frame( m1[,1] , res)
#	res[,1] <- m1[,1]
	return(res)
	
		}