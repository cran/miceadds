mids2datlist <-
function( midsobj ){
	if ( class(midsobj) == "mids.1chain" ){
		midsobj <- midsobj$midsobj		
			}
    m <- midsobj$m
    datlist <- as.list( 1:m )
    for (ii in 1:m){  
			datlist[[ii]] <- complete( midsobj , ii ) 
					}
    return(datlist)
        }
