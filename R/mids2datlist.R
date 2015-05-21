mids2datlist <- function( midsobj ){
	if ( class(midsobj) == "mids.1chain" ){
		midsobj <- midsobj$midsobj		
			}
	#*****************************		
	# object of class mids
	if ( class(midsobj) %in% c("mids" ) ){		
		m <- midsobj$m
		datlist <- as.list( 1:m )
		for (ii in 1:m){  
				datlist[[ii]] <- mice::complete( midsobj , ii ) 
						}
					}	
	#******************************
    # object of class mids.nmi		
	if ( class(midsobj) %in% c("mids.nmi" ) ){		
		Nimp <- midsobj$Nimp
		datlist <- as.list(1:Nimp["between"])
		dat1 <- as.list(1:Nimp["within"])
		# imp <- midsobj$imp
		for (bb in 1:Nimp["between"]){
			# bb <- 1
			datlist[[bb]] <- dat1
			for (ww in 1:Nimp["within"] ){
				# ww <- 1
				datlist[[bb]][[ww]] <- complete.mids.nmi( midsobj , action= c(bb,ww) )	
										}
								}
			}		
    return(datlist)
        }
