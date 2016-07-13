datalist2mids <- function( dat.list , progress=TRUE ){
    
	CALL <- base::match.call()
	
	#*** preliminary check whether dat.list of type
	#    imputationList
	# if ( class(dat.list) %in% "imputationList" ){
	if ( base::inherits(dat.list , "imputationList" ) ){
		dat.list <- dat.list$imputations
	}
	#***
	M <- base::length(dat.list)
	if (M == 1){
		dat0 <- dat.list[[1]]
		dat0[ ,"__dummy"] <- 1
		dat0[1,"__dummy"] <- NA
		dat.list <- base::list( dat0 , dat0 )
		M <- 2	
	}
    datl1 <- dat.list[[1]]
	datl1 <- base::as.data.frame(datl1)
	datl2 <- base::matrix( NA , nrow= base::nrow(datl1) , ncol=base::ncol(datl1) )
	base::colnames(datl2) <- base::colnames(datl1)
	VV <- base::ncol(datl1)
	for ( vv in 1:VV){
		datl2[ , vv] <- base::as.vector( datl1[,vv] )
	}
	datl1 <- base::as.data.frame(datl2)
    cM <- base::colMeans( is.na( datl1)  )
    # extract cells with original missing entries
    impvars <- base::which( cM == 0 )
	
    r1 <- 1 * base::is.na( datl1 )
	#*******
	# more than one dataset  => typical imputation
	if (M > 1){
		if (progress){
			base::cat("Analyze missing pattern\n-")
			utils::flush.console()
		}
		for (ii in 2:M){
			# ii <- 2
			datl2 <- dat.list[[ii]]
			r1[ , impvars] <- r1[,impvars] + 1*( datl2[,impvars] != datl1[,impvars ] )
			datl1 <- datl2
			if (progress){ 
				base::cat("-")
				utils::flush.console() 
			}
		}
	}
	#*******
	# only one dataset	
#	if (M==1){
#	    v11 <- datl1[1,1]
#		datl1[1,1] <- NA
#			}
				
    if (progress){ 
		base::cat("\n")
		utils::flush.console() 
	}        
    r1[ r1>0 ] <- 1
    dat0 <- datl1
    dat0[ r1 == 1 ] <- NA
    imp0 <- mice::mice( dat0 , maxit=0 , allow.na=TRUE)
    iM <- imp0$method
    elimvars <- base::names(cM)[ cM > 0 ]
    pM <- imp0$predictorMatrix
    if ( base::length(elimvars) > 0 ){
        iM[ elimvars ] <- ""
        pM[  elimvars , ] <- 0
        pM[ , elimvars ] <- 0
    }
    imp1 <- mice::mice( dat0 , maxit=0 , imputationMethod=iM , predictorMatrix=pM , 
                    m=M , allow.na=TRUE)

    # fill in missing in mids object
    IMP <- imp1$imp
	if (progress){
	    base::cat("Create mids object\n")
		utils::flush.console()
	}
    for (ii in 1:M){
        # ii <- 1
        dat.ii <- dat.list[[ii]]
        #******
        for ( vv1 in base::seq( 1 , base::length(impvars) ) ){
            vv <- names(impvars)[vv1]
            l1 <- dat.ii[r1[,vv]==1,vv]
            if ( base::length(l1) > 0 ){ 
				IMP[[vv]][ii] <-  l1 
			}
        }
		#********
        if (progress){ base::cat(".") ; utils::flush.console() }
	}	
    if (progress){ 
		base::cat("\n") 
	}
    imp1$imp <- IMP
    iM[ imp1$nmis == 0 ] <- ""
    iM[ imp1$nmis > 0 ] <- "imputed"
    imp1$method <- iM
	imp1$visitSequence <- -99 + 0*imp1$visitSequence
	# predictor matrix
	imp1$predictorMatrix <- -99 + 0*imp1$predictorMatrix	
	imp1$predictorMatrix[ iM == "" , ] <- 0
	imp1$call <- CALL	
    base::return(imp1)
}
###############################################################

datlist2mids <- datalist2mids