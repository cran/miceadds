datalist2mids <-
function( dat.list , progress=TRUE ){
    M <- length(dat.list)
    datl1 <- dat.list[[1]]
    cM <- colMeans( is.na( datl1)  )
    # extract cells with original missing entries
    impvars <- which( cM == 0 )
    r1 <- 1*is.na( datl1 )
    for (ii in 2:M){
        # ii <- 2
        datl2 <- dat.list[[ii]]
        r1[ , impvars] <- r1[,impvars] + 1*( datl2[,impvars] != datl1[,impvars ] )
        datl1 <- datl2
        if (progress){ cat("-") ; flush.console() }
                }
			
    if (progress){ cat("\n") ; flush.console() }                
    r1[ r1>0 ] <- 1
    dat0 <- datl1
    dat0[ r1 == 1 ] <- NA
    imp0 <- mice( dat0 , maxit=0 , allow.na=TRUE)
    iM <- imp0$method
    elimvars <- names(cM)[ cM > 0 ]
    pM <- imp0$predictorMatrix
    if ( length(elimvars) > 0 ){
        iM[ elimvars ] <- ""
        pM[  elimvars , ] <- 0
        pM[ , elimvars ] <- 0
                }
    imp1 <- mice( dat0 , maxit=0 , imputationMethod=iM , predictorMatrix=pM , 
                    m=M , allow.na=TRUE)
    # fill in missing in mids object
    IMP <- imp1$imp
	
    for (ii in 1:M){
        # ii <- 1
        dat.ii <- dat.list[[ii]]
        for ( vv1 in seq( 1 , length(impvars) ) ){
            vv <- names(impvars)[vv1]
            l1 <- dat.ii[r1[,vv]==1,vv]
            if ( length(l1) > 0 ){ 
				IMP[[vv]][ii] <-  l1 
					}
                        }
            if (progress){ cat(".") ; flush.console() }
                }
    if (progress){ cat("\n") }
    imp1$imp <- IMP
    iM[ iM=="" ] <- "not_imputed"
    iM[ iM!=""] <- "imputed"
    imp1$method <- iM
    return(imp1)
        }
