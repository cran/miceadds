
################################################################
pool.nmi.scalar.helper <- function( qhat , u , NV , NB , NW ){

		# Input: qhat, u, NV, NB, NW 
		# formulas follow Reiter & Raghanuthan: 
		#  multiple adaptations of multiple imputation
		qbar <- apply( qhat , 3 , mean )
		ubar <- apply( u , c(3,4) , mean )
        qbar_l <- apply( qhat , c(1,3) , mean )				
		# calculate variance statistics
		Wm <- matrix(NA,nrow=NV,ncol=NV)
		colnames(Wm) <- rownames(Wm) <- dimnames(qhat)[[3]]
		Bm <- Wm
		for (vv in 1:NV){
			# vv <- 1
			qbar_l.vv <- qbar_l[,vv]
			qhat.vv <- qhat[ ,,vv]
			Wm[vv,vv] <-  sum( ( qhat.vv - qbar_l.vv )^2 ) / NB / (NW - 1)
			Bm[vv,vv] <- sum( (qbar_l.vv - qbar[vv])^2 ) / (NB - 1)
						}
		for (vv1 in 1:(NV-1)){
		for (vv2 in (vv1+1):(NV)){
			# vv <- 1
			qbar_l.vv1 <- qbar_l[,vv1]
			qhat.vv1 <- qhat[ ,,vv1]
			qbar_l.vv2 <- qbar_l[,vv2]
			qhat.vv2 <- qhat[ ,,vv2]
			
			Wm[vv2,vv1] <- Wm[vv1,vv2] <-  sum( ( qhat.vv1 - qbar_l.vv1 ) *
									( qhat.vv2 - qbar_l.vv2 ) ) / NB / (NW - 1)
			Bm[vv2,vv1] <- Bm[vv1,vv2] <- sum( (qbar_l.vv1 - qbar[vv1]) *
									(qbar_l.vv2 - qbar[vv2]) ) / (NB - 1)
						}
					}			
						
		Um <- ubar
		Tm <- (1+1/NB)*Bm + (1-1/NW)*Wm + Um
		df <- ( (1+1/NB)*Bm )^2 / (NB-1) / Tm^2 +
				( (1-1/NW)*Wm )^2 / NB / (NW-1) / Tm^2
		df <- 1 / diag( df )
		# fraction of missing information
		lambda <- diag( ( Bm + (1-1/NW)*Wm ) / ( Um + Bm + (1-1/NW)*Wm ) )
		lambda_Within <- diag( Wm / ( Um + Wm ) )
		lambda_Between <- lambda - lambda_Within
			
		#*******
		# output
		fit <- list( "qhat" = qhat , "u"=u , 		
						"qbar" = qbar , "ubar" = ubar , 
						Wm=Wm , Bm=Bm , Tm=Tm , df=df , 
						lambda=lambda , lambda_Between = lambda_Between ,
						lambda_Within=lambda_Within , 
						"NV" = NV )						
		return(fit)
		}