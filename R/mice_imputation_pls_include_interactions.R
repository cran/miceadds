
mice_imputation_pls_include_interactions <- function(pls.interactions , 
	pls.print.progress, x , y , ry , type , min.int.cor , pls.maxcols)
{
    ##############################################
	
	use_interactions <- ! ( base::is.null(pls.interactions) )
    #------------ no interactions
    if ( ! use_interactions ){
        if( pls.print.progress ){   
            cat("\n" , paste("Created no Interactions" , 
					     substring( Sys.time() ,1) ) , "\n")
			utils::flush.console() 
        }
    }

    #------------ some interactions
	if (use_interactions){					
		use.int <- base::intersect( base::colnames(x) , pls.interactions  )
        N1 <- base::length(use.int)
		# standardize x
		cx <- base::colMeans( x )
		xs <- x - base::outer( base::rep(1, base::nrow(x)) , cx )
        if (N1 > 0){
            # search for interaction variables in predictorMatrix x?
            ind.int <- base::sort( base::which(  base::colnames(x) %in% use.int ) )
            dfr0 <- NULL
            if( pls.print.progress ){ 
				cat("\nCreate Interactions")
                cat("\n" , "Minimal Absolute Correlation for Interactions of ")
				cat("min.int.cor =" , min.int.cor , "\n\n")
			}								
			N1t <- 0 ; N2t <- 0
			# which interactions should not be created?
			dont.int <- base::which( base::colnames(x) %in% (base::names(type)[type==6]))				
			# create design matrix
			cols <- base::setdiff( base::seq( 1, base::ncol(x) ) , dont.int )
			dfr <- base::cbind( base::rep( ind.int, each= base::length(cols) ), 
								base::rep(cols , base::length(ind.int) ) )
			dfr <- dfr[ dfr[ , 1 ] != dfr[,2] , ]
			ind <- base::intersect( base::which( dfr[ , 1] %in% ind.int)  , 
							base::which(dfr[ , 2] %in% ind.int  ) )
			dfr1 <- dfr[ ind , ]
			dfr <- base::rbind( dfr[ base::setdiff( base::seq(1, base::nrow(dfr)),ind) , ] , 
									dfr1[ dfr1[,1]< dfr1[,2] , ])
			dfr <- dfr[ base::order( dfr[,1] ) , ]
									
			# create interactions
			res <- mice_imputation_create_interactions( 
						y_=y[ry] , 
						xobs_ = base::as.matrix(x[ry,]) , 
					    xall_ = base::as.matrix(x) ,
						index_int_ = base::as.matrix(dfr), 
					    min_int_cor_= min.int.cor , 
						maxcols_= base::min( base::nrow(dfr),pls.maxcols) )					
												
			r1 <- res$allcorrs
			r1[ base::is.na( r1[,1] ) , 1] <- 0
			res$allcorrs <- r1
					
			# total number of interactions
			N1t <- base::nrow(res$index_int)
			# retained number of interactions
			N2t <- base::ncol( res$xint )
			hx <- res$xint
			index_int2 <- res$index_int				
			index_int2 <- index_int2[ res$allcorrs[,2] == 1 , , drop=FALSE]
			if ( N2t > 0 ){
				base::colnames(hx) <- base::paste0( "X" , index_int2[,1] , "." , index_int2[,2] )
				x <- base::cbind( x , hx )
			}
			if (N2t == 0 ){
				res$allcorrs <- 0 * base::is.na(res$allcorrs )
			}
			N1t <- base::rowsum( 1+0*res$allcorrs[,1] , res$index_int[,1] )
			N1h <- base::rowsum( res$allcorrs[,2] , res$index_int[,1] )
			if (pls.print.progress){
			    cat(" ")   
				cat(paste( seq( 1 , nrow(N1t)) , 
				colnames(x[,ind.int]) ,  
							"Created" , N1t[,1] , 
							"Interactions | Kept", N1h[,1] , "Interactions " , 
							"\n") )				
			}
																		
			#***************************
            if( pls.print.progress ){
				cat("\n")
                cat(paste("Created" , sum(N1t[,1]) , "Interactions in Total | " , 
							substring( Sys.time() ,1) ) , "\n")
                utils::flush.console() 
                cat("Interactions with " , paste(use.int,collapse=" ") , "\n" , sep="")
                cat("Kept " , N2t , " Interactions in Total \n" , sep="")
                cat("  Minimal Absolute Correlation for Interactions of min.int.cor =" , 
							min.int.cor , "\n")
                utils::flush.console() 
			}              
        }
    }
	#--- output
	res <- base::list( x = x , xs = xs )
	base::return(res)				   
}