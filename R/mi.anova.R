mi.anova <-
function( mi.res , formula ){
    # INPUT:
    # mi.res  ... mids object (from mice imputation function)
    # formula ... formula for ANOVA model (variable names must be in colnames(mi.list[[1]]), ...
    # converting MICE object to mi.list
    mi.list <- mi.res
    if( class(mi.list) == "mids.1chain" ){	
		mi.list <- mi.list$midsobj
			}
    if( class(mi.list) == "mids" ){
            # number of imputations 
            m <- mi.list$m
            # list of completed datasets
            h1 <- list( rep("", m ))
            for (ii in 1:m){ 
				h1[[ii]] <- as.data.frame( complete( mi.list , ii ) ) 
							}										
            mi.list <- h1
            }
    # converting mi.norm objects
    if (class(mi.res) == "mi.norm" ){ mi.list <- mi.list$imp.data }
    anova.imp0 <- lapply( mi.list , FUN = function(dat){ 
                    lm( formula, data = dat ) } )                 
    anova.imp <- lapply( anova.imp0 , FUN = function( obj){ summary(aov(obj)) } )
    # number of F statistics to be evaluated
    FF <- nrow( anova.imp[[1]][[1]] ) - 1
    anova.imp.inf <- t( sapply( 1:FF , FUN = function(ff){
            micombine.F( sapply( 1:( length(anova.imp) ) , FUN = function(ii){ 
						anova.imp[[ii]][[1]]$'F value'[ff] } ) ,
                            df1 = anova.imp[[1]][[1]]$Df[ff] , display = FALSE )
				} ) )
		
    # ANOVA results
    res <- anova.imp.inf[ , c(3,4,1,2) ]
    res <- matrix( res , ncol = 4 )
    res[,3] <- round( res[,3] , 4 )
    res[,4] <- round( res[,4] , 6 )
    rownames(res) <- rownames( anova.imp[[1]][[1]] )[1:FF]
    res <- data.frame(res)
    # compute eta squared and partial eta squared coefficients
    SS <- rowMeans( matrix( unlist( lapply( anova.imp , FUN = function(ll){ 
				ll[[1]][,2] } ) ) , ncol = length(mi.list) )  )
    # calculate (average) R squared
    r.squared <-  sum(SS[ - (FF+1) ]) / sum(SS) 
    res$eta2 <- round( SS[ - ( FF + 1 ) ] / sum( SS ) , 6 )
    res$partial.eta2 <- round( SS[ - (FF+1) ] / ( SS[ - (FF+1) ] + SS[ FF + 1 ] ) , 6 )
    colnames(res)[3:4] <- colnames( anova.imp[[1]][[1]] )[ c(4,5) ]
    colnames(res)[1:2] <- c("df1" , "df2")
    c1 <- colnames(res)
    res <- rbind( res , res[1,] )
    rownames( res)[ nrow(res) ]  <- "Residual"
    res[ nrow(res) , ] <- NA
    res <- data.frame( "SSQ" = SS , res )
    colnames(res)[-1] <- c1
    cat("Univariate ANOVA for Multiply Imputed Data \n\n")
    cat("lm Formula: ", formula  )
    cat( paste( "\nR^2=" , round(r.squared , 6 ) , sep="") , "\n" )
    cat("..........................................................................\n")
    cat("ANOVA Table \n " )
    print( round( res ,5) )	
    invisible( list( "r.squared" = r.squared , "anova.table" = res ) )
    }
