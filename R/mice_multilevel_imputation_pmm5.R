mice_multilevel_imputation_pmm5 <- function (y, ry, x, yhatobs ,
		yhatmis , donors=3 , noise = 1E5 , ...){	
	N1 <- base::length(yhatobs)
	N0 <- base::length(yhatmis)
	N <- N0 + N1	
	GG <- 1000* base::max( base::abs(yhatobs) , base::abs(yhatmis) )
    dfr <- base::cbind( 1 , 1:N1 , yhatobs , y[ry] )
    dfr0 <- base::cbind( 0 , 1:N0 , yhatmis , NA)  
    dfr <- base::rbind( dfr , dfr0 )	
    base::colnames(dfr) <- base::c("obs" , "index_obs_miss" , "yhat" , "y") 
    # add some small noise to create unique entries in matrix d0
    d00 <- base::abs( base::diff(dfr[,"yhat"]) )
    fg1 <- base::min( d00[ d00 > 0 ] )    
    dfr[,"yhat"] <- dfr[,"yhat"] + stats::runif( N , 0 , fg1 / noise )
    dfr <- base::data.frame(dfr[ base::order(dfr[,3] ) , ])             
    dfr$sortindex <- 1:N
	dfr$obsindex_low <- base::cumsum( dfr$obs )
    ind <- base::seq( N , 1 , -1 )
    Ny <- base::sum( ry)
    N0 <- base::sum( ! ry )
    c1 <- Ny - base::cumsum( dfr$obs[ ind ] )   + 1    
    dfr$obsindex_upp <- c1[ ind ]	
	vy <- base::c(1,Ny)
	dfr$obsindex_low <- mice::squeeze( dfr$obsindex_low , vy) 
	dfr$obsindex_upp <- mice::squeeze( dfr$obsindex_upp , vy) 	
    dfr0 <- dfr[ dfr$obs == 0 , ]
    dfr1 <- dfr[ dfr$obs == 1 , ]	
    # create matrix for sampling
    ydonors <- matrix( NA , nrow=nrow(dfr0) , ncol=2*donors )
	dfr0 <- dfr0[ order(dfr0$index_obs_miss) , ]
    for ( dd in 1:donors){
        ydonors[,dd] <- dfr1[ mice::squeeze( dfr0$obsindex_low - dd + 1 ,c(1,Ny) ) , "y"]
        ydonors[,dd+donors] <- dfr1[ mice::squeeze( dfr0$obsindex_upp + dd - 1 ,c(1,Ny) ) , "y"]    
                        }
    ind.sample <- sample( 1:(2*donors) , N0 , replace = TRUE )
    imp <- ydonors[ cbind( 1:N0 , ind.sample) ]
    base::return(imp)
}
