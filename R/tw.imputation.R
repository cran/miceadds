tw.imputation <- function( data , integer = FALSE ){
        n <- nrow(data)
        p <- ncol(data)        
        # person mean      
        pm <- rowMeans( data , na.rm = T )
        # item mean
        im <- colMeans( data , na.rm = T )
        # overall mean
        om <- mean( as.matrix(data) , na.rm = T)
        # two-way imputation matrix
        tw <- outer( pm , rep( 1 , p ) )  +
                   outer( rep( 1 , n ) , im ) - om
        # polytomous data (we assume that all variables have the same scales)
        m1 <- min(na.omit(data)); m2 <- max(na.omit(data))
        tw[ tw < m1 ] <- m1
        tw[ tw > m2 ] <- m2
        tw.raw <- data
        tw.raw[ is.na(data)  ] <- tw[ is.na(data) ]
		if (integer ){ 
			gt <- tw.raw - floor(tw.raw)
			tw1 <- matrix( rbinom(  as.matrix(gt) , 1 , prob = as.matrix(gt) ) , ncol = p , byrow=F)
			tw.item <- floor(tw.raw) +  tw1	
			  } else { tw.item <- tw.raw }
        return( tw.item )
        }
