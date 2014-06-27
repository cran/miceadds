
########################################################
# weighted mean
ma.wtd.meanNA <- function( data , weights = rep(1,nrow(data) ) ){
    data1 <- data
    dataResp <- 1 - is.na( data )
    data1[ is.na(data1) ] <- 0
    data1 <- as.matrix( data1 )        
    # calculate means
    sumweight <- colSums( dataResp * weights )
    M_vars <- colSums( data1 *  weights ) / sumweight
    return( M_vars )
        }
#########################################################



###############################################################################
# weighted SD
ma.wtd.sdNA <- function( data , weights = rep(1,nrow(data) ) ){
    data1 <- data
    dataResp <- 1 - is.na( data )
    data1[ is.na(data1) ] <- 0
    data1 <- as.matrix( data1 )        
    # calculate means
    sumweight <- colSums( dataResp * weights )
    M_vars <- colSums( data1 *  weights ) / sumweight
    M_varsM <- matrix( M_vars , nrow= nrow(data1) , ncol=length(M_vars ) , byrow=TRUE )
    data1adj <- ( data1 - M_varsM ) * dataResp # take care of missings    
	sdx <- sqrt( colSums( data1adj^2 * weights ) /  colSums( dataResp * weights ) )
    return( sdx )
        }
###############################################################################




###############################################################################
# weighted covariance
ma.wtd.covNA <- function( data , weights = rep(1,nrow(data) ) ){
    data1 <- data
    dataResp <- 1 - is.na( data )
    data1[ is.na(data1) ] <- 0
    data1 <- as.matrix( data1 )        
    # calculate means
    sumweight <- colSums( dataResp * weights )
    M_vars <- colSums( data1 *  weights ) / sumweight
    M_varsM <- matrix( M_vars , nrow= nrow(data1) , ncol=length(M_vars ) , byrow=TRUE )
    data1adj <- ( data1 - M_varsM ) * dataResp # take care of missings    
    sqrtweights <- sqrt( weights )
    # calculate weighted covariance
    # cross-products
    covXY <- crossprod( data1adj * sqrtweights )
    covWXY <- crossprod( dataResp * sqrtweights )
    covXY <- covXY / covWXY
    return( covXY )
        }
###############################################################################

# calculate cov2cor here
ma.wtd.corNA <- function( data , weights = rep(1,nrow(data) ) ){
	cov2cor( ma.wtd.covNA( data=data , weights=weights ) )
			}