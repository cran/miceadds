mice.impute.2l.latentgroupmean.ML <- function (y, ry, x, type , 
                    pls.facs = NULL , imputationWeights = NULL ,
                    interactions = NULL , quadratics = NULL , 
					EAP = FALSE , ...){  
    # latent group mean
    cluster <- as.numeric( x[ , type == - 2] )
    covariates <- as.matrix( x[ , type == 1 ] )
    base::colnames(covariates) <- base::colnames(x)[ type == 1 ]
    y <- x[ , type == 2 ]
    # distinguish cases with and without covariates
    if ( base::sum( type == 1 ) > 0){ 
        cov2 <- base::cbind( cluster , covariates )
        # aggregate covariates
        covaggr <- mice.impute.2l.groupmean( y , ry , x = cov2 , 
						type = base::c(-2 , base::rep(1, base::ncol(covariates) ) ) ,
						grmeanwarning=FALSE )
        base::colnames(covaggr) <- base::colnames(covariates)
        # aggregation at level 2
        covaggr.l2 <- stats::aggregate( covaggr , base::list( cluster ) , base::mean )
        base::colnames(covaggr.l2)[-1] <- base::colnames(covaggr)
        y.l2 <- stats::aggregate( y , base::list( cluster ) , base::mean , na.rm=TRUE )
        h1 <- as.matrix(covaggr.l2[,-1] )
        base::colnames(h1) <- base::colnames(covaggr)
        # PLS interactions and quadratics
		res <- mice_imputation_get_states(pos = base::parent.frame(n=1) )	
		vname <- res$vname
		newstate <- res$newstate					
        plsout <- mice_imputation_pls_helper( newstate = newstate , vname = vname , 
						pls.impMethod = "xplsfacs" , 
                        x = h1 , y = y.l2[,2] , ry= ( ! is.na(y.l2[,2] )) , 
                        imputationWeights = rep( 1 , nrow(covaggr.l2)) , 
                        interactions = interactions, quadratics = quadratics ,  
						pls.facs = pls.facs ,  ... )
        # imputation PLS
        if( ! base::is.null( plsout$yimp  ) ){ 
            covaggr.l2r <- base::as.matrix(plsout$yimp[,-1])
            covaggr <- base::as.matrix( covaggr.l2r[ base::match(cluster,covaggr.l2[,1]),])
        }		
        mod <- lme4::lmer( y ~ as.matrix( covaggr  ) + ( 1 | cluster ) )
    } else {
        mod <- lme4::lmer( y ~ ( 1 | cluster ) )
    }							
    modr <- lme4::ranef( mod , condVar = TRUE )
    modr <- modr$cluster	
	modf <- mod@resp$mu		
    # extract cluster indices
    a1 <- stats::aggregate( modf , base::list( cluster) , base::mean )
    a1[,3] <-  base::sqrt( base::attr( modr , "postVar" )[1,1,] )
    # match cluster indices
    ind <- base::match( cluster , a1[,1] )
    ximp <- stats::rnorm( nrow(a1) , mean = a1[,2] ,  sd = (1-EAP)*a1[,3] )[ ind ]
    base::return(ximp)
}
