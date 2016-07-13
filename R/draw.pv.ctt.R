draw.pv.ctt <- function( y , dat.scale = NULL , x=NULL , samp.pars = TRUE , alpha = NULL ,
                            sig.e = NULL , var.e=NULL , true.var = NULL ){ 
    #---------------------------------------------------------------------------##
    # INPUT:                                                                    ##
    # y         ...  vector of scale scores                                     ##
	#           should missings be allowed or not?								##
    # dat.scale ...  data frame with items correspond. to scale for             ##
    #                   calculating Cronbach's Alpha                            ##
    # x         ...  data frame containing all covariates                       ##
    # samp.pars ...  sampling of regression parameters (Default=TRUE; if = FALSE##
    #                   then randomness is only due to person sampling)         ##
    # alpha     ...  estimate of Cronbach's alpha which is used                 ##
    #                   in the PV imputation model                              ##
    # sig.e     ...  measurement error of scale scores (can be a number or a    ##
    #                       vector)                                             ##
    # true.var  ...  variance of latent trait (this should not be NULL if       ##
    #                       sig.e is specified)                                 ##
    #----------------------------------------------------------------------------#
    # calculate Cronbach's Alpha if alpha == NULL
	if ( ! base::is.null(var.e) ){ 
		sig.e <- base::sqrt( var.e )
	}	
    if (  base::is.null(alpha) & base::is.null(sig.e)  ){ 
        alpha <- .cronbach.alpha( dat.scale )
    }
	#******
    # calculate scale scores if not provided
	if ( ( ! base::is.null( dat.scale) ) & ( base::is.null(y) ) ){
			y <- base::rowMeans( dat.scale , na.rm=TRUE )
	}
	#*********
    y0 <- y
	if ( ! base::is.null(x) ){
	    x0ind <- TRUE
		x0 <- base::scale( x , scale=FALSE )
		mod <- stats::lm( y0 ~ as.matrix(x0) )
	} else { 
		mod <- stats::lm( y0 ~ 1 ) 
		x0ind <- FALSE
	}				
    # determine fitted y (regression model)
    yfitted <- mod$fitted
    smod <- summary(mod)
    # calculate true variance of scale
    if ( base::is.null(sig.e) ){   
		var.ytrue <- stats::var(y,na.rm=TRUE) * alpha 
	}
    if ( ( ! base::is.null(sig.e) ) & ( ! base::is.null( true.var ) ) ){ 
		var.ytrue <- true.var 
	}
    if ( ( ! base::is.null(sig.e) ) & ( base::is.null( true.var ) ) ){ 
        var.ytrue <- stats::var(y , na.rm=TRUE ) - base::mean( sig.e^2 , na.rm=TRUE) 				
		}
    # calculate residual variance in the regression model
    sig2.th.y1 <-  var.ytrue * ( 1 - smod$r.squared )
	vary <- stats::var(y,na.rm=TRUE)
    sig2.th.y <-  vary * ( 1 - smod$r.squared )


	# correction ARb 2013-11-04
	# define alpha if it is not already defined
	if ( base::is.null(alpha) ){  
		alpha <- 1 - base::mean( sig.e^2 , na.rm=TRUE) / vary  
	}	
	sig2.th.y <- base::max( 1E-5 , sig2.th.y - vary*(1-alpha) )
	
    # draw new residual variance
    if ( samp.pars ){ 
        N <- base::length(y)
        sig2.th.y <- N * sig2.th.y / stats::rchisq(1, N )
    }
	# calculate measurement error variance
    if (is.null(sig.e) ){ 
		sig2.e <- stats::var(y) * ( 1 - alpha )
    } else {   
		sig2.e <- sig.e^2
		sig2.e <- base::ifelse( base::is.na(sig2.e) , 
					1000 * base::mean( sig2.e , na.rm=TRUE) , sig2.e )
	}							
    # calculate conditional reliability
    rho.c <- sig2.th.y / ( sig2.th.y + sig2.e ) 
    # draw regression parameter
    if ( samp.pars ){ 
        v <- stats::vcov(mod)	
		m <- base::rep(0, base::nrow(v) )
		rn <- MASS::mvrnorm( 1 , mu = m , Sigma = v )
        beta.star <- stats::coef(mod) + rn				
        # draw new fitted y
		if ( x0ind ){
			yfitted <- base::cbind(1,x0) %*% beta.star
		} else {
			yfitted <- beta.star											
		}
    }
    #................................
    # draw plausible values
    # expected values
    theta.bar <- rho.c * y0 + ( 1 - rho.c ) * yfitted
    # add random noise
    sd.pv <- base::sqrt(  ( 1 - rho.c ) * sig2.th.y )
    y.pv <- theta.bar + stats::rnorm( base::length(y) , mean=0 , sd = sd.pv )
    y.pv <- base::as.vector( y.pv )
	#****************
	# print
	base::cat("\nPlausible Value Imputation\n\n")
	h1 <- base::round( stats::var(y0, na.rm=TRUE) , 4 )
	h2 <- base::round( stats::var(y.pv, na.rm=TRUE) , 4 )
	base::cat( "Observed variance:" , h1 , "\n")
	base::cat( "Sampled PV variance:" , h2 , "\n")
	base::cat( "PV Reliability:" , base::round(h2 / h1 ,4  ) , "\n")	
	base::cat( "Conditional Reliability of Y given X:" , 
			base::round( base::mean(rho.c , na.rm=TRUE) ,4  ) , "\n")
    base::return( y.pv )
}
###################################################################
# define options where y can have missing values
# take care that y0 is then defined only on observed responses
###################################################################
	
#############################################
#############################################
# unstandardized estimate of Cronbach's alpha
.cronbach.alpha <- function( dat.scale ){
    I <- base::ncol( dat.scale )
    var.scale <- stats::var( dat.scale , use="pairwise.complete.obs" )
    v.bar <- base::mean( base::diag( var.scale )  )
    c.bar <- base::mean( var.scale[ upper.tri( var.scale ) ] )
    alpha <- ( I * c.bar ) / ( v.bar + (I-1) * c.bar )
    base::return(alpha)
}
#############################################
