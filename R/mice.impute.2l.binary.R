mice.impute.2l.binary <- function(y, ry, x, type, intercept=TRUE,
                                  groupcenter.slope=FALSE, draw.fixed=TRUE,
                                  random.effects.shrinkage=10^-6,
                                  glmer.warnings=TRUE,
                                  ...){

  # *** ...............................
  # preliminary calculations

  clus <- x[,type==-2]
  clus2 <- unique(clus)
  ngr <- length(clus2)

  # add groupmeans in the regression model
  if ( any( type %in% c(3,4) ) ){ 
    x0 <- as.matrix(cbind( x[,type==-2], x[,type %in% c(3,4)] ))
    colnames(x0) <- c( colnames(x)[type==-2], colnames(x)[type %in% c(3,4)] )
    type0 <- c( -2, rep(1,ncol(x0)-1) )
    x0.aggr <- as.matrix(.mice.impute.2l.groupmean(y=y, ry=ry, x=x0, type=type0,
                                                   grmeanwarning=FALSE, ...))
    colnames(x0.aggr) <- paste0("M._", colnames(x0)[-1])
    # group mean centering
    if ( groupcenter.slope ){ 
      x0.aggr1 <- as.matrix(x0.aggr)
      colnames(x0.aggr1) <- colnames(x0)[-1]
      x0cent <- x0[,-1] - x0.aggr1
      x[ , colnames(x0cent) ] <- x0cent
    }
    # combine covariate matrix
    x <- cbind( x , x0.aggr )
    # add type
    type1 <- c( type , rep(1 , ncol(x0.aggr) ) )
    names(type1) <- c( names(type) , colnames(x0.aggr) )   
    type1[ type1 == 3 ] <- 1
    type1[ type1 == 4 ] <- 2
    type <- type1
  }	

  rhs.f <- paste0( c(ifelse(!intercept,0,1), colnames(x)[type %in% c(1,2)]), collapse="+" )
  rhs.r <- paste0( c(1, colnames(x)[type==2]), collapse="+" )
  fml <- paste0( "dv._lmer~", rhs.f, "+(", rhs.r,"|", colnames(x)[type==-2],")" )

  # fit based on observed y
  y1 <- y
  y1[!ry] <- NA
  if(glmer.warnings){
    fit <- lme4::glmer(fml, data=data.frame(dv._lmer=y1, x), na.action="na.omit",
                       family= stats::binomial(link="logit"))
  }else{
    suppressWarnings(
      fit <- lme4::glmer(fml, data=data.frame(dv._lmer=y1, x), na.action="na.omit",
                         family= stats::binomial(link="logit"))
    )
  }

  # x for prediction
  x0 <- as.matrix( x[!ry,type>=1,drop=F] )
  if(intercept) x0 <- cbind(1,x0)
  clus0 <- clus[!ry]

  # *** ...............................
  # Imputation from random coefficients logitistic regression
  #
  # Written by Sabine Zinn, November 2013
  # Material from Tom A.B. Snijders, August 2012; Alexander Robitzsch, July 2012
  # Corrected and modified by Simon Grund, March 2016

  # draw fixed effects
  if(draw.fixed){                             # posterior draw for fixed effects
    b <- lme4::fixef(fit)
    b.star <- b + as.vector( t(base::chol(vcov(fit))) %*% rnorm(length(b)) )
  }else{
    b.star <- lme4::fixef(fit)                # skip drawing fixed effects
  }
  ffit <- x0 %*% b.star                       # pred. based on fixed effects
  fl <- lme4::getME(fit, "flist")[[1]]
  ind <- match(clus0, clus2)
  ind2 <- match(unique(fl), clus2)
  vu <- lme4::VarCorr(fit)[[1]][,,drop=F]     # random effects (co-)variance
  re0 <- lme4::ranef(fit, condVar=TRUE)[[1]]
  q <- ncol(re0)
  re <- matrix(0,ngr,q)                       # re: 0 if fully unobserved
  re[ind2,] <- as.matrix(re0)                 # re: EAP if partially observed
  pv0 <- attr(re0, "postVar")
  pv <- array(0, dim=c(q,q,ngr))
  pv[,,ind2] <- pv0                # pv: post. variance if partially observed
  pv[,,-ind2] <- vu                # pv: random effects cov. if fully unobserved
  # draw random effects
  u <- matrix(0, ngr, q)
  if(q==1){
    u[,1] <- rnorm(ngr, mean=re[,1], sd=sqrt(pv[1,1,]))
  }else{
    for(i in 1:ngr){
      u[i,] <- re[i,] +
        as.vector( t(base::chol(pv[,,i] + diag(random.effects.shrinkage,q)))
                   %*% rnorm(q) )
    }
  }
  # generate imputations
  prob <- 1/(1+exp( -(ffit + rowMeans(u)[ind]) ))    # prob: expected probabilities
  rn.unif <- runif(sum(!ry),0,1)
  imp <- as.numeric(rn.unif < prob)                  # imp: imputed value

  return(imp)
}



.mice.impute.2l.groupmean <- function (y, ry, x, type , grmeanwarning=TRUE, ...){  

  # Written by Alexander Robitzsch, July 2012

  if ( ( ncol(x) > 2 ) & grmeanwarning )   warning("\nMore than one variable is requested to be aggregated.\n")    
  # calculate aggregated values
  a1 <- aggregate( x[, type %in% c(1,2) ] , list( x[,type == -2] ) , mean , na.rm=T)
  i1 <- match( x[,type == -2] , a1[,1] )
  ximp <- as.matrix(a1[i1,-1])
  colnames(ximp) <- paste( names(type)[ type %in% c(1,2) ] , names(type)[ type == -2 ] , sep="." )

  return(ximp)

}
