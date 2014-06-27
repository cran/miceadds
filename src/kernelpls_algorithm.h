

#include <Rcpp.h>
#include <RcppArmadillo.h>

using namespace Rcpp;

// extern "C" {
//    Rcpp::List kernelplsaux(Rcpp::NumericMatrix Yr, Rcpp::NumericMatrix Xr, Rcpp::NumericVector nc)  ;
//      }

////////////////////////////////////
/// PLS function
            
Rcpp::List kernelplsaux(Rcpp::NumericMatrix Yr, Rcpp::NumericMatrix Xr, Rcpp::NumericVector nc){
    int nobj = Xr.nrow(); 
    int npred = Xr.ncol();
    int nresp = Yr.ncol() ;
    int ncomp = nc[0] ;

//    double eps_add = pow(10,-10) ;
    double eps_add = 1e-10 ;
    
    
    arma::mat X(Xr.begin(), nobj, npred, false);       // reuses memory and avoids extra copy
    arma::mat Y(Yr.begin(), nobj, nresp , false); 

    // define matrices
    //    ## Projection, loadings
    //    R <- P <- matrix(0, ncol = ncomp, nrow = npred)
    //    tQ <- matrix(0, ncol = nresp, nrow = ncomp)# Y loadings; transposed
    //    B <- array(0, c(npred, nresp, ncomp))

    arma::mat R = arma::zeros( npred , ncomp  ); 
    arma::mat P = arma::zeros( npred , ncomp  );
    arma::mat tQ = arma::zeros( ncomp , nresp  );
    arma::mat B = arma::zeros( npred , ncomp  );

    // auxiliary matrices ;
    arma::mat wa ;
    arma::mat ra ;
    arma::mat ua ;
    arma::mat pa ;
    arma::mat qa ;
    arma::mat ta ;
    arma::mat tmp1 ;
    arma::mat tmp2 = P ;
    arma::mat tmp3 ;
    arma::mat tsq ;
    arma::mat tsq1 ;
    arma::mat tmp11 ;
    arma::mat tmp12 ;

    //        W <- P                        # Loading weights
    //        U <- TT <- matrix(0, ncol = ncomp, nrow = nobj)# scores
    //        tsqs <- rep.int(1, ncomp)       # t't
    //        fitted <- matrix(0, c(nobj, ncomp))
    arma::mat W = P ;
    arma::mat U = arma::zeros( nobj , ncomp );
    arma::mat TT = arma::zeros( nobj , ncomp );	
    arma::mat tsqs = arma::ones( ncomp , 1 ) ;
    arma::mat fitted = arma::zeros( nobj , ncomp );	

    //    ## 1.
    //    XtY <- crossprod(X, Y)
    arma::mat XtY = arma::mat( trans(X) * Y ) ;

    // set component index ;
    // int aa=0 ;
    for (int aa=0;aa<ncomp;++aa){

    //    ## 2.
    //    w.a <- XtY / sqrt(c(crossprod(XtY)))
    wa = arma::mat( XtY / repmat( sqrt( trans(XtY) * XtY + eps_add ) ,
                npred,1) ) ;

    //        ## 3.
    //        r.a <- w.a
    //            for (j in 1:(a - 1))
    //                r.a <- r.a - (P[,j] %*% w.a) * R[,j]
    //        }

    ra = wa ;
    for (int jj=0 ; jj < aa ; ++jj ){
    //	int jj=0;  // include for loop here !!!!
        tmp11 = arma::mat( trans(P.col(jj))  * wa ) ;
        tmp12 = arma::repmat( tmp11 , npred , 1 ) ;
        ra = arma::mat( ra -  tmp12 % R.col(jj) ) ;
                }
            
        
    //        ## 4.
    //        t.a <- X %*% r.a
    //        tsq <- c(crossprod(t.a))
    //        p.a <- crossprod(X, t.a) / tsq
    //        q.a <- crossprod(XtY, r.a) / tsq

    ta = arma::mat( X * ra ) ;
    tsq = arma::mat( trans(ta) * ta ) ;
    tsq1 = arma::repmat( tsq , npred , 1 ) ;
    pa = arma::mat( ( trans(X) * ta ) / tsq1) ;
    qa = arma::mat( ( trans(XtY) * ra ) / tsq ) ;

    //     ## 5.
    //     XtY <- XtY - (tsq * p.a) %*% t(q.a)
    tmp2 = arma::repmat( tsq , npred , 1 ) ;
    XtY = arma::mat( XtY - ( tmp2 % pa ) * trans(qa) ) ;

    //     ## 6.-8.
    //     R[,a] <- r.a
    //     P[,a] <- p.a
    //     tQ[a,] <- q.a
    //     B[,a] <- R[,1:a, drop=FALSE] %*% tQ[1:a,, drop=FALSE]
    R.col(aa) = ra ;
    P.col(aa) = pa ;
    tQ.row(aa) = qa ;
    B.col(aa) = arma::mat( 
        R( arma::span(0,npred-1) , arma::span(0,aa) ) *
        tQ( arma::span(0,aa) , arma::span(0,0) )
                ) ; 

    // # !stripped
    // tsqs[a] <- tsq
    tsqs( aa , 0 ) = tsq(0,0) ;
    //     ## Extra step to calculate Y scores:
    //     u.a <- Y %*% q.a / c(crossprod(q.a)) # Ok for nresp == 1 ??
//    ua = arma::mat( ( Y * qa ) / 
//        arma::repmat( trans(qa) * qa  , nobj , 1 ) ) ;        
    // correction ARb 2013-11-12    
    ua = arma::mat( ( Y * qa ) / 
        arma::repmat( trans(qa) * qa  + eps_add , nobj , 1 ) ) ;        
    //     ## make u orth to previous X scores:
    //     if (a > 1) u.a <- u.a - TT %*% (crossprod(TT, u.a) / tsqs)
    if (aa > 0 ){
       ua = ua - arma::mat(  TT * ( ( trans(TT) * ua ) / ( tsqs + eps_add ) ) );	
            }
    //     U[,a] <- u.a
    U.col(aa) = ua ;
    //     TT[,a] <- t.a
    TT.col(aa) = ta ;
    //     W[,a] <- w.a
    W.col(aa) = wa ;
    //    fitted[,a] <- TT[,1:a] %*% tQ[1:a,, drop=FALSE]
    fitted.col(aa) = arma::mat(
        TT( arma::span(0,nobj-1) , arma::span(0,aa) ) *
        tQ( arma::span(0,aa) , arma::span(0,0) )
                ) ;
        }  // end loop components
    //*******************

    ////////////////////////////////////
    // OUTPUT:
    return Rcpp::List::create(
           Rcpp::Named("Y_used") = Y,
           Rcpp::Named("X_used") = X ,
           _["R"] = R , _["P"] = P , _["tQ"] = tQ ,
           _["B"]=B , _["XtY"] = XtY ,
           _["wa"] = wa , _["ra"]=ra , _["ta"]=ta ,
           _["tsq"] = tsq , _["pa"]=pa , _["qa"]=qa ,
           _["W"]=W , _["U"]=U , _["TT"]=TT ,
           _["tsqs"]=tsqs , _["fitted"]=fitted 
                ) ;

}


