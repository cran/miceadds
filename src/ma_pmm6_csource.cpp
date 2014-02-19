

// includes from the plugin
#include <RcppArmadillo.h>
#include <Rcpp.h>


#ifndef BEGIN_RCPP
#define BEGIN_RCPP
#endif

#ifndef END_RCPP
#define END_RCPP
#endif

using namespace Rcpp;


// user includes


// declarations
extern "C" {
SEXP ma_pmm6_C( SEXP y_, SEXP ry01_, SEXP x_, SEXP ridge_, SEXP coefu_, SEXP donorsample_) ;
}

// definition

SEXP ma_pmm6_C( SEXP y_, SEXP ry01_, SEXP x_, SEXP ridge_, SEXP coefu_, SEXP donorsample_ ){
BEGIN_RCPP
  
       
     Rcpp::NumericVector y(y_); // varlist only one variable  
     Rcpp::NumericVector ry01(ry01_); // varlist only one variable  
     Rcpp::NumericMatrix x(x_);  
       
     int ridge = as<double>(ridge_);   // donors  
     Rcpp::NumericVector coefu1(coefu_); // sampling regression coefficients  
     Rcpp::NumericVector donorsample(donorsample_); // sampling regression coefficients  
       
     //********************************  
     // fit linear model  
     arma::colvec ry01A(ry01.begin(), ry01.size(), false);   
     arma::uvec ind_obs=arma::find(ry01A==1);  
     // arma::uvec ind_miss=arma::find(ry01A==0);  
     int n = x.nrow(), k = x.ncol();  
     int nobs = ind_obs.size() ;  
     int nmiss = n - nobs ;  
     arma::mat xA(x.begin(), n, k, false);    
     arma::mat xobs=xA.rows(ind_obs) ;  
     arma::colvec yA(y.begin(), y.size(), false);   
     arma::colvec yobs = yA.rows(ind_obs) ;  
     // arma::colvec coef = arma::solve(xobs, yobs);        
     arma::mat xtx = arma::mat( arma::trans(xobs) * xobs ) ;  
     for (int ii=0;ii<k;ii++){ xtx(ii,ii)=xtx(ii,ii)+ridge ; }  
     arma::mat xinv = arma::inv( xtx ) ;  
     arma::mat coef2 = arma::mat( xinv * arma::trans(xobs) * yobs ) ;  
     arma::colvec resid = arma::mat( yobs - xobs*coef2 ) ;   
     double sig2 = arma::as_scalar( arma::trans(resid)*resid/(n-k) );  
     // sample new coefficient coef  
     arma::mat vcoef = arma::mat( sig2 * xinv ) ;  
     arma::colvec coefu(coefu1.begin(), coefu1.size(), false);   
     arma::mat coef = arma::mat( coef2 + chol(vcoef) * coefu ) ;  
       
       
     //*********************************  
     // prediction and matrix arrangement  
       
     Rcpp::NumericVector yimp_ind(nmiss) ;  
     Rcpp::NumericVector yimp(nmiss) ;  
     Rcpp::NumericVector yimp_donors(nobs) ;  
       
     double t1=0;  
     double t2=0;  
     arma::mat ypred_mis = arma::mat( xA * coef ) ;  
     Rcpp::NumericMatrix YSM1(n,5);  
     arma::mat YSM(YSM1.begin(), n, 5, false);    
     for (int nn=0;nn<n; nn++){  
         YSM(nn,0) = ypred_mis(nn,0) ;  
         YSM(nn,1) = ry01[nn] ;  
         t1 ++ ;  
         if ( ry01[nn] == 1 ){  
             YSM(nn,2) = t1 ;  
             YSM(nn,4) = y[nn] ;  
                             }  
         if ( ry01[nn] == 0 ){  
             t2 ++ ;  
             YSM(nn,3) = t2 ;  
                             }                          
                     }  
       
     //********************************************  
     // sorting  
     // sort according to variable  
     arma::colvec Mvv = YSM.col(0) ;                
     arma::uvec indvv = arma::sort_index( Mvv ) ;  
     arma::mat YSM_sort=YSM.rows(indvv) ;  
     t1=0;         
     int zz=0;   
     double g1 =0;                  
     for (int nn=0;nn<n; nn++){  
         if ( YSM_sort(nn,1) == 1 ){  
             t1 ++ ;          
//             YSM_sort(nn,5) = t1 ;  
             yimp_donors[ t1-1 ] = YSM_sort(nn,4) ;  
                             }  
         if ( YSM_sort(nn,1) == 0 ){  
//             YSM_sort(nn,6) = t1 ;  
             g1 = t1 + donorsample[zz] ;  
             if ( g1 <= 0 ){ g1 = 1 ; }  
             if (g1 > nobs){ g1 = nobs ; }          
//             YSM_sort(nn,7) = g1 ;  
             zz ++ ;  
             yimp_ind[ YSM_sort(nn,3) - 1 ] = g1 ;  
                             }                          
                     }                  
       
     // allocation  
     for (int zz=0;zz<nmiss;zz++){  
         yimp[zz] = yimp_donors[ yimp_ind[zz] - 1 ] ;  
     }  
       
     return( wrap( yimp) ) ;  
       
         ////////////////////////////////////  
         // OUTPUT:  
     //    return Rcpp::List::create(  
     //           Rcpp::Named("donors") = donors ,  
     //           Rcpp::Named("x") = x  ,  
     //           _["xA"] = xA  ,         _["xobs"] = xobs ,  
     //          _["yobs"] = yobs ,        _["ind_obs"]=ind_obs ,   
     // _["yimp_donors"]=yimp_donors                  
     //                ) ;
END_RCPP
}



