
// includes from the plugin
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include "kernelpls_algorithm.h"


#ifndef BEGIN_RCPP
#define BEGIN_RCPP
#endif

#ifndef END_RCPP
#define END_RCPP
#endif

using namespace Rcpp;


// user includes
// #include <P:\Eigene_Projekte\R-Routinen\IRT-Functions\miceadds_Package\Entwicklung\PLS\Code\kpls_02.cpp>

// declarations
extern "C" {
   SEXP kernelpls_1dim_C( SEXP Y, SEXP X, SEXP comp) ;
}

// definition

SEXP kernelpls_1dim_C( SEXP Y, SEXP X, SEXP comp ){
BEGIN_RCPP
  
       
     Rcpp::NumericMatrix Yr(Y);          
     Rcpp::NumericMatrix Xr(X);          
     Rcpp::NumericVector nc(comp) ;  
     // include double here  
       
     // run PLS algorithm 'kernelpls_aux'  
     Rcpp::List res = kernelplsaux(Yr , Xr,  nc)  ;  
       
       
     return( wrap(res) ) ;
END_RCPP
}




