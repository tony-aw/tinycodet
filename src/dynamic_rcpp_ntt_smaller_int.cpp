

#include <Rcpp.h>

using namespace Rcpp;






  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_ntt_smaller_int_00)]]
LogicalVector rcpp_ntt_smaller_int_00(
    IntegerVector x, IntegerVector y, bool equal
  ) {
    R_xlen_t n = x.length();
    LogicalVector out(n);
    if(equal) {
      for(R_xlen_t i = 0; i < n; ++i) {
        out[i] = x[i] <= y[i];
      }
    } else {
      for(R_xlen_t i = 0; i < n; ++i) {
        out[i] = x[i] < y[i];
      }
    }
    
    return out;
  }





  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_ntt_smaller_int_01)]]
LogicalVector rcpp_ntt_smaller_int_01(
    IntegerVector x, int y, bool equal
  ) {
    R_xlen_t n = x.length();
    LogicalVector out(n);
    if(equal) {
      for(R_xlen_t i = 0; i < n; ++i) {
        out[i] = x[i] <= y;
      }
    } else {
      for(R_xlen_t i = 0; i < n; ++i) {
        out[i] = x[i] < y;
      }
    }
    
    return out;
  }





  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_ntt_smaller_int_10)]]
LogicalVector rcpp_ntt_smaller_int_10(
    int x, IntegerVector y, bool equal
  ) {
    R_xlen_t n = y.length();
    LogicalVector out(n);
    if(equal) {
      for(R_xlen_t i = 0; i < n; ++i) {
        out[i] = x <= y[i];
      }
    } else {
      for(R_xlen_t i = 0; i < n; ++i) {
        out[i] = x < y[i];
      }
    }
    
    return out;
  }


