

#include <Rcpp.h>

using namespace Rcpp;






  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_ntt_smaller_dbl_00)]]
LogicalVector rcpp_ntt_smaller_dbl_00(
    NumericVector x, NumericVector y, double tol, bool equal
  ) {
    R_xlen_t n = x.length();
    LogicalVector out(n);
    if(equal) {
      for(R_xlen_t i = 0; i < n; ++i) {
        out[i] = (fabs(x[i] - y[i]) < tol) || x[i] <= y[i];
      }
    } else {
      for(R_xlen_t i = 0; i < n; ++i) {
        out[i] = (y[i] - x[i]) >= tol;
      }
    }
    
    return out;
  }





  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_ntt_smaller_dbl_01)]]
LogicalVector rcpp_ntt_smaller_dbl_01(
    NumericVector x, double y, double tol, bool equal
  ) {
    R_xlen_t n = x.length();
    LogicalVector out(n);
    if(equal) {
      for(R_xlen_t i = 0; i < n; ++i) {
        out[i] = (fabs(x[i] - y) < tol) || x[i] <= y;
      }
    } else {
      for(R_xlen_t i = 0; i < n; ++i) {
        out[i] = (y - x[i]) >= tol;
      }
    }
    
    return out;
  }





  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_ntt_smaller_dbl_10)]]
LogicalVector rcpp_ntt_smaller_dbl_10(
    double x, NumericVector y, double tol, bool equal
  ) {
    R_xlen_t n = y.length();
    LogicalVector out(n);
    if(equal) {
      for(R_xlen_t i = 0; i < n; ++i) {
        out[i] = (fabs(x - y[i]) < tol) || x <= y[i];
      }
    } else {
      for(R_xlen_t i = 0; i < n; ++i) {
        out[i] = (y[i] - x) >= tol;
      }
    }
    
    return out;
  }


