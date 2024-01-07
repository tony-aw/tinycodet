#include <Rcpp.h>

using namespace Rcpp;

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_any_nonpos)]]
bool rcpp_any_nonpos(IntegerVector indx) {
  R_xlen_t n = indx.length();
  for(R_xlen_t i = 0; i < n; ++i) {
    if(indx[i] < 1) return true;
  }
  return false;
}

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_any_neg)]]
bool rcpp_any_neg(IntegerVector indx) {
  R_xlen_t n = indx.length();
  for(R_xlen_t i = 0; i < n; ++i) {
    if(indx[i] < 0) return true;
  }
  return false;
}