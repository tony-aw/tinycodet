#include <Rcpp.h>

using namespace Rcpp;

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_any_badloc)]]
bool rcpp_any_badloc(IntegerVector lower, IntegerVector upper) {
  R_xlen_t n = lower.length();
  for(R_xlen_t i = 0; i < n; ++i) {
    if(upper[i] < lower[i]) return true;
  }
  return false;
}