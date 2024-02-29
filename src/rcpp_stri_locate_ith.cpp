
#include <Rcpp.h>
using namespace Rcpp;

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_convert_neg_i)]]
IntegerVector rcpp_convert_neg_i(IntegerVector n_matches, IntegerVector i) {
  
  int n = i.length();
  IntegerVector out(n);
  int res = 0;
  
  for(int j = 0; j < n; ++j) {
    res = n_matches[j] - abs(i[j] + 1);
    if(res < 1) {
      out[j] = 1;
    } else {
      out[j] = res;
    }
  }
  
  return out;
}



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_convert_pos_i)]]
IntegerVector rcpp_convert_pos_i(IntegerVector n_matches, IntegerVector i) {
  
  int n = i.length();
  IntegerVector out(n);
  
  for(int j = 0; j < n; ++j) {
    if(i[j] < n_matches[j]) {
      out[j] = i[j];
    } else {
      out[j] = n_matches[j];
    }
  }
  
  return out;
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_alloc_stri_locate_ith)]]
IntegerMatrix rcpp_alloc_stri_locate_ith(List p1, IntegerVector i) {
    int n = p1.length(); // using regular integer, because maximum nrow/ncol for matrices is approx 2^30 -1 anyway
    IntegerMatrix out(n, 2);
    for(int j = 0; j < n; ++j) {
      IntegerMatrix temp = p1[j];
      IntegerMatrix::Row temprow = temp( i[j], _ );
      IntegerMatrix::Row mainrow = out( j , _ );
      mainrow = temprow;
    }
    return  out;
}